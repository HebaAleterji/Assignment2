#' @title Compute Hadi's Influence Measure
#' @description Computes Hadi's Influence Measure for an lm model.
#' @param model An object of class lm.
#' @return A numeric vector of Hadi's influence measures.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' hadis_influence_measure(model)
hadis_influence_measure <- function(model) {
  if (!inherits(model, "lm")) {
    stop("The model should be an object of class 'lm'")
  }

  e_i <- residuals(model)
  SSE <- sum(e_i^2)
  p <- length(coef(model))
  H <- hatvalues(model)
  n <- length(e_i)

  d_i <- e_i / sqrt(SSE / (n - p))

  H_i <- (H / (1 - H)) + ((p + 1) / (1 - H)) * (d_i^2 / (1 - d_i^2))

  return(H_i)
}



#' @title Compute Cook's Distance from Scratch
#' @description Computes Cook's Distance for an lm model from scratch.
#' @param model An object of class lm.
#' @return A numeric vector of Cook's distances.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' cooks_distance_scratch(model)
cooks_distance_scratch <- function(model) {
  h <- hatvalues(model)
  e <- residuals(model)
  s2 <- sum(e^2) / df.residual(model)
  p <- length(coef(model))
  (e^2 / (p * s2)) * (h / (1 - h)^2)
}

#' @title Compute DFFITS from Scratch
#' @description Computes DFFITS for an lm model from scratch.
#' @param model An object of class lm.
#' @return A numeric vector of DFFITS values.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' dffits_scratch(model)
dffits_scratch <- function(model) {
  h <- hatvalues(model)
  e <- residuals(model)
  s2 <- sum(e^2) / df.residual(model)
  dffits <- e * sqrt(h / (1 - h)) / sqrt(s2)
  dffits
}


#' @title Perform Influence Diagnostics
#' @description Computes selected influence measures for an lm model.
#' @param data The dataset used in the model.
#' @param model An object of class lm.
#' @param measure The influence measure to compute ("cooks", "dffits", "hadi", or "all").
#' @return A list containing the selected influence measures.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' influence_diagnostics(mtcars, model, "all")
influence_diagnostics <- function(data, model,  measure = c("all", "cooks", "dffits", "hadi")) {

  # Subset the data to include only the predictors used in the model
  data_subset <- subset_data(data, model)

  validate_inputs(data_subset, model)
  measure <- match.arg(measure)

  results <- list(cooks = NULL, dffits = NULL, hadi = NULL)

  if (measure == "all" || measure == "cooks") {
    results$cooks <-  cooks_distance_scratch(model)
  }
  if (measure == "all" || measure == "dffits") {
    results$dffits <- dffits_scratch(model)
  }
  if (measure == "all" || measure == "hadi") {
    results$hadi <- hadis_influence_measure(model)

  }

  if (length(results) == 0) {
    stop("Invalid measure specified")
  }

  plot_influence(results)

}


#' @title Plot Influence Measures
#' @description Plots the provided influence measure results for a model.
#' @param diagnostics The result from influence_diagnostics.
#' @return NULL.
#' @examples
#' diagnostics <- influence_diagnostics(mtcars, model, "all")
#' plot_influence(diagnostics)
plot_influence <- function(diagnostics) {
  max_length <- max(sapply(diagnostics, length))

  plot(1:max_length, diagnostics$cooks, type = "h", col = "blue", ylim = range(unlist(diagnostics)),
       main = "Influence Measures", ylab = "Influence", xlab = "Index")

  if (!is.null(diagnostics$dffits)) {
    lines(1:max_length, diagnostics$dffits, type = "h", col = "red")
  }

  if (!is.null(diagnostics$hadi)) {
    lines(1:max_length, diagnostics$hadi, type = "h", col = "green")
  }

  legend("topright", legend = c("Cook's Distance", "DFFITS", "Hadi's Influence Measure"),
         col = c("blue", "red", "green"), lty = 1)
}

subset_data <- function(data, model) {
  model_vars <- all.vars(formula(model))
  data_subset <- data[, model_vars, drop = FALSE]
  return(data_subset)
}


#' @title Validate Inputs
#' @description Validates the inputs for the influence diagnostics functions.
#' @param data The dataset used in the model.
#' @param model An object of class lm.
#' @return NULL. Stops execution if validation fails.
validate_inputs <- function(data, model) {
  stopifnot(is.data.frame(data))
  stopifnot(inherits(model, "lm"))

  if (anyNA(data)) stop("Data contains NA values")

  if (any(unlist(lapply(data, function(col) any(is.infinite(col)))))) {
    stop("Data contains infinite values")
  }

  # Extract the terms from the model
  terms <- attr(model$terms, "term.labels")
  response <- attr(model$terms, "response")
  predictors <- length(terms)

  if (ncol(data) != predictors + 1) {  # Include response variable column
    stop("The number of columns in data does not match the number of predictors in the model")
  }
}

