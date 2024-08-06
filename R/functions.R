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



#' @title Perform Influence Diagnostics
#' @description Computes selected influence measures for an lm model.
#' @param data The dataset used in the model.
#' @param model An object of class lm.
#' @param measure The influence measure to compute ("cooks", "dffits", or "hadi").
#' @return A numeric vector of the selected influence measure.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' influence_diagnostics(mtcars, model, "cooks")
influence_diagnostics <- function(data, model, measure = c("cooks", "dffits", "hadi")) {

  # Subset the data to include only the predictors used in the model
  data_subset <- subset_data(data, model)

  validate_inputs(data_subset, model)
  measure <- match.arg(measure)

  result <- switch(measure,
                   "cooks" =  cooks.distance(model),
                   "dffits" = dffits(model),
                   "hadi" = hadis_influence_measure(model))

  return(result)
}

#' @title Plot Influence Measures
#' @description Plots the provided influence measure result for a model.
#' @param diagnostics The result from influence_diagnostics.
#' @param measure The influence measure being plotted ("cooks", "dffits", or "hadi").
#' @return NULL.
#' @examples
#' diagnostics <- influence_diagnostics(mtcars, model, "cooks")
#' plot_influence(diagnostics, "cooks")
plot_influence <- function(diagnostics, measure = "cooks") {
  plot(diagnostics, main = paste("Influence Measure:", measure),
       ylab = "Influence", xlab = "Index", type = "h", col = "blue")
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

