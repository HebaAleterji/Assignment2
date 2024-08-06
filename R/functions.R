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
  validate_inputs(data, model)
  measure <- match.arg(measure)

  result <- switch(measure,
                   "cooks" =  cooks.distance(model),
                   "dffits" = dffits(model),
                   "hadi" = hadis_influence_measure(model))

  plot_influence(data, model)

  return(result)
}

#' @title Plot Influence Measures
#' @description Plots the selected influence measure for an lm model.
#' @param data The dataset used in the model.
#' @param model An object of class lm.
#' @param measure The influence measure to plot ("cooks", "dffits", or "hadi").
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' plot_influence(mtcars, model, "cooks")
plot_influence <- function(data, model, measure = "cooks") {
  diagnostics <- influence_diagnostics(data, model, measure)

  plot(diagnostics, main = paste("Influence Measure:", measure),
       ylab = "Influence", xlab = "Index")
}

