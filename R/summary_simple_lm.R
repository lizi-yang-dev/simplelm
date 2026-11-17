#' Summarize a Simple Linear Regression Model
#'
#' @param object A fitted simple_lm model.
#' @param ... Additional arguments (unused).
#'
#' @return A list containing regression summary statistics.
#' @export
summary.simple_lm <- function(object, ...) {

  residuals <- object$residuals
  fitted_values <- object$fitted.values

  n <- length(residuals)
  p <- length(object$coefficients)

  # Residual degrees of freedom
  df_residual <- n - p

  # Residual sum of squares
  rss <- sum(residuals^2)

  # Residual standard error
  residual_se <- sqrt(rss / df_residual)

  # Total sum of squares
  y <- fitted_values + residuals
  tss <- sum((y - mean(y))^2)

  # R-squared
  r_squared <- 1 - rss / tss

  # Adjusted R-squared
  adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / df_residual

  return(list(
    residual.standard.error = residual_se,
    r.squared = r_squared,
    adj.r.squared = adj_r_squared,
    df.residual = df_residual
  ))
}
