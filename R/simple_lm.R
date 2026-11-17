#' Fit a Linear Regression Model Using Ordinary Least Squares
#'
#' Fits a linear regression model using the normal equations.
#' The model includes an intercept by default.
#'
#' @param X A numeric matrix, data frame, or vector of predictors.
#' @param y A numeric vector of response values.
#'
#' @details
#' Missing and infinite values are not allowed.
#' The function requires more observations than parameters
#' and does not support rank-deficient design matrices.
#'
#' @return An object of class `simple_lm` containing:
#' \describe{
#'   \item{coefficients}{Estimated regression coefficients.}
#'   \item{fitted.values}{Predicted values for the training data.}
#'   \item{residuals}{Model residuals.}
#'   \item{predictor_names}{Names of predictor variables.}
#' }
#'
#' @examples
#' x <- 1:10
#' y <- 2 + 3 * x
#' model <- simple_lm(x, y)
#' model$coefficients
#'
#' @export
simple_lm <- function(X, y) {
  # Check that y is a numeric vector
  if (!is.numeric(y) || !is.null(dim(y))) {
    stop("y must be a numeric vector.")
  }

  # Convert predictors to a matrix
  X <- as.matrix(X)

  # Check that X is numeric
  if (!is.numeric(X)) {
    stop("X must contain only numeric values.")
  }

  # Check matching sample sizes
  if (nrow(X) != length(y)) {
    stop("The number of rows in X must match the length of y.")
  }

  # Check missing or infinite values
  if (any(!is.finite(X)) || any(!is.finite(y))) {
    stop("X and y must contain only finite values.")
  }

  # Convert X to a matrix
  X <- as.matrix(X)

  # Assign column names if missing
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("x", seq_len(ncol(X)))
  }

  # Add an intercept column
  X <- cbind(Intercept = 1, X)

  # Check sufficient sample size
  if (nrow(X) <= ncol(X)) {
    stop("The number of observations must exceed the number of parameters.")
  }

  # Check for perfect multicollinearity
  if (qr(X)$rank < ncol(X)) {
    stop("The design matrix is rank deficient.")
  }

  # Compute X'X
  XtX <- crossprod(X)

  # Compute X'y
  Xty <- crossprod(X, y)

  # Solve the normal equations
  coefficients <- solve(XtX, Xty)

  # Compute fitted values
  fitted_values <- as.vector(X %*% coefficients)

  # Compute residuals
  residuals <- y - fitted_values

  # Return results
  model <- list(
    coefficients = coefficients,
    fitted.values = fitted_values,
    residuals = residuals,
    predictor_names = colnames(X)[-1]
  )

  class(model) <- "simple_lm"

  return(model)
}
