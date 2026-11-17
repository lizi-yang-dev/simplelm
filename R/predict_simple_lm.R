#' Predict Values from a Simple Linear Regression Model
#'
#' @param object A fitted simple_lm model.
#' @param newdata A numeric matrix or data frame of predictors.
#' @param ... Additional arguments (unused).
#'
#' @return A numeric vector of predicted values.
#' @export

predict.simple_lm <- function(object, newdata, ...) {

  # Convert new data to a matrix
  newdata <- as.matrix(newdata)

  # Check whether new data are numeric
  if (!is.numeric(newdata)) {
    stop("newdata must contain only numeric values.")
  }

  # Check the number of predictors
  if (ncol(newdata) != length(object$predictor_names)) {
    stop("Incorrect number of predictors.")
  }

  # Check for missing or infinite values
  if (any(!is.finite(newdata))) {
    stop("newdata must contain only finite values.")
  }

  # Check column names when provided
  if (!is.null(colnames(newdata))) {
    if (!identical(colnames(newdata), object$predictor_names)) {
      stop("Predictor names or order do not match the fitted model.")
    }
  }

  # Add intercept column
  X_new <- cbind(Intercept = 1, newdata)

  # Calculate predictions
  predictions <- as.vector(X_new %*% object$coefficients)

  return(predictions)
}
