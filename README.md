
# simplelm

simplelm is an R package for fitting linear regression models using ordinary least squares (OLS).

I wrote this package to practice implementing linear regression with matrix operations instead of calling lm() directly. It supports model fitting, prediction, and basic summary statistics.

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("lizi-yang-dev/simplelm")
```

## Features

- Fit simple and multiple linear regression models.
- Estimate regression coefficients using matrix operations.
- Predict responses for new observations.
- Calculate R-squared, adjusted R-squared, and residual standard error.
- Validate inputs and detect rank-deficient design matrices.

## Example

```r
library(simplelm)

set.seed(123)

x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 2 + 3 * x1 - 1.5 * x2 + rnorm(100, sd = 0.5)

X <- data.frame(x1, x2)

model <- simple_lm(X, y)

# Estimated regression coefficients
model$coefficients

# Model summary
summary(model)

# Predictions
newdata <- data.frame(
  x1 = c(0, 1, 2),
  x2 = c(1, 0, -1)
)

predict(model, newdata)
```

## Comparison with lm()

The package provides a simplified implementation of linear regression. Its results can be compared against R's built-in `lm()` function to verify numerical correctness.

## Limitations

- Only numeric predictors are supported.
- An intercept is always included.
- Missing and infinite values are not supported.
- Rank-deficient design matrices are not supported.
- The normal equations can be numerically unstable for ill-conditioned data.
