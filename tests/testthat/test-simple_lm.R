test_that("simple_lm estimates correct coefficients", {

  x <- 1:10
  y <- 2 + 3 * x

  model <- simple_lm(x, y)

  expect_equal(
    as.numeric(model$coefficients),
    c(2, 3),
    tolerance = 1e-8
  )

})

test_that("multiple regression matches lm", {

  set.seed(123)

  x1 <- rnorm(100)
  x2 <- rnorm(100)
  y <- 2 + 3*x1 - 1.5*x2 + rnorm(100, sd = 0.5)

  X <- data.frame(x1, x2)

  my_model <- simple_lm(X, y)
  original_model <- lm(y ~ x1 + x2)

  expect_equal(
    as.numeric(my_model$coefficients),
    as.numeric(coef(original_model)),
    tolerance = 1e-8
  )

})

test_that("prediction matches lm", {

  set.seed(123)

  x <- 1:20
  y <- 2 + 3*x + rnorm(20)

  my_model <- simple_lm(x, y)
  original_model <- lm(y ~ x)

  newdata <- data.frame(x1 = c(5, 10, 15))

  expect_equal(
    as.numeric(predict(my_model, newdata)),
    as.numeric(predict(original_model,
                       newdata = data.frame(x = c(5, 10, 15)))),
    tolerance = 1e-8
  )

})

test_that("summary statistics match lm", {

  set.seed(123)

  x <- 1:20
  y <- 2 + 3*x + rnorm(20)

  my_model <- simple_lm(x, y)
  original_model <- lm(y ~ x)

  my_summary <- summary(my_model)
  original_summary <- summary(original_model)

  expect_equal(my_summary$r.squared,
               original_summary$r.squared)

  expect_equal(my_summary$adj.r.squared,
               original_summary$adj.r.squared)

  expect_equal(my_summary$residual.standard.error,
               original_summary$sigma)

})

test_that("invalid inputs produce errors", {

  # Different sample sizes
  expect_error(
    simple_lm(matrix(1:10, ncol = 1), 1:5)
  )

  # Missing values
  expect_error(
    simple_lm(matrix(c(1, 2, NA, 4), ncol = 1), 1:4)
  )

  # Non-numeric predictors
  expect_error(
    simple_lm(data.frame(x = c("a", "b", "c")), 1:3)
  )

  # Perfect multicollinearity
  x1 <- 1:10
  x2 <- 2 * x1

  expect_error(
    simple_lm(data.frame(x1, x2), 1:10)
  )

})
