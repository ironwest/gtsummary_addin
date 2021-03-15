test_that("Test get_terms() for lm: multivariate with no interaction", code = {

  dat <- iris
  mod <- lm("Sepal.Length ~ Sepal.Width + Petal.Length + Species", data=dat)

  res <- get_terms(mod)

  testthat::expect_equal(res,c("Sepal.Width", "Petal.Length", "Species"))

})
