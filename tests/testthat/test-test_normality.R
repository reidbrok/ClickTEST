test_that("normality work", {
  set.seed(123)

  # Number of samples
  n <- 1000

  # Create a data frame
  df <- data.frame(
    normal1 = rnorm(n, mean = 50, sd = 10),    # Normally distributed
    normal2 = rnorm(n, mean = 100, sd = 20),   # Normally distributed
    uniform = runif(n, min = 0, max = 50),     # Uniform distribution (non-normal)
    poisson = rpois(n, lambda = 20),           # Poisson distribution (non-normal)
    exponential = rexp(n, rate = 0.1)          # Exponential distribution (non-normal)
  )
  expect_equal(test_normality(df, threshold = 0.05)$normal, c("normal1","normal2"))
})
