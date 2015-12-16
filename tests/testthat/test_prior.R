context("Testing make_prior")

test_that("make_prior computes a vector with means and a covariance matrix with 1/alpha", {
  prior = make_prior(alpha = 1, n = 3)

  expect_equal(prior[[1]], matrix(rep(0,3), nrow = 3))
  expect_equal(prior[[2]], diag(1/1, nrow = 3))
})
