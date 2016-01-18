context("Testing make_prior")

test_that("make_prior computes a matrix with means and a covariance matrix with 1/alpha", {
  prior = make_prior(alpha = 1, n = 3)

  expect_equal(dim(prior[[1]])[1], 3)
  expect_equal(dim(prior[[2]])[1], 3)
  expect_equal(dim(prior[[2]])[2], 3)

  expect_equal(prior[[1]][1],0)
  expect_equal(prior[[1]][2],0)
  expect_equal(prior[[1]][3],0)

  expect_equal(prior[[2]][1,1],1)
  expect_equal(prior[[2]][1,2],0)
  expect_equal(prior[[2]][1,3],0)
  expect_equal(prior[[2]][2,1],0)
  expect_equal(prior[[2]][2,2],1)
  expect_equal(prior[[2]][2,3],0)
  expect_equal(prior[[2]][3,1],0)
  expect_equal(prior[[2]][3,2],0)
  expect_equal(prior[[2]][3,3],1)
})
