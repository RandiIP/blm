context("Test the statistical functionallity")

seed = abs(as.integer(1000 * rnorm(1)))

w0 = 0.3
w1 = 1.2
b = 1

w0s50 = w1s50 = w0s5000 = w1s5000 = rep(0,100)

for (i in 1:100) {
  set.seed(seed + i)
  x50 = rnorm(50)
  x5000 = rnorm(5000)

  y50 = rnorm(50, w0 + w1 * x50, 1/b)
  y5000 = rnorm(5000, w0 + w1 * x5000, 1/b)

  d50 = data.frame(y=y50, x=x50)
  d5000 = data.frame(y=y5000, x=x5000)

  blm50 = blm(y~x, d50, beta = b)
  blm5000 = blm(y~x, d5000, beta = b)

  w0s50[i] = coefficients(blm50)[[1]]
  w0s5000[i] = coefficients(blm5000)[[1]]
  w1s50[i] = coefficients(blm50)[[2]]
  w1s5000[i] = coefficients(blm5000)[[2]]
}

test_that(paste("The estimation of weigth vectors gets more precise with more data, works with seed: ", seed), {
  expect_less_than(abs(mean(w0s5000) - w0), abs(mean(w0s50) - w0))
  expect_less_than(abs(mean(w1s5000) - w1), abs(mean(w1s50) - w1))
})

test_that(paste("The variance in the posterior decrease with increasing data points, works with seed: ", seed), {
  expect_less_than(var(w0s5000), var(w0s50))
  expect_less_than(var(w1s5000), var(w1s50))
})

