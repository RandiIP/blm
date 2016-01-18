context("Test the predictions")

seed = abs(as.integer(1000 * rnorm(1)))

w0 = 0.3
w1 = 1.2
b = 1

set.seed(seed)
x50 = rnorm(50)
x5000 = rnorm(5000)

y50 = rnorm(50, w0 + w1 * x50, 1/b)
y5000 = rnorm(5000, w0 + w1 * x5000, 1/b)

d50 = data.frame(y=y50, x=x50)
d5000 = data.frame(y=y5000, x=x5000)

blm50 = blm(y~x, d50, beta = b)
blm5000 = blm(y~x, d5000, beta = b)

newX = data.frame(x = rnorm(100))

predictions50 = unlist(predict(blm50, newX)$mean)
predictions5000 = unlist(predict(blm5000, newX)$mean)

trueY = unlist(w0 + w1 * newX)

diff50 = abs(trueY - predictions50)
diff5000 = abs(trueY - predictions5000)

test_that(paste("The prediction gets better with more data, works with seed: ", seed), {
  expect_less_than(sum(diff5000), sum(diff50))
})
