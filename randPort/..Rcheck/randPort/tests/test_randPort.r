
context("Testing randPort objects")

data(jan)
rP <- randPort(jan, match.var = "value", weight.var = "portfolio", n=10)
test_that("All the weights sum to 1", {
    expect_that(all(abs(apply(rP@matched.weights, 2, sum)-1) < 1e-7), is_true())
})

test_that("Errors are thrown for invalid input", {
    expect_that(randPort(jan, match.var = "Michaela ditched me", weight.var = "portfolio", n = 10), throws_error())
    expect_that(randPort(jan, match.var = "value", weight.var = c("portfolio", "benchmark"), n = 10), throws_error())
    expect_that(randPort(jan, match.var = "value", weight.var = "Mike needs a new phone", n = 10), throws_error())
    jan$zeros = 0
    expect_that(randPort(jan, match.var = "value", weight.var = "zeros", n = 10), throws_error())
})
