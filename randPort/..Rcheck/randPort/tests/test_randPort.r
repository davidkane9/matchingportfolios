
context("Testing randPort objects")

data(jan)
rP <- randPort(jan, match.var = "value", weight.var = "portfolio", n=10)
test_that("", {
    expect_that(all(apply(rP@matched.weights, 2, sum)==1), is_true())

})
