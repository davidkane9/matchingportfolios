context("Testing constraints")

Emat = matrix(1, ncol = 10, nrow = 1)
x0 = c(1, rep(0, 9))
test_that("Generated Weights match the original constraints", {
    expect_that(Emat %*% getWeights(Emat, x0, 1), equals(Emat %*% x0))
    expect_that(all(getWeights(Emat, x0, 1) > 0), is_true())
})
