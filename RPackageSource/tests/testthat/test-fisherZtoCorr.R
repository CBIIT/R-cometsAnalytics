
context("fisherZtoCorr")

trueVals <- c(-1, 0, 1, NA, NaN)
obj      <- RcometsAnalytics:::FisherZtoCorr(c(-Inf, 0, Inf, NA, NaN))

# Compare result to the baseline. 
test_that("RcometsAnalytics:::fisherZtoCorr",
{
  expect_equal(trueVals, obj)
})

