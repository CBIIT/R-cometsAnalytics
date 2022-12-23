
context("normOutFileStr")

str  <- "cohort=lung$12@"
base <- "cohort_lung_12"
obj  <- RcometsAnalytics:::normOutFileStr(str)

# Compare result to the baseline. 
test_that("RcometsAnalytics:::normOutFileStr",
{
  expect_equal(base, obj)
})

