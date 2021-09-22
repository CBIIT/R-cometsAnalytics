
context("designMat")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline object
rdafile <- file.path(dir, "test_objects", "test_designMat.rda")
load(rdafile)

# Call the function to test
dmat <- RcometsAnalytics:::runModel.designMat(b_data$subjdata, vars) 

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.designMat",
{
  expect_equal(b_dmat, dmat)
})

