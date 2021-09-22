
context("designMat no vars")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline object
rdafile <- file.path(dir, "test_objects", "test_designMat_noVars.rda")
load(rdafile)

# Call the function to test
obj <- RcometsAnalytics:::runModel.designMat(b_data$subjdata, vars) 

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.designMat no vars",
{
  expect_equal(b_obj, obj)
})

