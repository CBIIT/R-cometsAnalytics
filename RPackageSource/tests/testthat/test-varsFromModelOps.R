
context("infile.getAllVarsFromModelOptions")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline objects (b_obj, modops)
rdafile <- file.path(dir, "test_objects", "test_varsFromModelOps.rda")
load(rdafile)

# Call the function to test
obj  <- RcometsAnalytics:::infile.getAllVarsFromModelOptions(modops)

# Compare result to the baseline. 
test_that("RcometsAnalytics:::infile.getAllVarsFromModelOps",
{
  expect_equal(b_obj, obj)
})

