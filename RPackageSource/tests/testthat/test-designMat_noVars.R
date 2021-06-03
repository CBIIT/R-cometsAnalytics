
context("designMat no vars")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline object
rdafile <- file.path(dir, "test_objects", "test_designMat_noVars.rda")
load(rdafile)

# Call the function to test
obj <- COMETS:::runModel.designMat(b_data$subjdata, vars) 

# Compare result to the baseline. 
test_that("COMETS:::runModel.designMat no vars",
{
  expect_equal(b_obj, obj)
})

