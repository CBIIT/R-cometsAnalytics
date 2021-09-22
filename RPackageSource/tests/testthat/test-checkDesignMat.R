
context("checkDesignMatCols")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_checkDesignMat.rda")
load(rdafile)

# Call the function to test
obj <- RcometsAnalytics:::runModel.checkDesignMatCols(b_dmat, op)

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.checkDesignMatCols",
{
  expect_equal(b_obj, obj)
})

