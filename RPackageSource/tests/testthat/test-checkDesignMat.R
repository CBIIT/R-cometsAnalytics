
context("checkDesignMatCols")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_checkDesignMat.rda")
load(rdafile)

# Call the function to test
obj <- COMETS:::runModel.checkDesignMatCols(b_dmat, op)

# Compare result to the baseline. 
test_that("COMETS:::runModel.checkDesignMatCols",
{
  expect_equal(b_obj, obj)
})

