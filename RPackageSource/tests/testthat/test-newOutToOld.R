
context("newVersionOutToOldOut")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline objects b_runCorr, b_corr
rdafile <- file.path(dir, "test_objects", "test_newCorrToOld.rda")
load(rdafile)

# Call the function to test
obj <- COMETS:::newVersionOutToOldOut(b_corr)

# Compare result to the baseline. 
attr(b_runCorr, "ptime") <- NULL
attr(obj, "ptime")       <- NULL 

test_that("COMETS:::newVersionOutToOldOut",
{
  expect_equal(b_runCorr, obj)
})

