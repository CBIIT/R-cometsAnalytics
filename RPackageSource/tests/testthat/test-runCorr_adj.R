
context("runCorr adj")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_runCorr_adj.rda")
load(rdafile)

# Call the function to test
obj <- COMETS::runCorr(modeldata, b_data, cohort = "cohort")

# Compare result to the baseline. 
attr(b_runCorr, "ptime") <- NULL
attr(obj, "ptime")       <- NULL
test_that("COMETS::runCorr adj",
{
  expect_equal(b_runCorr, obj)
})

