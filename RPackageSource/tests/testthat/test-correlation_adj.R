
context("runCorr adj")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_correlation_adj.rda")
load(rdafile)

# Call the function to test
obj <- COMETS::runModel(modeldata, b_data, cohort = "cohort")

# Compare result to the baseline. 
attr(b_corr, "ptime") <- NULL
attr(obj, "ptime")    <- NULL
test_that("COMETS::runCorr adj",
{
  expect_equal(b_corr, obj)
})

