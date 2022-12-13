
context("runCorr adj")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_correlation_adj.rda")
load(rdafile)

# Call the function to test
obj <- RcometsAnalytics::runModel(modeldata, b_data, cohort = "cohort")

# Compare result to the baseline. 
attr(b_corr, "ptime") <- NULL
attr(obj, "ptime")    <- NULL
if (is.list(b_corr) && is.list(obj)) {
  b_corr$Info <- NULL
  obj$Info    <- NULL
}
test_that("RcometsAnalytics::runCorr adj",
{
  expect_equal(b_corr, obj)
})

