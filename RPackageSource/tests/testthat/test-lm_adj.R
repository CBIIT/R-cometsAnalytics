
context("lm adj")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_lm_adj.rda")
load(rdafile)

# Call the function to test
obj <- RcometsAnalytics::runModel(modeldata, b_data, cohort = "", op=op)

# Compare result to the baseline.
attr(b_obj, "ptime") <- NULL
attr(obj, "ptime")   <- NULL 
if (is.list(b_obj) && is.list(obj)) {
  b_obj$Info <- NULL
  obj$Info   <- NULL
}
test_that("RcometsAnalytics:: lm adj",
{
  expect_equal(b_obj, obj)
})

