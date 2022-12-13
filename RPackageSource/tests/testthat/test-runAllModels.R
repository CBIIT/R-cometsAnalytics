
context("runAllModels")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_runAllModels.rda")
load(rdafile)

# Call the function to test
obj <- RcometsAnalytics::runAllModels(b_data, writeTofile=FALSE)

# Compare result to the baseline. 
test_that("RcometsAnalytics::runAllModels",
{
  for (i in 1:length(b_obj)) {
    attr(b_obj[[i]], "ptime") <- NULL
    attr(obj[[i]], "ptime")   <- NULL
    if (is.list(b_obj[[i]]) && is.list(obj[[i]])) {
      b_obj[[i]]$Info         <- NULL
      obj[[i]]$Info           <- NULL
    }
    expect_equal(b_obj[[i]], obj[[i]])
  }
})

