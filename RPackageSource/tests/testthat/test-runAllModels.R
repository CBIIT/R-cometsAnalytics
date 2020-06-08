# Test COMETS::runAllModels(test_exmetabdata,writeTofile=F)

context("runAllModels")

# Baseline
dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_allresults.rda")
load(rdafile)

# Call the function to test
allresults <- COMETS::runAllModels(test_exmetabdata,writeTofile=F)

# Do not compare ptime attribute
attr(allresults, "ptime")      <- NULL
attr(test_allresults, "ptime") <- NULL
for (i in 1:length(test_allresults)) attr(test_allresults[[i]], "ptime") <- NULL
for (i in 1:length(allresults)) attr(allresults[[i]], "ptime") <- NULL

# Compare result to the baseline
test_that("COMETS::runAllModels(exmetabdata,writeTofile=F)",
{
  expect_identical(allresults, test_allresults)
})

