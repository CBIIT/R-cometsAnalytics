# Test COMETS::runCorr

context("runCorr runModel")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)
rdafile <- file.path(dir, "test_objects", "test_exmodeldata_bmi_age.rda")
load(rdafile)

# Call the functions to test
results1  <- COMETS::runCorr(test_exmodeldata, test_exmetabdata,"DPP")
results2  <- COMETS::runModel(test_exmodeldata, test_exmetabdata,"DPP")

# Compare results
attr(results1, "ptime") <- NULL
attr(results2, "ptime") <- NULL
test_that("runCorr vs runModel",
{
  expect_equal(results1, results2)
})

