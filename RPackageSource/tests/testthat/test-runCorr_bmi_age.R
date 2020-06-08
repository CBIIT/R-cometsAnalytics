# Test COMETS::runCorr

context("runCorr bmi age")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)
rdafile <- file.path(dir, "test_objects", "test_exmodeldata_bmi_age.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_excorrdata_bmi_age.rda")
load(rdafile)

# Call the function to test
excorrdata  <- COMETS::runCorr(test_exmodeldata, test_exmetabdata,"DPP")

# Compare result to the baseline. Note the the runCorr function adds a time
#  attribute to the returned object, and the time cannot be reproduced.
attr(excorrdata, "ptime")      <- NULL
attr(test_excorrdata, "ptime") <- NULL
test_that("COMETS::runCorr(exmodeldata,exmetabdata, DPP)",
{
  expect_identical(excorrdata, test_excorrdata)
})

