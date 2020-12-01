# Test COMETS::runModel

context("runModel lm1")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)
rdafile <- file.path(dir, "test_objects", "test_lm_data1.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_lm_res1.rda")
load(rdafile)

# Call the function to test
results  <- COMETS::runModel(test_exmodeldata, test_exmetabdata, cohort="DPP",
                         op=list(model="lm"))


# Compare result to the baseline. Note the the runCorr function adds a time
#  attribute to the returned object, and the time cannot be reproduced.
attr(results, "ptime")      <- NULL
attr(test_results, "ptime") <- NULL
test_that("COMETS::runModel(op=list(model=lm))",
{
  expect_equal(results, test_results)
})

