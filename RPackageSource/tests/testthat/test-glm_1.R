# Test COMETS::runModel

context("runModel glm1")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)
rdafile <- file.path(dir, "test_objects", "test_glm_data1.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_glm_res1.rda")
load(rdafile)

# Call the function to test
results  <- COMETS::runModel(test_exmodeldata, test_exmetabdata, cohort="DPP",
             op=list(model="glm", model.options=list(family="binomial")))


# Compare result to the baseline. Note the the runCorr function adds a time
#  attribute to the returned object, and the time cannot be reproduced.
attr(results, "ptime")      <- NULL
attr(test_results, "ptime") <- NULL
test_that("COMETS::runModel(op=list(model=glm, family=binomial))",
{
  expect_equal(results, test_results)
})

