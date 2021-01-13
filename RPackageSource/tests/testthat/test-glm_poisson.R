# Test COMETS::runModel

context("runModel glm poisson")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)
rdafile <- file.path(dir, "test_objects", "test_glm_data2.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_glm_poisson.rda")
load(rdafile)

# Call the function to test
results  <- COMETS::runModel(test_exmodeldata, test_exmetabdata, cohort="DPP",
             op=list(model="glm", model.options=list(family="poisson")))


# Compare result to the baseline. 
attr(results, "ptime")      <- NULL
attr(test_results, "ptime") <- NULL
test_that("COMETS::runModel(op=list(model=glm, family=poisson))",
{
  expect_equal(results, test_results)
})

