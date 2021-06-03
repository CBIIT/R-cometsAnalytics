
context("getModelData_interactive")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_modeldata_interactive.rda")
load(rdafile)

# Call the function to test
obj <- COMETS::getModelData(b_data, modelspec="Interactive",
           outcomes=outcomes, exposures=exposures, adjvars=adjvars, where=where)

# Compare result to the baseline. 
test_that("COMETS::getModelData",
{
  expect_equal(b_obj, obj)
})

