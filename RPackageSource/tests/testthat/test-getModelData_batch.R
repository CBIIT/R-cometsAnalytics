
context("getModelData_batch")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects (b_obj, modlabel)
rdafile <- file.path(dir, "test_objects", "test_modeldata_batch.rda")
load(rdafile)

# Call the function to test
obj <- COMETS::getModelData(b_data, modlabel=modlabel, modelspec="Batch")

# Compare result to the baseline. 
test_that("COMETS::getModelData",
{
  expect_equal(b_obj, obj)
})

