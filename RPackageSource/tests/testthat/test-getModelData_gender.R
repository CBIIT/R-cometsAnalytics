# Test the call COMETS::getModelData(test_exmetabdata,modlabel="1 Gender adjusted")
#    from the vignette

context("getModelData gender")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_exmodeldata.rda")
load(rdafile)

# Call the function to test
exmodeldata <- COMETS::getModelData(test_exmetabdata,modlabel="1 Gender adjusted")

# Compare result to the baseline
test_that("COMETS::getModelData(exmetabdata,modlabel=1 Gender adjusted)",
{
  expect_equal(exmodeldata, test_exmodeldata)
})

