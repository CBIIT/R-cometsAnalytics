# Test the call COMETS::getModelData(exmetabdata, modelspec="Interactive",
#	exposures=c("age","bmi_grp"), where=c("age>40","bmi_grp>2"))
#   from the vignette

context("getModelData age>40 bmi_grp>2")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_exmodeldata_age40_bmi2.rda")
load(rdafile)

# Call the function to test
exmodeldata <- COMETS::getModelData(test_exmetabdata, modelspec="Interactive",
	            exposures=c("age","bmi_grp"), where=c("age>40","bmi_grp>2"))

# Compare result to the baseline
test_that("COMETS::getModelData(exmetabdata,modelspec = Interactive, where=c(age>40, bmi_grp>2))",
{
  expect_equal(exmodeldata, test_exmodeldata)
})

