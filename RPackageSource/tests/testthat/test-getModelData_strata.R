# Test the call COMETS::getModelData(test_exmetabdata,modelspec="Interactive",rowvars=c("lactose","lactate"),
#	colvars=c("age","bmi_grp"),strvars="race_grp")
#  from the vignette

context("getModelData strata")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_exmodeldata_strata.rda")
load(rdafile)

# Call the function to test
exmodeldata <- COMETS::getModelData(test_exmetabdata,modelspec="Interactive",rowvars=c("lactose","lactate"),
	colvars=c("age","bmi_grp"),strvars="race_grp")

# Compare result to the baseline
test_that("COMETS::getModelData(exmetabdata, ... ,strvars=race_grp)",
{
  expect_identical(exmodeldata, test_exmodeldata)
})

