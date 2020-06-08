# Test the call COMETS::getModelData(test_exmetabdata,modelspec = "Interactive",colvars = c("bmi_grp","age"))
#   from the vignette

context("getModelData bmi age")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)

# Load the baseline results
rdafile <- file.path(dir, "test_objects", "test_exmodeldata_bmi_age.rda")
load(rdafile)

# Call the function to test
exmodeldata <- COMETS::getModelData(test_exmetabdata,modelspec = "Interactive",colvars = c("bmi_grp","age"))

# Compare result to the baseline
test_that("COMETS::getModelData(exmetabdata,modelspec = Interactive,colvars = c(bmi_grp,age))",
{
  expect_identical(exmodeldata, test_exmodeldata)
})

