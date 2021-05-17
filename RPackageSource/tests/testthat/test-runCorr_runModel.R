# Test COMETS::runCorr

context("runCorr runModel")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
load(rdafile)
rdafile <- file.path(dir, "test_objects", "test_exmodeldata_bmi_age.rda")
load(rdafile)

# Call the functions to test
results1  <- COMETS::runCorr(test_exmodeldata, test_exmetabdata,"DPP")
results2  <- COMETS::runModel(test_exmodeldata, test_exmetabdata,"DPP")

keep     <- c("corr", "pvalue")
results1 <- results1[, keep, drop=FALSE]
results2 <- (results2$Effects)[, c("corr", "pvalue"), drop=FALSE]
colnames(results2) <- keep

# Compare results
test_that("runCorr vs runModel",
{
  expect_equal(results1, results2)
})

