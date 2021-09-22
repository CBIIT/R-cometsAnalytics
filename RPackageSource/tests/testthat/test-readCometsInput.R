
context("readCOMETSinput")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data and excel file (b_data, b_f)
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Input Excel file to test
f <- file.path(dir, "test_objects", b_f)

# Call the function to test
data  <- RcometsAnalytics::readCOMETSinput(f)

# Compare result to the baseline. 
test_that("RcometsAnalytics::readCOMETSinput",
{
  expect_equal(b_data, data)
})

