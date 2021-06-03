
context("readCOMETSinput")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data and excel file (b_data, b_f)
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Input Excel file to test
f <- file.path(dir, "test_objects", b_f)

# Call the function to test
data  <- COMETS::readCOMETSinput(f)

# Compare result to the baseline. 
test_that("COMETS::readCOMETSinput",
{
  expect_equal(b_data, data)
})

