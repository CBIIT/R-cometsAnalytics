# Test that the file cometsInputAge.xlsx get read correctly with COMETS::readCOMETSinput

context("readCOMETSinput")

dir     <- system.file("extdata", package="COMETS", mustWork=TRUE)
rdafile <- file.path(dir, "test_objects", "test_exmetabdata.rda")
csvfile <- file.path(dir, "cometsInputAge.xlsx")

# Read the data file
exmetabdata <- COMETS::readCOMETSinput(csvfile)

# Load the baseline data
load(rdafile)

test_that("COMETS::readCOMETSinput(cometsInputAge.xlsx)",
{
  expect_identical(exmetabdata, test_exmetabdata)
})

