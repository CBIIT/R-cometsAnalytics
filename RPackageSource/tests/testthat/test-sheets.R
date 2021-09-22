
context("sheets")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline objects (b_f, sheet, b_sheets, b_obj)
rdafile <- file.path(dir, "test_objects", "test_sheets.rda")
load(rdafile)

# Input Excel file to test
f <- file.path(dir, "test_objects", b_f)

# Call the function to test
sheets <- readxl::excel_sheets(f)
obj    <- readExcelSheet(f, sheet, sheets, stopOnError=0, optional=0) 

# Compare result to the baseline. 
test_that("RcometsAnalytics:::readExcelSheet",
{
  expect_equal(b_sheets, sheets)
  expect_equal(b_obj, obj)
})

