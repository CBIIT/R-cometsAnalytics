
context("filterCOMETSinput")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data (b_data)
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects (b_obj, where)
rdafile <- file.path(dir, "test_objects", "test_filter.rda")
load(rdafile)

# Call the function to test
obj1 <- COMETS:::filterCOMETSinput(b_data, where=where1)
obj2 <- COMETS:::filterCOMETSinput(b_data, where=where2)
obj3 <- COMETS:::filterCOMETSinput(b_data, where=where3)


# Compare result to the baseline. 
test_that("COMETS::filterCOMETSinput 1",
{
  expect_equal(b_obj1, obj1)
})
test_that("COMETS::filterCOMETSinput 2",
{
  expect_equal(b_obj2, obj2)
})
test_that("COMETS::filterCOMETSinput 3",
{
  expect_equal(b_obj3, obj3)
})

