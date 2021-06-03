
context("infile.getAllVarsFromModels")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline objects (b_obj, mods)
rdafile <- file.path(dir, "test_objects", "test_varsFromModels.rda")
load(rdafile)

# Call the function to test
obj <- COMETS:::infile.getAllVarsFromModels(mods)

# Compare result to the baseline. 
test_that("COMETS:::infile.getAllVarsFromModels",
{
  expect_equal(b_obj, obj)
})

