
context("lm adj")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_lm_adj.rda")
load(rdafile)

# Call the function to test
obj <- COMETS::runModel(modeldata, b_data, cohort = "", op=op)

# Compare result to the baseline.
attr(b_obj, "ptime") <- NULL
attr(obj, "ptime")   <- NULL 
test_that("COMETS:: lm adj",
{
  expect_equal(b_obj, obj)
})

