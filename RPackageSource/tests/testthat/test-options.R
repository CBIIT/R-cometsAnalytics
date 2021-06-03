
context("options")

dir <- system.file("extdata", package="COMETS", mustWork=TRUE)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_options.rda")
load(rdafile)

op_global <- COMETS:::getValidGlobalOps()
op_glm    <- COMETS:::getDefaultModelOptions("glm")
op_lm     <- COMETS:::getDefaultModelOptions("lm")
op_corr   <- COMETS:::getDefaultModelOptions("correlation")

# Compare result to the baseline. 
test_that("options",
{
  expect_equal(b_op_global, op_global)
  expect_equal(b_op_glm, op_glm)
  expect_equal(b_op_lm, op_lm)
  expect_equal(b_op_corr, op_corr)
})

