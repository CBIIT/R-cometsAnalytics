
context("options")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_options.rda")
load(rdafile)

op_global <- RcometsAnalytics:::getValidGlobalOps()
op_glm    <- RcometsAnalytics:::getDefaultModelOptions("glm")
op_lm     <- RcometsAnalytics:::getDefaultModelOptions("lm")
op_corr   <- RcometsAnalytics:::getDefaultModelOptions("correlation")

# Compare result to the baseline. 
test_that("options",
{
  expect_equal(b_op_global, op_global)
  expect_equal(b_op_glm, op_glm)
  expect_equal(b_op_lm, op_lm)
  expect_equal(b_op_corr, op_corr)
})

