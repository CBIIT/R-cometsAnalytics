
context("runModel.getModelVectors")

family    <- "gaussian"
yvec      <- c(-1, 0,  0.5, 1, 2, NA, NaN, Inf)
wvec      <- c(1,  1, -0.5, 1, 1, 1,  1,   1)
ovec      <- c(NA, 1, -0.5, 1, 1, 1,  1,   1)
gdta      <- data.frame(y=yvec, wgts=wvec, offset=ovec) 
modeldata <- list(gdta=gdta, designSubOrder=1:length(yvec), 
                  wgtcov="wgts", offcov="offset")
mop       <- list(family=family, weightsFlag=1, offsetFlag=1, 
                  timeFlag=0, groupFlag=0,
                  weights.vec=wvec, offset.vec=ovec)
op        <- list(model.options=mop)

# Correct results
bv.y   <- c(TRUE,  TRUE, TRUE,  TRUE, TRUE, FALSE, FALSE, FALSE)
bv.w   <- c(TRUE,  TRUE, FALSE, TRUE, TRUE, TRUE,  TRUE,  TRUE)
bv.o   <- c(FALSE, TRUE, TRUE,  TRUE, TRUE, TRUE,  TRUE,  TRUE)
subset <- bv.y & bv.w & bv.o
bobj   <- list(response=yvec, subset=subset, op=op) 

# Call the function to test
obj <- RcometsAnalytics:::runModel.getModelVectors(modeldata, "y", op)

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.getModelVectors",
{
  expect_equal(bobj$response, obj$response)
  expect_equal(bobj$subset, obj$subset)
  expect_equal(bobj$op, obj$op)
})

