
context("runModel.waldTest.main")

parms         <- c(2, -1, NA, 0.5)
nms           <- c("p1", "p.1", "p.1.2", "P.1")
cov           <- diag(length(parms))
names(parms)  <- nms
rownames(cov) <- nms
colnames(cov) <- nms
varnames      <- nms[2:3]

# Baseline
bobj <- list(pvalue=2*pnorm(1, lower.tail=FALSE), df=1, test=-1)

obj  <- RcometsAnalytics:::runModel.waldTest.main(parms, cov, varnames)

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.waldTest.main",
{
  expect_equal(bobj$pvalue, as.numeric(obj$pvalue))
  expect_equal(bobj$test,   as.numeric(obj$test))
  expect_equal(bobj$df,     as.numeric(obj$df))
})

