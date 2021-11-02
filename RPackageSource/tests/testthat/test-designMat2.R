
context("runModel.designMat2")

int  <- rep(1, 10)
x1   <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1))
x2   <- seq(from=0.1, to=1, by=0.1)
x3   <- c(rep("a", 5), rep("b", 5))
data <- data.frame(x1=x1, x2=x2, x3=x3)

# Baseline matrix
bmat           <- cbind(int, as.numeric(x1==2), as.numeric(x1==3), x2, as.numeric(x3=="b"))
colnames(bmat) <- c("(Intercept)", "x1.2", "x1.3", "x2", "x3.b")
rownames(bmat) <- 1:10

mat  <- RcometsAnalytics:::runModel.designMat(data, c("x1", "x2", "x3"), categorical=NULL)
attr(mat, "assign")    <- NULL
attr(mat, "contrasts") <- NULL

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.designMat2",
{
  expect_equal(bmat, mat)
})

