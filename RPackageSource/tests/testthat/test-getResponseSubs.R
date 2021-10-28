
context("runModel.getResponseSubs")

y <- c(-1, 0, 0.5, 1, 2, NA, NaN, Inf)

# Correct results
bv1 <- c(TRUE,  TRUE,  TRUE,  TRUE, TRUE,  FALSE, FALSE, FALSE)
bv2 <- c(FALSE, TRUE,  FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
bv3 <- c(FALSE, TRUE,  FALSE, TRUE, TRUE,  FALSE, FALSE, FALSE)
bv4 <- c(FALSE, FALSE, TRUE,  TRUE, TRUE,  FALSE, FALSE, FALSE)
bv5 <- c(TRUE,  TRUE,  TRUE,  TRUE, TRUE,  FALSE, FALSE, FALSE)
bv6 <- c(TRUE,  TRUE,  TRUE,  TRUE, TRUE,  FALSE, FALSE, FALSE)

# Call the function to test
ov1 <- RcometsAnalytics:::runModel.getResponseSubs(y, "gaussian") 
ov2 <- RcometsAnalytics:::runModel.getResponseSubs(y, "binomial") 
ov3 <- RcometsAnalytics:::runModel.getResponseSubs(y, "poisson") 
ov4 <- RcometsAnalytics:::runModel.getResponseSubs(y, "Gamma") 
ov5 <- RcometsAnalytics:::runModel.getResponseSubs(y, "myfamily") 
ov6 <- RcometsAnalytics:::runModel.getResponseSubs(y, "") 

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.getResponseSubs",
{
  expect_equal(bv1, ov1)
  expect_equal(bv2, ov2)
  expect_equal(bv3, ov3)
  expect_equal(bv4, ov4)
  expect_equal(bv5, ov5)
  expect_equal(bv6, ov6)
})

