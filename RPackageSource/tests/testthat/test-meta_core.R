
context("meta_calc")

# Test 1
beta     <- c(1, 2)
se       <- c(1, 1)
# Correct results
true.est <- 1.5
true.se  <- sqrt(0.5)
true.n   <- 2
obj      <- RcometsAnalytics::meta_calc(beta, se)

# Test 2
beta     <- c(1, NA, 2, 3)
se       <- c(1, 1,  1, 0)
obj2     <- RcometsAnalytics::meta_calc(beta, se)

# Compare result to the baseline. 
test_that("RcometsAnalytics::meta_calc",
{
  expect_equal(true.est, obj$fixed.estimate)
  expect_equal(true.est, obj$random.estimate)
  expect_equal(true.se,  obj$fixed.std.error)
  expect_equal(true.se,  obj$random.std.error)
  expect_equal(true.n,   obj$n.cohort)

  expect_equal(true.est, obj2$fixed.estimate)
  expect_equal(true.est, obj2$random.estimate)
  expect_equal(true.se,  obj2$fixed.std.error)
  expect_equal(true.se,  obj2$random.std.error)
  expect_equal(true.n,   obj2$n.cohort)

})

