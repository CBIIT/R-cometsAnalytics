
context("runModel.getFormulaStr")

yvar     <- "y"
dmatCols <- c("age", "gender")
t1var    <- "time1"
t2var    <- "time2"
svar     <- "group" 

# correct formula strings
b.form1 <- "y ~ age + gender"  # glm, lm
b.form2 <- "y ~ age + gender + strata(group)"  # conditional logistic
b.form3 <- "Surv(time1, y) ~ age + gender"  # Survival
b.form4 <- "Surv(time1, time2, y) ~ age + gender"  # Survival

form1 <- RcometsAnalytics:::runModel.getFormulaStr(yvar, dmatCols) 
form2 <- RcometsAnalytics:::runModel.getFormulaStr(yvar, dmatCols, strata.var=svar) 
form3 <- RcometsAnalytics:::runModel.getFormulaStr(yvar, dmatCols, time1.var=t1var) 
form4 <- RcometsAnalytics:::runModel.getFormulaStr(yvar, dmatCols, time1.var=t1var, time2.var=t2var) 

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.getFormulaStr",
{
  expect_equal(b.form1, form1)
  expect_equal(b.form2, form2)
  expect_equal(b.form3, form3)
  expect_equal(b.form4, form4)
})

