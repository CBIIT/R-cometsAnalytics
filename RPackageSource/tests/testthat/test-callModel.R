
context("runModel.callModel")

# Set up data
n    <- 20
int  <- rep(1, n)
y    <- rep(0:1, times=n/2)
x    <- c(rep(0, times=n/2), rep(1, times=n/2))
e    <- seq(from=0.1, to=2, by=0.1)
t1   <- rep(1:5, times=4)
t2   <- t1 + 1:n
g    <- rep(1:5, each=4)

dmat <- cbind(int, x, e)
colnames(dmat) <- c("(Intercept)", "x", "e")

data         <- as.data.frame(dmat)
data[, "y"]  <- y
data[, "t1"] <- t1
data[, "t2"] <- t2
data[, "g"]  <- g

# Baseline objects
b.obj1 <- summary(lm(y ~ x + e, data=data))$coefficients
b.obj2 <- summary(glm(y ~ x + e, data=data, family=binomial()))$coefficients
b.obj3 <- summary(coxph(Surv(t1, y) ~ x + e, data=data, ties="efron"))$coefficients
b.obj4 <- summary(coxph(Surv(t1, y) ~ x + e, data=data, ties="breslow"))$coefficients
b.obj5 <- summary(coxph(Surv(t1, y) ~ x + e, data=data, ties="exact"))$coefficients
b.obj6 <- summary(coxph(Surv(t1, t2, y) ~ x + e, data=data, ties="efron"))$coefficients
b.obj7 <- summary(coxph(Surv(t1, t2, y) ~ x + e, data=data, ties="breslow"))$coefficients
b.obj8 <- summary(clogit(y ~ x + e + strata(g), data=data, method="exact"))$coefficients

# Call package functions
op   <- list(model.options=list(singular.ok=TRUE, tol=1e-7))
obj1 <- summary(RcometsAnalytics:::runModel.callLM(dmat, y, op))$coefficients
op   <- list(model.options=list(singular.ok=TRUE, famFun=binomial(), control=glm.control()))
obj2 <- summary(RcometsAnalytics:::runModel.callGLM(dmat, y, op))$coefficients
op   <- list(model.options=list(n.time.vars=1, time1.vec=t1, ties="efron"))
obj3 <- summary(RcometsAnalytics:::runModel.callCoxph(dmat, y, op))$coefficients
op   <- list(model.options=list(n.time.vars=1, time1.vec=t1, ties="breslow"))
obj4 <- summary(RcometsAnalytics:::runModel.callCoxph(dmat, y, op))$coefficients
op   <- list(model.options=list(n.time.vars=1, time1.vec=t1, ties="exact"))
obj5 <- summary(RcometsAnalytics:::runModel.callCoxph(dmat, y, op))$coefficients
op   <- list(model.options=list(n.time.vars=2, time1.vec=t1, time2.vec=t2, ties="efron"))
obj6 <- summary(RcometsAnalytics:::runModel.callCoxph(dmat, y, op))$coefficients
op   <- list(model.options=list(n.time.vars=2, time1.vec=t1, time2.vec=t2, ties="breslow"))
obj7 <- summary(RcometsAnalytics:::runModel.callCoxph(dmat, y, op))$coefficients
op   <- list(model.options=list(group.vec=g, weightsFlag=0, method="exact"))
obj8 <- summary(RcometsAnalytics:::runModel.callClogit(dmat, y, op))$coefficients

# Compare result to the baseline. 
test_that("RcometsAnalytics:::runModel.callModel",
{
  expect_equal(b.obj1, obj1)
  expect_equal(b.obj2, obj2)
  expect_equal(b.obj3, obj3)
  expect_equal(b.obj4, obj4)
  expect_equal(b.obj5, obj5)
  expect_equal(b.obj6, obj6)
  expect_equal(b.obj7, obj7)
  expect_equal(b.obj8, obj8)
})

