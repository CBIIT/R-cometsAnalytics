
context("glm adj")

dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)

# Load the baseline data
rdafile <- file.path(dir, "test_objects", "test_data.rda")
load(rdafile)

# Load the baseline objects
rdafile <- file.path(dir, "test_objects", "test_glm_adj.rda")
load(rdafile)

# Call the function to test
obj <- RcometsAnalytics::runModel(modeldata, b_data, cohort = "", op=op)

# Test independent of RcometsAnalytics  
pvals0  <- b_obj$Effects[, "pvalue", drop=TRUE]
data    <- modeldata$gdta
mv      <- "METAB"
form    <- as.formula(paste0(outcomes, " ~ ", adjvars, " + ", mv))
metabs  <- modeldata$ccovs
nmetabs <- length(metabs) 
pvals   <- rep(NA, nmetabs)
for (i in 1:nmetabs) {
  data[, mv] <- data[, metabs[i], drop=TRUE]
  coef       <- summary(glm(form, data=data, family=binomial()))$coefficients
  pvals[i]   <- coef[mv, 4]	
}

# Compare result to the baseline. 
b_obj$Info           <- NULL
obj$Info             <- NULL
attr(b_obj, "ptime") <- NULL
attr(obj, "ptime")   <- NULL
test_that("RcometsAnalytics:: glm adj",
{
  expect_equal(b_obj, obj)
  expect_equal(pvals0, pvals)
})

