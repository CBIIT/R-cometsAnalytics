# Strings that are visible to the end user of COMETS. This file makes it easy to
#   change column names, options, etc.

runmodel.getTimeAttr <- function() {"ptime"}

# Delimiter in output lists of variable names
runModel.getVarSep    <- function() {" "}
runModel.getOldVarSep <- function() {" "}
runModel.replaceSpace <- function() {"."}

# Batch or interactive
getMode_batch       <- function() {"Batch"}
getMode_interactive <- function() {"Interactive"}
getAllMetabsName    <- function() {"All metabolites"}
getAllMetabsNewName <- function() {"*"}

# For appending stratification columns
runModel.getStrataColName    <- function() {"stratavar"}
runModel.getStrataNumColName <- function() {"strata"}

# For Warnings matrix
runModel.getWarningCol        <- function() {"type"}
runModel.getObjectCol         <- function() {"object"}
runModel.getMessageCol        <- function() {"message"}
runModel.getWarningsListName  <- function() {"Errors_Warnings"}
runModel.getUnknownErrorStr   <- function() {"unknown error"}
runModel.getTooFewSubsStr     <- function() {"too few subjects"}
runModel.getStratTooFewSubStr <- function() {"Stratum contains to few subjects"}

# Message when exposure has been removed from design matrix
runModel.getExpRemFromDesign <- function() {"exposure removed from design matrix"}

# Message when exposure could not be estimated from the model
runModel.getExpNotEstimated <- function() {"exposure could not be estimated"}

# For option strings
getNoFamilyValue         <- function() {""}
getOpStrSep              <- function() {";"}
getOpStrEq               <- function() {"="}
getStrataSep             <- function() {","}
getWhereSep              <- function() {","}
getModelOpsName          <- function() {"model.options"}
getOldCorrModelName      <- function() {"runcorr"}
getCorrModelName         <- function() {"correlation"}
getGlmModelName          <- function() {"glm"}
getLmModelName           <- function() {"lm"}
getValidModelNames       <- function() {c(getCorrModelName(), getGlmModelName(), getLmModelName())}
getMetabDataOpsName      <- function() {"options"} 
getOutEffectsOpName      <- function() {"output.Effects"}
getOutEffectsOpDefault   <- function() {"exposure"}
getOutEffectsOpVals      <- function() {c(getOutEffectsOpDefault(), "all")}
getOutModSumOpName       <- function() {"output.ModelSummary"}
getOutModSumOpDefault    <- function() {"anova"}
getOutModSumOpVals       <- function() {c(getOutModSumOpDefault(), "all")}
getExpParmsOpName        <- function() {"output.exp_parms"}
getExpParmsOpDefault     <- function() {NULL}
getAddCiOpName           <- function() {"output.ci_alpha"}
getAddCiOpDefault        <- function() {0.95}


# For Effects data frame
getEffectsName             <- function() {"Effects"}
getEffectsTermName         <- function() {"term"}
getEffectsOutcomespecName  <- function() {"outcomespec"}
getEffectsExposurespecName <- function() {"exposurespec"}
getEffectsCorrEstName      <- function() {"corr"}
getEffectsPvalueName       <- function() {"pvalue"}
getEffectsRunName          <- function() {"run"}
getEffectsEstName          <- function() {"estimate"}
getEffectsEstSeName        <- function() {"std.error"}
getEffectsGlmCoefNames     <- function() {c(getEffectsTermName(), getEffectsEstName(), getEffectsEstSeName(), 
                                          "statistic", getEffectsPvalueName())}
getEffectsPcorCoefNames    <- function() {c(getEffectsTermName(), getEffectsCorrEstName(), 
                                            getEffectsPvalueName())}
# Added later to Effects data frame
getEffectsLowerName       <- function() {"estimate.lower"}
getEffectsUpperName       <- function() {"estimate.upper"}



# ModelSummary 
getModelSummaryName        <- function() {"ModelSummary"}
getModelSummaryNobsName    <- function() {"nobs"}
getModelSummaryFunCol      <- function() {"model_function"}
getModelSummaryRunModeName <- function() {"runmode"}
getModelSummaryGlmFitNames <- function() {c("null.deviance", "df.null", "logLik", "AIC", "BIC", "deviance", 
                                            "df.residual", getModelSummaryNobsName())}
getModelSummaryLmFitNames  <- function() {c("r.squared", "adj.r.squared", "sigma", "statistic",
                                            getEffectsPvalueName(), "df", "logLik", "AIC", "BIC", "deviance", 
                                            "df.residual", getModelSummaryNobsName())}

# For input excel file
getModelOptionsIdCol     <- function() {"modelspec"}
getOptionNameCol         <- function() {"option"}
getOptionValueCol        <- function() {"value"}
getMetabSheetName        <- function() {"Metabolites"}
getSubMetabSheetName     <- function() {"SubjectMetabolites"}
getSubDataSheetName      <- function() {"SubjectData"}
getVarMapSheetName       <- function() {"VarMap"}
getModelsSheetName       <- function() {"Models"}
getOptionsSheetName      <- function() {"ModelOptions"}
getGlobalOptionName      <- function() {c("ModelChecks", "ModelOutput")}
getModelFunctionCol   <- function() {"function"}
getReqSheetNames      <- function() {c(getMetabSheetName(), getSubMetabSheetName(), getSubDataSheetName(), getVarMapSheetName())} 
getReqMetabSheetCols  <- function() {c("metabolite_name")}
getVarMapVarRefCol    <- function() {"VARREFERENCE"}
getVarMapCohortVarCol <- function() {"COHORTVARIABLE"}
getVarMapVarTypeCol   <- function() {"VARTYPE"}
getReqVarMapSheetCols <- function() {c(getVarMapVarRefCol(), getVarMapCohortVarCol(), getVarMapVarTypeCol())}
getModelsModelCol     <- function() {"MODEL"}
getModelsOutcomeCol   <- function() {"OUTCOMES"}
getModelsExposureCol  <- function() {"EXPOSURE"}
getModelsAdjCol       <- function() {"ADJUSTMENT"}
getModelsStratCol     <- function() {"STRATIFICATION"}
getModelsWhereCol     <- function() {"WHERE"}
getReqModelsSheetCols <- function() {c(getModelsModelCol(), getModelsOutcomeCol(), getModelsExposureCol(), 
                                       getModelsAdjCol(), getModelsStratCol(), getModelsWhereCol())}
getReqModOpSheetCols  <- function() {c(getModelOptionsIdCol(), getModelFunctionCol(), getOptionNameCol(), getOptionValueCol())}
getVarMapVarTypeCont  <- function() {"continuous"}
getVarMapVarTypeCat   <- function() {"categorical"}
getVarMapVarTypeVals  <- function() {c(getVarMapVarTypeCont(), getVarMapVarTypeCat())}

getVarRef_metabId    <- function() {"metabolite_id"}
getVarRef_subjectId  <- function() {"id"}

# Model options that specify variables
runModel.getOptionsThatAreVars <- function() {c("weights", "offset")}

runModel.testModelString  <- function() {"Begin testing models in Models sheet... \n"}
runModel.testModelString2 <- function() {"Finished testing models in Models sheet. \n"}
