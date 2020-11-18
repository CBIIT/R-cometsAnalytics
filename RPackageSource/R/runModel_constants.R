# Delimiter in output lists of variable names
runModel.getVarSep <- function() {";"}

# For appending stratification columns
runModel.getStrataColName    <- function() {"strata"}
runModel.getStrataNumColName <- function() {"strata.num"}

# For Warnings matrix
runModel.getWarningCol        <- function() {"type"}
runModel.getObjectCol         <- function() {"object"}
runModel.getMessageCol        <- function() {"message"}
runModel.getWarningsListName  <- function() {"Warnings"}
runModel.getUnknownErrorStr   <- function() {"unknown error"}
runModel.getTooFewSubsStr     <- function() {"too few subjects"}
runModel.getStratTooFewSubStr <- function() {"Stratum contains to few subjects"}

# Message when exposure has been removed from design matrix
runModel.getExpRemFromDesign <- function() {"exposure removed from design matrix"}

# For option strings
getNoFamilyValue   <- function() {""}
getOpStrSep        <- function() {";"}
getOpStrEq         <- function() {"="}
getModelOpsName    <- function() {"model.options"}
getCorrModelName   <- function() {"correlation"}
getGlmModelName    <- function() {"glm"}
getLmModelName     <- function() {"lm"}
getValidModelNames <- function() {c(getCorrModelName(), getGlmModelName(), getLmModelName())}

# For Effects data frame
getEffectsName            <- function() {"Effects"}
getEffectsTermName        <- function() {"term"}
getEffectsOutcomespecName <- function() {"outcomespec"}
getEffectsCorrEstName     <- function() {"corr"}

# ModelSummary 
getModelSummaryName       <- function() {"ModelSummary"}

# For input excel file
getModelOptionsIdCol <- function() {"modelname"}
getOptionNameCol     <- function() {"option"}
getOptionValueCol    <- function() {"value"}
getOptionsSheetName  <- function() {"Options"}
getGlobalOptionName  <- function() {"GlobalOption"}
getModelFunctionCol  <- function() {"function"}

getVarRef_metabId    <- function() {"metabolite_id"}
getVarRef_subjectId  <- function() {"id"}

# Model options that specify variables
runModel.getOptionsThatAreVars <- function() {c("weights", "offset")}

