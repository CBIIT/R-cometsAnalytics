# Strings that are visible to the end user of RcometsAnalytics. This file makes it easy to
#   change column names, options, etc.

getVersionNumber     <- function() {"2.9.0.6"}
runmodel.getTimeAttr <- function() {"ptime"}
class_runCorr        <- function() {"runCorr"}
class_runModel       <- function() {"runModel"}
getInputFileOpsName  <- function() {"input.file"}

runCorrDepMsg_batch  <- function() {"runCorr is deprecated, use FUNCTION=correlation instead"}
runCorrDepMsg_inter  <- function() {"runCorr is deprecated, call runModel instead"}

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
runModel.expRefInvalid        <- function() {"exposure reference level is not valid"}


# Message when exposure has been removed from design matrix
runModel.getExpRemFromDesign <- function() {"exposure removed from design matrix"}

# Message when exposure could not be estimated from the model
runModel.getExpNotEstimated <- function() {"exposure could not be estimated"}

# For option strings
getNoFamilyValue          <- function() {""}
getOpStrSep               <- function() {";"}
getOpStrEq                <- function() {"="}
getStrataSep              <- function() {","}
getWhereSep               <- function() {","}
getOpModelName            <- function() {"model"}
getModelOpsName           <- function() {"model.options"}
getOldCorrModelName       <- function() {"runcorr"}
getCorrModelName          <- function() {"correlation"}
getGlmModelName           <- function() {"glm"}
getLmModelName            <- function() {"lm"}
getCoxphModelName         <- function() {"coxph"}
getClogitModelName        <- function() {"clogit"}
getValidModelNames        <- function() {c(getCorrModelName(), getGlmModelName(), getLmModelName(),
                                           getCoxphModelName(), getClogitModelName())}
getMetabDataOpsName       <- function() {"options"} 
getOutEffectsOpName       <- function() {"output.Effects"}
getOutEffectsOpDefault    <- function() {"exposure"}
getOutEffectsOpVals       <- function() {c(getOutEffectsOpDefault(), "all")}
getOutModSumOpName        <- function() {"output.ModelSummary"}
getOutModSumOpDefault     <- function() {"anova"}
getOutModSumOpVals        <- function() {c(getOutModSumOpDefault(), "all")}
getExpParmsOpName         <- function() {"output.exp_parms"}
getExpParmsOpDefault      <- function() {NULL}
getAddCiOpName            <- function() {"output.ci_alpha"}
getAddCiOpDefault         <- function() {0.95}
getAddMetabColsOpName     <- function() {"output.metab.cols"}
getAddMetabColsDefault    <- function() {"metabolite_name"}
getAddMetabColsSep        <- function() {","}
getOutTypeOpName          <- function() {"output.type"}
getOutTypeOpRda           <- function() {"rda"}
getOutTypeOpExcel         <- function() {"xlsx"}
getOutTypeOpVals          <- function() {c(getOutTypeOpRda(), getOutTypeOpExcel())}
getOutTypeOpDefault       <- function() {"xlsx"} 
getOutCommonColsOpName    <- function() {"output.common.cols"}
getOutCommonColsOpDefault <- function() {0} # 0 or 1
getOutMergeOpName         <- function() {"output.merge"}
getOutMergeOpNone         <- function() {"none"}
getOutMergeOpAll          <- function() {"all"}
getOutMergeOpByModelFunc  <- function() {"by_function"}
getOutMergeOpByModelSpec  <- function() {"by_model_type"}
getOutMergeOpVals         <- function() {c(getOutMergeOpAll(), getOutMergeOpNone(), 
                                           getOutMergeOpByModelFunc(), getOutMergeOpByModelSpec())}
getOutMergeOpDefault      <- function() {getOutMergeOpNone()} 
getOutMergeAllStr         <- function() {"all_models"}
getMaxNpairwiseOpName     <- function() {"max.npairwise"}
getMaxNpairwiseOpDefault  <- function() {1000}

getOutfileSpCharSep       <- function() {"_"}
getOutfileCohortSep       <- function() {"__"}
getOutfileModelPart       <- function() {1}
getOutfileCohortPart      <- function() {2}


# For Effects data frame
getEffectsName             <- function() {"Effects"}
getEffectsTermName         <- function() {"term"}
getEffectsOutcomespecName  <- function() {"outcomespec"}
getEffectsExposurespecName <- function() {"exposurespec"}
getEffectsCorrEstOldName   <- function() {"corr"}
getEffectsCorrEstName      <- function() {"estimate"}
getEffectsPvalueName       <- function() {"pvalue"}
getEffectsRunName          <- function() {"run"}
getEffectsEstName          <- function() {"estimate"}
getEffectsEstSeName        <- function() {"std.error"}
getEffectsStatName         <- function() {"statistic"}
getEffectsGlmCoefNames     <- function() {c(getEffectsTermName(), getEffectsEstName(), getEffectsEstSeName(), 
                                            getEffectsStatName() , getEffectsPvalueName())}
getEffectsPcorCoefNames    <- function() {c(getEffectsTermName(), getEffectsCorrEstName(), 
                                            getEffectsPvalueName())}
# Added later to Effects data frame
getEffectsLowerName       <- function() {"estimate.lower"}
getEffectsUpperName       <- function() {"estimate.upper"}
# For exponeniated estimates
getEffectsExpEstName      <- function() {"exp.estimate"}
getEffectsExpEstSeName    <- function() {"exp.std.error"}
getEffectsExpLowerName    <- function() {"exp.estimate.lower"}
getEffectsExpUpperName    <- function() {"exp.estimate.upper"}

# ModelSummary 
getModelSummaryName        <- function() {"ModelSummary"}
getModelSummaryNobsName    <- function() {"nobs"}
getModelSummaryFunCol      <- function() {"model_function"}
getModelSummaryModelCol    <- function() {"model"}
getModelSummaryModelNumCol <- function() {"model_number"}
getModelSummaryRunModeName <- function() {"runmode"}
getModelSummaryGlmFitNames <- function() {c("null.deviance", "df.null", "logLik", "AIC", "BIC", "deviance", 
                                            "df.residual", getModelSummaryNobsName())}
getModelSummaryLmFitNames  <- function() {c("r.squared", "adj.r.squared", "sigma", "statistic",
                                            getEffectsPvalueName(), "df", "logLik", "AIC", "BIC", "deviance", 
                                            "df.residual", getModelSummaryNobsName())}
getModelSummaryCoxphFitNames  <- function() {c("n", "nevent", "statistic.log", "p.value.log", "statistic.sc", 
                                               "p.value.sc", "statistic.wald", "p.value.wald", "statistic.robust",
                                               "p.value.robust", "r.squared", "r.squared.max", "concordance",
                                               "std.error.concordance", "logLik", "AIC", "BIC", getModelSummaryNobsName())}
getModelSummaryClogitFitNames <- function() {getModelSummaryCoxphFitNames()}
getModelSummaryOutUidCol  <- function() {"outcome_uid"}
getModelSummaryExpUidCol  <- function() {"exposure_uid"}
getModelSummaryCovStrSep1 <- function() {","}
getModelSummaryCovStrSep2 <- function() {"|"}
getModelSummaryCovStrCol  <- function() {"exposure.covariances"}


# For input excel file
getModelOptionsIdCol     <- function() {"model_type"}
getOptionNameCol         <- function() {"option"}
getOptionValueCol        <- function() {"value"}
getMetabSheetName        <- function() {"Metabolites"}
getSubMetabSheetName     <- function() {"SubjectMetabolites"}
getSubDataSheetName      <- function() {"SubjectData"}
getVarMapSheetName       <- function() {"VarMap"}
getModelsSheetName       <- function() {"Models"}
getOptionsSheetName      <- function() {"Model_Types"}
getGlobalOptionName      <- function() {c("ModelChecks", "ModelOutput")}
getModelFunctionCol      <- function() {"function"}
getReqSheetNames         <- function() {c(getMetabSheetName(), getSubMetabSheetName(), getSubDataSheetName(), getVarMapSheetName())} 
getReqMetabSheetCols     <- function() {c("metabolite_name")}
getVarMapVarRefCol       <- function() {"VARREFERENCE"}
getVarMapCohortVarCol    <- function() {"COHORTVARIABLE"}
getVarMapVarDefCol       <- function() {"VARDEFINITION"}
getVarMapVarTypeCol      <- function() {"VARTYPE"}
getVarMapAccValsCol      <- function() {"ACCEPTED_VALUES"}
getVarMapAccValsSep      <- function() {","}
getReqVarMapSheetCols    <- function() {c(getVarMapVarRefCol(), getVarMapCohortVarCol(), 
                                          getVarMapVarTypeCol())}
getModelsModelCol        <- function() {"MODEL"}
getModelsOutcomeCol      <- function() {"OUTCOMES"}
getModelsExposureCol     <- function() {"EXPOSURE"}
getModelsAdjCol          <- function() {"ADJUSTMENT"}
getModelsStratCol        <- function() {"STRATIFICATION"}
getModelsWhereCol        <- function() {"WHERE"}
getModelsTimeCol         <- function() {"TIME"}
getModelsGroupCol        <- function() {"GROUP"}
getModelsExpRefCol       <- function() {"EXPOSURE_REFERENCE"}
getReqModelsSheetCols    <- function() {c(getModelsModelCol(), getModelsOutcomeCol(), getModelsExposureCol(), 
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

# options for RaMP
getRampPvalOpName              <- function() {"chemEnrich.adjPvalue"}
getRampPvalOpDefault           <- function() {0.05}
getRampPvalAdjMethod           <- function() {"BH"}
getRampCallChemEnrichOpName    <- function() {"chemEnrich"}
getRampCallChemEnrichOpDefault <- function() {0}
getRampOpName                  <- function() {"ramp.options"} # Name of list in the main options list
getRampChemEnrichDfName        <- function() {"ChemEnrich"} # Name of the data frame in return list from runModel

getTable1DfName                <- function() {"Table1"} 

# Info table
getInfoTableDfName             <- function() {"Info"}
getInfoTableNameCol            <- function() {"name"}
getInfoTableValueCol           <- function() {"value"}
getInfoTable2plusVars          <- function() {"*"}
getInfoTableOutcomeName        <- function() {"outcome"}
getInfoTableExposureName       <- function() {"exposure"}
getInfoTableStrataName         <- function() {"strata"}
getInfoTableModelFuncName      <- function() {"op$model"}
getInfoTableFamilyName         <- function() {"model.options$family"}
getInfoTableCohortName         <- function() {"cohort"}
getInfoTableModelNmName        <- function() {"model name"}
getInfoTableFileNmName         <- function() {"input file"}
getInfoTableExpRefName         <- function() {"exposurerefs"}
getAllRetSheetNames            <- function() {c(getModelSummaryName(), getEffectsName(),
                                                runModel.getWarningsListName(),
                                                getRampChemEnrichDfName(), getTable1DfName(),
                                                getInfoTableDfName())
                                             }

# Meta analysis
getMetaModelsSheetName            <- function() {"Meta_Models"}
getMetaModelTypeSheetName         <- function() {"Meta_Types"}
getMetaOpFileModelsCol            <- function() {"MODEL"}
getMetaOpFileModelTypeCol         <- function() {"META_TYPE"}
getMetaGlobalOptionName           <- function() {"GLOBAL"}
metaOp_methodName                 <- function() {"method"}
metaOp_methodDefault              <- function() {"fixed"}
metaOp_methodVals                 <- function() {c(metaOp_methodDefault(), "random")}
metaOp_minNcohortName             <- function() {"min.n.cohort"}
metaOp_minNcohortDefault          <- function() {2}
metaOp_cohortMinSubs              <- function() {"min.nsub.cohort"}
metaOp_cohortMinSubsDefault       <- function() {25}
metaOp_totalMinSubs               <- function() {"min.nsub.total"}
metaOp_totalMinSubsDefault        <- function() {50}
metaOp_save.mem                   <- function() {"save.mem"}
metaOp_save.memDefault            <- function() {1}
metaOp_cohorts.include            <- function() {"cohorts.include"}
metaOp_cohorts.exclude            <- function() {"cohorts.exclude"}
metaOp_strataToExcludeFromHetTest <- function() {"strata.exclude.het.test"}
metaOp_check.consistency          <- function() {"check.consistency"}
metaOp_check.consistencyDefault   <- function() {TRUE}
metaOp_oneModelCheck              <- function() {"oneModelCheck"}
metaOp_oneModelCheckDefault       <- function() {TRUE}
metaOp_dups.allow                 <- function() {"dups.allow"}
metaOp_dups.allowDefault          <- function() {FALSE}
metaOp_stopOnFileError            <- function() {"stopOnFileError"}
metaOp_stopOnFileErrorDefault     <- function() {TRUE}
metaOp_models                     <- function() {"models"}
metaDataOp_renameLevels           <- function() {"rename.levels"}
metaDataOp_renameLevels.var       <- function() {"var"}
metaDataOp_renameLevels.old       <- function() {"old"}
metaDataOp_renameLevels.new       <- function() {"new"}


metaModelNameNumberSep         <- function() {" "}
metaModelNameDefault           <- function() {"NA"}
metaRetListResultsTable        <- function() {"Results"}
getHarmMetabIdCol              <- function() {"metabolite_name"}
getMetaBetaCol                 <- function() {"meta.estimate"}
getMetaBetaSeCol               <- function() {"meta.std.error"}
getMetaPvalueCol               <- function() {"meta.pvalue"}
getMetaFixedBetaCol            <- function() {"fixed.estimate"}
getMetaFixedBetaSeCol          <- function() {"fixed.std.error"}
getMetaFixedPvalueCol          <- function() {"fixed.pvalue"}
getMetaFixedBetaLCol           <- function() {"fixed.estimate.L"}
getMetaFixedBetaUCol           <- function() {"fixed.estimate.U"}
getMetaRandomBetaCol           <- function() {"random.estimate"}
getMetaRandomBetaSeCol         <- function() {"random.std.error"}
getMetaRandomPvalueCol         <- function() {"random.pvalue"}
getMetaRandomBetaLCol          <- function() {"random.estimate.L"}
getMetaRandomBetaUCol          <- function() {"random.estimate.U"}
getMetaNcohortCol              <- function() {"n.cohort"}
getMetaNsubCol                 <- function() {"n.sub"}
getMetaHetPvalueCol            <- function() {"het.pvalue"}
getMetaStrataHetFixedPCol      <- function() {"strata.fixed.het.pvalue"}
getMetaStrataHetFixedDfCol     <- function() {"strata.fixed.het.df"}
getMetaStrataHetRandomPCol     <- function() {"strata.random.het.pvalue"}
getMetaStrataHetRandomDfCol    <- function() {"strata.random.het.df"}
getMetaMessageCol              <- function() {"message"}
getMetaDirectionCol            <- function() {"direction"}
getMetaIdCol                   <- function() {"id"}
getMetaEpsForCorr              <- function() {1e-6}
getMetaLabelForOutFiles        <- function() {"meta"}
getMetaValidExt                <- function() {c("xlsx", "rda", "zip", "tar", "tar.gz")}
getMetaIdNamesSep              <- function() {":::"}
getMetaIdNamesUidCols          <- function() {c(getModelSummaryModelCol(), getModelSummaryOutUidCol(), 
                                                getModelSummaryExpUidCol(), getEffectsTermName())}
getMetaIdNamesStratCols        <- function() {c(runModel.getStrataColName(), runModel.getStrataNumColName())}
getMetaStratSep                <- function() {"."}


# For merging multiple files from same cohort
mrf_precedenceOpName           <- function() {"precedence"}
mrf_precedenceNobs             <- function() {"nobs"}
mrf_precedenceData             <- function() {"data"}
mrf_precedenceDefault          <- function() {mrf_precedenceNobs()}
mrf_precedenceValid            <- function() {c(mrf_precedenceNobs(), mrf_precedenceData())}
mrf_consistencyOpName          <- function() {"check.consistency"}
mrf_consistencyDefault         <- function() {TRUE}
mrf_InfoMrgSep                 <- function() {"||"}
mrf_FileCol                    <- function() {"file"}
mrf_runSep                     <- function() {"_"}

# For non-COMETS output
dfToC_se.col                   <- function() {"se.col"}
dfToC_est.col                  <- function() {"estimate.col"}
dfToC_nobs.col                 <- function() {"nobs.col"}
dfToC_nobs                     <- function() {"nobs"}
dfToC_outcome.col              <- function() {"outcome.col"}
dfToC_outcome.name             <- function() {"outcome.name"}
dfToC_exposure.col             <- function() {"exposure.col"}
dfToC_exposure.name            <- function() {"exposure.name"}
dfToC_stratavar.col            <- function() {"stratavar.col"}
dfToC_strata.col               <- function() {"strata.col"}
dfToC_strata.name              <- function() {"strata.name"}
dfToC_strata.value             <- function() {"strata.value"}
dfToC_model.col                <- function() {"model.col"}
dfToC_model.name               <- function() {"model.name"}
dfToC_change.col.values        <- function() {"change.col.values"}
dfToC_change.col               <- function() {"col"}
dfToC_change.old               <- function() {"old"}
dfToC_change.new               <- function() {"new"}
dfToC_cohort                   <- function() {"cohort"}
dfToC_file                     <- function() {"file"}
dfToC_fileSep                  <- function() {"sep"}
dfToC_where                    <- function() {"where"}
dfToC_newRef                   <- function() {"new.ref.value"}

cometsReqOutSheetNames         <- function() {c(getEffectsName(), getModelSummaryName(), getInfoTableDfName())}
filelistSheetOp                <- function() {"sheet"}
