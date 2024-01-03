
# rci = readCOMETSinput
# res = readExcelSheet
# infile.basicSheetCheck, etc
# gmd = getModelData
# mod = runModel
# meta
# meta options file
# for general arguments


msg_rci_1               <- function() "ERROR: file must be a character string giving the complete path to the Excel file."
msg_rci_2               <- function(x) paste0("ERROR: input Excel file ", x, " does not exist.")
msg_rci_3               <- function() "ERROR: check that the input file is an Excel file"
msg_rci_4               <- function(x) paste0("ERROR: the input Excel file is missing all required sheets: ", x)  
msg_rci_5               <- function(x) paste0("NOTE: the column ", x[1], " was not found in the ", x[2], " sheet. ", 
                                              "Assuming older version of Excel file.\n")
msg_rci_6               <- function(x) paste0("NOTE: the ", x, " sheet was not found in input excel file.",
                                              " Assuming RcometsAnalytics will be run in interactive mode.\n")
msg_rci_7               <- function() "Running Integrity Check...\n"
msg_rci_8               <- function() "ERROR in Excel file. See above ERROR message(s)."
msg_rci_9               <- function() "Passed all integrity checks, analyses can proceed. If you are part of COMETS, please download metabolite list below and submit to the COMETS harmonization group."
msg_rci_10              <- function(x) paste0("ERROR: matching variable name(s) '", x[1], 
                                              "' to ", x[2], " column")
msg_rci_11              <- function(x) paste0("ERROR: ", x[1], " column needed in ", toupper(getModelsSheetName()),
                                              " sheet for categorical exposure variables")
msg_rci_12              <- function(x) paste0("ERROR with ", x[1], 
                                              " column for model '", x[2], "',",
                                              " length(", x[3], ")!=length(", x[4], ")")
msg_rci_13              <- function(x) paste0("ERROR: ", x[1], "=", x[2], " for variable ", x[3], 
                                              " in model '", x[4], "'", " must be listed in the ",
                                              toupper(getVarMapAccValsCol()), " column")
msg_rci_14              <- function(x) paste0("ERROR with ", x[1], " of variable ", x[2], 
                                              " in ", x[3], " sheet")
msg_rci_15              <- function(x) paste0("ERROR: the value(s) '", x[1], "' of variable ", x[2], 
                                              " are not the ACCEPTED VALUES ", x[3],
                                              " as defined in the ", toupper(getVarMapSheetName()), " sheet")
msg_rci_16              <- function(x) paste0("ERROR: variable ", x[1], " contains values that are not",
                                              " the ACCEPTED VALUES ", x[2],
                                              " as defined in the ", toupper(getVarMapSheetName()), " sheet")
msg_rci_17              <- function(x) paste0("ERROR: column ", x[1], " not found in data\n")
msg_rci_18              <- function() "No filtering was performed because 'where' parameter is NULL"
msg_rci_19              <- function() "No filtering was performed because 'where' parameter contains no filters"
msg_rci_20              <- function() "Make sure your 'where' filters contain logicals (>, >=, <, <=, =, !=)"
msg_rci_21              <- function() paste0("ERRORS were produced for some models in the ", getModelsSheetName(),
                                             " sheet. See messages above. \n")
msg_rci_22              <- function() "Something went wrong with the harmonization"
msg_rci_23              <- function(x) paste0("The categorical variable '", x[1], "' was not found in the data")
msg_rci_24              <- function(x) paste0("ERROR: categorical variable '", x[1], "' has value(s) '",
                                              x[2], "' not defined in the ", x[3], 
                                              " column of the ", x[4], " sheet.")
msg_rci_25              <- function(x) paste0("Possible problem with the ", getVarMapVarDefCol(),
                                              " of the ", getVarMapSheetName(), 
                                              " for variable ", x[1], "\n")


msg_res_1               <- function(x) paste0("ERROR: the input Excel file is missing the ", x, " sheet.\n")
msg_res_2               <- function(x) paste0("ERROR: multiple sheets in the input Excel file map to the ", x, " sheet.\n")
msg_res_3               <- function(x) paste0("ERROR attempting to read the ", x, " sheet.\n")
msg_res_4               <- function(x) paste0("ERROR the ", x, " sheet has no rows.\n")
msg_res_5               <- function(x) paste0("ERROR the ", x, " sheet is empty.\n")
msg_res_6               <- function(x) paste0("POSSIBLE ERROR: the ", x[1], " sheet may contain duplicated column(s) ", x[2], ".\n")
msg_res_7               <- function(x) paste0(x[1], " sheet is read in.\n")
msg_res_8               <- function(x) paste0("There are ", x[1], " categorical variables.\n")

msg_ibsc_1              <- function(x) paste0("ERROR in the ", x[1], " sheet. The column(s) (", x[2], ") are missing.\n")
msg_ibsc_2              <- function(x) paste0("ERROR in the ", x[1], " sheet. There must be at least ", x[2], " columns.\n")
msg_ibsc_3              <- function(x) paste0("ERROR in the ", x[1], " sheet. A row with ", 
                                              x[2], "=", x[3], " was not found.\n")
msg_ibsc_4              <- function(x) paste0(x[1], " on row(s) ", x[2], " of the ", x[3], " sheet. ",
                                              x[4], " contains missing values.\n")
msg_ibsc_5              <- function(x) paste0("ERROR on row(s) ", x[1], " of the ", 
                                              x[2], " sheet. ", x[3], " must be one of ", x[4], ".\n")
msg_ibsc_6              <- function(x) paste0(x[1], " in the ", x[2], " sheet. ", 
                                              x[3], "=", x[4], " are duplicated.\n")
msg_ibsc_7              <- function(x) paste0("ERROR in the ", x[1], " sheet. The ", x[2],  " column(s) ", 
                                              x[3], " contain invalid values ", x[4], ".\n")
msg_ibsc_8              <- function(x) paste0("ERROR in the ", x[1], " sheet. The ", x[2],  " column(s) ", 
                                              x[3], " are not numeric.\n")
msg_ibsc_9              <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet. ", x[3], 
                                              "=(", x[4], ") is not valid.\n")
msg_ibsc_10             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet. ", x[3], "=", x[4], 
                                              " not found in the ", x[5], " sheet.\n")
msg_ibsc_11             <- function(x) paste0("ERROR: the ", x[1], " and ", x[2], 
                                              " sheets have no subject ids in common.\n")
msg_ibsc_12             <- function(x) paste0("WARNING: there were ", x[1], " subject ids from the ", x[2], " sheet",
                                              " that were not found in the ", x[3], " sheet.\n")
msg_ibsc_13             <- function(x) paste0("ERROR for model on row ", x[1], " of the ", x[2], " sheet: \n", x[3], "\n")
msg_ibsc_14             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet. ",
                                              x[3], "=", x[4], " is a reserved word.\n")  
msg_ibsc_15             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet.",
                                              x[3], "=", x[4], " is invalid.\n")  
msg_ibsc_16             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet.",
                                              " The column ", x[3], " should only contain ",
                                              "at most ", x[4], " variable(s).\n")
msg_ibsc_17             <- function(x)  paste0("ERROR on row ", x[1], " of the ", x[2], " sheet. ",
                                               x[3], "=", x[4], " are not listed in the ",
                                               toupper(getVarMapVarRefCol()), " column of the ", getVarMapSheetName(), " sheet.\n")
msg_ibsc_18             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet. ",
                                              x[3], "=", x[4], " do not exist in the data! Check the naming!\n")
msg_ibsc_19             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet.",
                                              " The ", x[3], " column must contain variable names.\n")
msg_ibsc_20             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet.",
                                              " The ", x[3], " column cannot have missing values.\n")  
msg_ibsc_21             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet.",
                                              " The variable(s) ", x[3], " appear on both the ",
                                              x[4], " and ", x[5], " columns. This is not allowed.\n")
msg_ibsc_22             <- function(x) paste0("ERROR on row ", x[1], " of the ", x[2], " sheet.",
                                              " The ", x[3], " column must contain one of the", 
                                              " following operators (<, >, =, >=, <=, !=)\n")  

msg_gmd_1               <- function() "ERROR: please make sure that you have identified one or more exposure variables"
msg_gmd_2               <- function(x) paste0("ERROR: check that user-input variable(s) ", x[1], 
                                              " exist (should match VARREFERENCE column in VarMap Sheet)") 
msg_gmd_3               <- function() "ERROR: define an exposure reference for each exposure"
msg_gmd_4               <- function() paste0("ERROR: modelspec is set to '", getMode_batch(), 
                                             "' yet model label (modlabel) is empty.  Please set modlabel.")
msg_gmd_5               <- function(x) paste0("ERROR: the model name '", x[1], "' does not exist in the input Excel file. Please check your Models sheet.")
msg_gmd_6               <- function(x) paste0("ERROR: the model name '", x[1], "' corresponds to more than one row in the input Excel file. Please check your Models sheet.")
msg_gmd_7               <- function(x) paste0("ERROR: check that user-input variable(s) ", x[1],  
                                              " exist (should match VARREFERENCE column in VarMap Sheet)")
msg_gmd_8               <- function(x) paste0("ERROR applying WHERE: ", x[1])
msg_gmd_9               <- function(x) paste0("Filtering subjects according to the rule(s) ", 
                                              paste0(x[1], collapse=" AND "), ". ", 
                                              x[2], " of ", x[3]," are retained. \n")
msg_gmd_10              <- function() "Remember that in batch mode, multiple WHERE conditions should be separated by a comma.\n"
msg_gmd_11              <- function() "For example: x = 1, y > 3 \n"
msg_gmd_12              <- function() "ERROR: no rows left in data after applying the WHERE condition"
msg_gmd_13              <- function() "ERROR: too few samples for this model, so the model will not be run."
msg_gmd_14              <- function() "ERROR: no model function (e.g. glm, lm, etc.) specified"
msg_gmd_15              <- function() "ERROR: more than one model function specified while only one function can be used per model"
msg_gmd_16              <- function() paste0("ERROR: missing option names in ", getOptionsSheetName(), " sheet")
msg_gmd_17              <- function(x) paste0("ERROR: the options ", x[1], " appear more than once in the ",
                                              getOptionsSheetName(), " sheet")
msg_gmd_18              <- function(x) paste0("ERROR: ", x[1], " = ", x[2], " not found in ", 
                                              getOptionsSheetName(), " sheet")
msg_gmd_19              <- function() "Check the WHERE condition.\n"

msg_mod_1               <- function() "ERROR: cohortLabel must be a string"
msg_mod_2               <- function() paste0("options for runModel were obtained from the excel options sheet,",
                                             " not from the op list passed in")
msg_mod_3               <- function(x) paste("ERROR: the number of models exceeds the max number allowed ",
                                             "set by option '", x[1], "'", sep="")
msg_mod_4               <- function(x) paste0("ERROR: the stratification variable(s) ",
                                              x[1], " contains more than ", x[2], 
                                              " unique values. The max number of stratification levels is defined by the option max.nstrata.",
                                              " Please check your stratification variable(s)")
msg_mod_5               <- function(x) paste0("ERROR: ", x[1]," has less than ", x[2], " observations and will not be run.")
msg_mod_6               <- function(x) paste0("ERROR: ", x[1]," has all exposure variables removed (e.g., if the exposure variables had all missing, or constant).")
msg_mod_7               <- function(x) paste0("ERROR: ", x[1]," has all outcome variables removed (e.g., if the outcome variables had all missing, or constant).")
msg_mod_8               <- function(x) paste0("ERROR: ", x[1], "$gdta must be a data frame")
msg_mod_9               <- function(x) paste0("ERROR: ", x[1], "$modelspec must be ", x[2], " or ", x[3])
msg_mod_10              <- function(x) paste0("ERROR: all of the ", x[1], " are also ", x[2], "!! ",
                                              "Please make sure ", x[1], " are not ", x[2], ".")
msg_mod_11              <- function(x) paste0("Some of the ", x[1], " are also ", x[2], "!!\n",
                                              "The variable(s) ", x[3],
                                              " will be dropped from the list of ", x[1], ".")
msg_mod_12              <- function(x) paste0("ERROR: the variable(s) ", x[1], " are both ", x[2],
                                              " and ", x[3], " variables!  This is not allowed.")
msg_mod_13              <- function() "ERROR: outcome variables must be specified"
msg_mod_14              <- function() "ERROR: exposure variables must be specified"
msg_mod_15              <- function() "ERROR: only one outcome variable and it is also the exposure variable"
msg_mod_16              <- function(x) paste("The variable(s) ", x[1], " have been removed from ", 
                                             x[2], " because of: ", x[3], sep="")
msg_mod_17              <- function() runCorrDepMsg_batch()
msg_mod_18              <- function() runCorrDepMsg_inter()
msg_mod_19              <- function(x) paste0("ERROR: no time variables specified for ", x[1], " model.")
msg_mod_20              <- function(x) paste0("ERROR: only one or two time variables can be specified for ", x[1], " model.")
msg_mod_21              <- function(x) paste0("ERROR: no group variable specified for ", x[1], " model.")
msg_mod_22              <- function(x) paste0("ERROR: only one group variable can be specified for ", x[1], " model.")
msg_mod_23              <- function(x) paste0("ERROR: ", x[1], " is not a valid option")
msg_mod_24              <- function(x) paste0("WARNING: ", x[1], " subject(s) have been removed due to missing values")
msg_mod_25              <- function() paste0("Duplicated harmonized metabolite ids")
msg_mod_26              <- function(x) paste0("Duplicated harmonized metabolite ids in cohort ", x[1])

msg_meta_1              <- function(x) paste0("ERROR: number of files provided (", x[1], ")",
                                              " is less than required. In this analysis, then number of files ", 
                                              "required is ", metaOp_minNcohortName())
msg_meta_2              <- function() paste0("ERROR: number of files to process is less than ", metaOp_minNcohortName())
msg_meta_3              <- function() "ERROR: different models were run among the cohorts and cannot be used for meta-analysis"
msg_meta_4              <- function() "ERROR: different model functions were run among the cohorts and cannot be used for meta-analysis"
msg_meta_5              <- function(x) paste0("ERROR: the cohorts '", x[1], "' contributed more than one file.",
                                              " If a single cohort has multiple projects, they should each have a unique name." )
msg_meta_6              <- function() "ERROR: all cohorts must have the same exposure variable for meta-analysis"
msg_meta_7              <- function() "ERROR: all cohorts must have the same outcome variable for meta-analysis"
msg_meta_8              <- function() "ERROR: all cohorts must have the same exposure reference for meta-analysis"
msg_meta_9              <- function() "ERROR with Effects/ModelSummary"
msg_meta_10             <- function() "ERROR: all metabolites have been filtered out"
msg_meta_11             <- function() "ERROR: there are no metabolites left (e.g., due to filtering) to perform meta-analysis."
msg_meta_12             <- function(x) paste0("ERROR: ", x[1], " table not found or is empty")
msg_meta_13             <- function() "ERROR: name of cohort not found"
msg_meta_14             <- function() "ERROR: name of model not found"
msg_meta_15             <- function() "ERROR: model names are not unique"
msg_meta_16             <- function() "ERROR: too few cohorts left after exclusions"
msg_meta_17             <- function() paste0("ERROR: cohort names are not unique.",
                                             " If this is intended, set option ", metaOp_mergeCohortFiles(),
                                             " to TRUE.\n")
msg_meta_18             <- function(x) paste0("ERROR: directory ", x[1], " not created")
msg_meta_19             <- function(x) paste0(x[1], "='", x[2], "' not found in ", x[3], " data frame")
msg_meta_20             <- function() "ERROR in meta_extractAllFiles"
msg_meta_21             <- function() "ERROR: all files are required to have a correct file extension, such as .xlsx, .rda, .zip, .tar, .tar.gz"
msg_meta_22             <- function() "ERROR: no files found in the directories"
msg_meta_23             <- function() "ERROR: all files must follow a valid naming convention, e.g. 'modelName__cohortName__date.xlsx'"
msg_meta_24             <- function() "ERROR: specified model name(s) do not match do not match any of the results file names."
msg_meta_25             <- function() "ERROR: data contains no rows or no columns"
msg_meta_26             <- function() paste("ERROR: set option ", dfToC_cohort(), " to specify the cohort name")
msg_meta_27             <- function(x) paste0("ERROR: column ", x[1], " is missing. Set option ", 
                                              x[2], " or ", x[3])
msg_meta_28             <- function(x) paste0("ERROR: column ", x[1], " is missing. Set option ", x[2])
msg_meta_29             <- function(x) paste0("ERROR: column ", x[1], " not found in data")
msg_meta_30             <- function(x) paste0(x[1], "='", x[2], "' not found in the data")
msg_meta_31             <- function() "ERROR: file type is not expected. Please re-run model using COMETS version 3.0 or later."
msg_meta_32             <- function() "ERROR: the above file(s) do not have the correct file extension or are not valid directories"
msg_meta_33             <- function() "ERROR: the above file(s) do not have the correct file extension"
msg_meta_34             <- function() "ERROR: Data does not contain unique results"
msg_meta_35             <- function() "ERROR matching rows between Effects and ModelSummary data frames"
msg_meta_36             <- function() "ERROR with ModelSummary"
msg_meta_37             <- function(x) paste0("ERROR with file ", x[1], "\n")
msg_meta_38             <- function() "ERROR loading all files"
msg_meta_39             <- function() "ERROR in meta_parseFileNames"
msg_meta_40             <- function(x) paste0("ERROR: model '", x[1], "' failed with message ", x[2], "\n")
msg_meta_41             <- function(x) paste0("No levels to change for variable ", x[1])
msg_meta_42             <- function(x) paste0("The values ", x[1], " for column ", x[2], " were not changed. Check option ", x[3])
msg_meta_43             <- function(x) paste0(x[1], " file(s) are left after applying option ", x[2], "\n")
msg_meta_44             <- function(x) paste0("No files found for model ", x[1], "\n")
msg_meta_45             <- function(x) paste0("The option ", x[1], " removed all strata.",
                                              " Stratification het test will not be performed.\n")
msg_meta_46             <- function(x) paste0("Begin meta-analysis for model ", x[1], "\n")
msg_meta_47             <- function() "ERROR: no metabolites remain after removing duplicates"
msg_meta_48             <- function(x) paste0("The metabolites ", x[1], " are duplicates in cohort ", x[2])
msg_meta_49             <- function() "ERROR while processing duplicate metabolites\n" 
msg_meta_50             <- function(x) paste0("NOTE: files for cohort ", x[1], " were merged into ", x[2], "\n")

msg_metaop_1            <- function() paste0("ERROR: missing value(s) in the ", getOptionNameCol(), 
                                             " of the ", getMetaModelTypeSheetName(), " sheet")
msg_metaop_2            <- function(x) paste0("ERROR: the options ", x[1], " appear more than once in the ",
                                              getMetaModelTypeSheetName(), " sheet")
msg_metaop_3            <- function(x) paste0("ERROR: ", x[1], " must be an Excel file")
msg_metaop_4            <- function(x) paste0("ERROR: check that ", x[1], " is an Excel file")
msg_metaop_5            <- function(x) paste0(x[1], " = ", x[2], " is not correctly specified")


msg_arg_len0            <- function(x) paste0("ERROR: ", x[1], " has length 0")
msg_arg_lenN            <- function(x) paste0("ERROR: ", x[1], " must have length ", x[2])
msg_arg_maxlen          <- function(x) paste0("ERROR: ", x[1], " must have length <= ", x[2])
msg_arg_notList         <- function(x) paste0("ERROR: ", x[1], " must be a list")
msg_arg_listNotValid    <- function(x) paste0("ERROR: the above ", x[1], " list is not valid")
msg_arg_notVec          <- function(x) paste0("ERROR: ", x[1], " must be a vector")
msg_arg_notCharVec      <- function(x) paste0("ERROR: ", x[1], " must be a character vector")
msg_arg_notNumVec       <- function(x) paste0("ERROR: ", x[1], " must be a numeric vector")
msg_arg_notNumMat       <- function(x) paste0("ERROR: ", x[1], " must be a numeric matrix")
msg_arg_notNumMatVec    <- function(x) paste0("ERROR: ", x[1], " must be a numeric matrix or vector")
msg_arg_notString       <- function(x) paste0("ERROR: ", x[1], " must be a string")
msg_arg_notTF           <- function(x) paste0("ERROR: ", x[1], " must be TRUE or FALSE")
msg_arg_objNotValid     <- function(x) paste0("ERROR: ", x[1], " must be one of ", x[2])
msg_arg_objNotInRange   <- function(x) paste0("ERROR: ", x[1], " must be ", x[2],  
                                              x[3], " and ", x[4], x[5], sep="")
msg_arg_objNotGEQ       <- function(x) paste0("ERROR: ", x[1], " must be >= ", x[2])
msg_arg_objNotIn        <- function(x) paste0("ERROR: the object(s) ", x[1], " are not in ", x[2])
msg_arg_0rows           <- function(x) paste0("ERROR: ", x[1], " has 0 rows")
msg_arg_0cols           <- function(x) paste0("ERROR: ", x[1], " has 0 columns")
msg_arg_Nrows           <- function(x) paste0("ERROR: ", x[1], " must have ", x[2], " rows")
msg_arg_Ncols           <- function(x) paste0("ERROR: ", x[1], " must have ", x[2], " columns")
msg_arg_noFiles         <- function()  "ERROR: No files specified"
msg_arg_filesDNE        <- function()  "ERROR: the above file(s) do not exist"
msg_arg_outfile         <- function(x) paste0("ERROR: ", x[1], " must be an output file name")
msg_arg_fileExtNotValid <- function(x) paste0("ERROR: ", x[1], " must have a valid file extension (", x[2], ")")
msg_arg_fileExtNotxlsx  <- function(x) paste0("ERROR: ", x[1], " must have a .xlsx extension")
msg_arg_fileExtUnknown  <- function(x) paste0("ERROR: file extension could not be determined for ", x[1])
msg_arg_outSavedToFile  <- function(x) paste0("Output saved to file: ", x[1], "\n")
msg_arg_noOutWrt        <- function()  "No output written, check the input object."
msg_arg_rdaMultObj      <- function(x) paste0("ERROR: ", x[1], " contains more than one object")
msg_arg_xlsxNoSheets    <- function(x) paste0("ERROR: ", x[1], " contains no sheets")
msg_arg_fileNoRows      <- function(x) paste0("ERROR: file ", x[1], " has no rows")
msg_arg_dir             <- function(x) paste0("ERROR: ", x[1], " must be a directory name")
msg_arg_dirDNE          <- function(x) paste0("ERROR: ", x[1], " does not exist")
msg_arg_dirNoWrtPerm    <- function(x) paste0("ERROR: ", x[1], " does not have write permission")
msg_arg_opEqValNotValid <- function(x) paste0("ERROR: the option ", x[1], getOpStrEq(), x[2], " is not valid")
msg_arg_opNotValid      <- function(x) paste0("ERROR: the option ", x[1], " is not valid")
msg_arg_opsNotValidFor  <- function(x) paste0("ERROR: ", x[1], " are not valid option name(s) for ", x[2])
msg_arg_opNotValidFor   <- function(x) paste0("ERROR: ", x[1], " is not valid option name for ", x[2])
msg_arg_opsError        <- function()  paste0("ERROR with options, make sure options are separated by a ", getOpStrSep())
msg_arg_colNotValid     <- function(x) paste0("ERROR: ", x[1], " must be a variable in the data")
msg_arg_colsNotValid    <- function(x) paste0("ERROR: some variable names in ", x[1], " are not valid")
msg_arg_colNumNotValid  <- function(x) paste0("ERROR: ", x[1], " must be a numeric variable in the data")
msg_arg_colNegNotValid  <- function(x) paste0("ERROR: ", x[1], " cannot have negative values")
msg_arg_varNotFound     <- function(x) paste0("ERROR: variable ", x[1], " not found in ", x[2])
msg_arg_colsNotFound    <- function(x) paste0("ERROR: ", x[1], " must contain columns ", x[2])

msg_ramp_no_padj        <- function(x) paste0("ERROR: No metabolites with adjusted p-value <= ", x[1])
msg_ramp_no_metab       <- function()  "No outcomes/exposures match with the metabolite ids"
msg_ramp_no_metab2      <- function()  "No metabolite names found"
msg_ramp_idtypes        <- function(x) paste0("ERROR: the metab data must contain one of the id types ", x[1])
msg_ramp_dbversion      <- function(x) paste0("ERROR: ", x[1], " must be a valid version number,",
                                              " see RaMP::listAvailableRaMPDbVersions()")

