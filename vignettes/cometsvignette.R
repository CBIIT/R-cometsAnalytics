## ------------------------------------------------------------------------
# Retrieve the full path of the input data
dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInput.xlsx")
# Read in and process the input data
exmetabdata <- COMETS::readCOMETSinput(csvfile)

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modbatch="1.1 Unadjusted")

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modelspec="Interactive",colvars="age bmi")

