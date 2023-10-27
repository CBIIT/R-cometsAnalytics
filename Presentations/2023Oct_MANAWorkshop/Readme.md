# COMETS Analytics: An Open-source Analytic Tool for Single Cohort and Meta-analyses of Multiple Cohorts in Metabolomic-based Edpidemiological Studies

### Ewy Mathé, Nicole Prince, Ella Temprosa
### Friday, October 27th, 10-11:45 am CT

# Workshop Goals
1. Participants will learn about the background and capabilities of COMETS Analytics (CA)
2. Participants will be able to conduct an analysis of metabolomic data in a single cohort using the provided CA example or their own data
3. Participants will learn how to set up a meta-analysis of metabolomics data across multiple cohorts
4. Participants will be able to provide feedback and request new features

# Prerequesities 
1. Latest version of R
2. Latest version of R Studio
3. Downloaded input [cohort_1.xlsx](cohort_1.xlsx) and [cohort_2.xlsx](cohort_2.xlsx) files from this directory

# Agenda
| Time | Topic |
| :---:   | --- | 
| 10:00-10:15 | Introduction to workshop, COMETS and COMETS Analytics  |
| 10:15-10:55 | Single cohort analyses  |
| |  * Input File |
| |   * R package analysis and output |
| |   * Exercise |
| 10:55-11:00 | BREAK |
| 11:00-11:25 | Meta-Analysis |
| | * Input File | 
| |   * Meta-analysis in R and Output |
| |   * Exercise |
| 11:25-11:40 | Questions and Discussion |
| 11:30-11:45 | Wrap up |

# 1 Getting Prepared
## 1.1 Installing COMETS Analytics
For this workshop, we will be using our v3.0-dev version, which is in development.  To install, please use the following code:
```
if (!require("devtools")) {
    install.packages("devtools")
}

library(devtools)
devtools::install_github("CBIIT/R-cometsAnalytics/RPackageSource@v3.0-dev")

library(RcometsAnalytics)
```

This version supports meta-analyses, in addition to the following single cohort models:
- Spearman correlation
- Spearman correlation with user-defined terms
- GLM Poisson
- GLM binomial
- Survival
- Conditional logistic regression 

## 1.2 Access to Input Data
The input data for this workshop is provided in the "cohort_1.xlsx" and "cohort_2.xlsx" sheets. These files can be downloaded from [this folder](https://github.com/CBIIT/R-cometsAnalytics/tree/v3.0-dev/RPackageSource/inst/extdata). 
You are also welcome to work through exercises with your own data.


# 2. Single Cohort Analyses
We will be following along the COMETS Vignette to conduct the single cohort analysis.
You can find the Vignette with the example input sheet [here](https://cbiit.github.io/R-cometsAnalytics/2023MANA_COMETSAnalytics_vignette.html)

# 3. Meta-Analyses 
## Introduction

RcometsAnalytics supports all cohort-specific analyses and meta-analyses of the R COMETS Analytics consortium. This collaborative work is done via the COMETS Data Infrastructure interest group activities. For more information, see the COMETS [**website**](http://epi.grants.cancer.gov/comets/). This vignette demonstrates using the RcometsAnalytics R package to run a meta-analysis from the command line assuming that the user does not already have model output files. The first three steps are similar to steps found in the vignette for running models locally: [**Comets_vignette**](https://cbiit.github.io/R-cometsAnalytics/cometsvignette_v2.1.html).

## Meta-analysis Background Information

A meta-analysis is performed by combining the results from more than one cohort in order to increase the power of association tests. Each set of cohort results should be from the same underlying model, and each cohort should be independent from all other cohorts, that is, no overlapping subjects between any two cohorts. This package will conduct both fixed-effects and random-effects meta-analyses based on the DerSimonian-Laird method.

## RcometsAnalytics R package

The main functions for performing meta-analyses in the RcometsAnalytics R package are *runMeta()* and *runAllMeta()*. Both functions require that the model output from each cohort has been saved to a file. The function *meta_calc()* is the core function that performs the calculations. Documentation of the RcometsAnalytics R package can be found here [**manual**](https://github.com/CBIIT/R-cometsAnalytics/blob/gh-pages/RcometsAnalytics-manual-v3.0.pdf).

## Data Input Format

The files input to the *runMeta()* meta-analysis function contain the output from the *runModel()* function. These files can be Excel files with extension .xlsx or R object files with extension .rda. Each file should contain the results from a single model run from one of the cohorts.


## Example Workflow with two cohorts

Let "cohort_1" and "cohort_2" be the names of the two cohorts. For each cohort, we will run the same model using the *runModel()* function, save the model output, and then run a meta-analysis using the *runMeta()* function.


### 1. Load Data

For each cohort, load the cohort's data with the *readCOMETSinput()* function.

```{r}
# Retrieve the full paths of each input data
file1 <- file.path(getwd(), "cohort_1.xlsx")
file2 <- file.path(getwd(), "cohort_2.xlsx")

# Read in and process the input data
data1 <- RcometsAnalytics::readCOMETSinput(file1)
data2 <- RcometsAnalytics::readCOMETSinput(file2)
```

### 2. Get Model Data

Get the model data for each cohort. The model must be the same for all cohorts. The model to be run will have age as a continuous exposure variable, all metabolites as the outcome variables, and bmi as a categorical adjustment variable.

```{r}
modeldata1 <- RcometsAnalytics::getModelData(data1, modelspec="Interactive", 
                          adjvars="bmi_grp", outcomes=NULL, exposures="age")
modeldata2 <- RcometsAnalytics::getModelData(data2, modelspec="Interactive", 
                          adjvars="bmi_grp", outcomes=NULL, exposures="age")
```

### 3. Linear Regression

The *runModel()* function will first be called to obtain the results from each cohort for a linear regression analysis. Since the meta-analysis function *runMeta()* requires files for input, the results from *runModel()* will be saved to files. The output file names defined below have the specific format <model name><cohort name><date>.ext which is important when using the *runAllMeta()* function shown in section 6. The files *runMeta()* accepts have the extension ".xlsx" or ".rda".\
The model name is specified as "AgeAdjustedForBMI".

results1 \<- RcometsAnalytics::runModel(modeldata1, data1, "cohort_1", out.file=outfile1, op=list(model="lm"))

```{r}
outfile1  <- paste0(getwd(), "/", "AgeAdjustedForBMI__cohort_1__2023-04-28.rda")
outfile2  <- paste0(getwd(), "/", "AgeAdjustedForBMI__cohort_2__2023-04-28.rda")
results1  <- RcometsAnalytics::runModel(modeldata1, data1, "cohort_1", out.file=outfile1, op=list(model="lm"))

results2  <- RcometsAnalytics::runModel(modeldata2, data2, "cohort_2", out.file=outfile2, op=list(model="lm"))
```

### 4. Meta Analysis of Results

Run the meta-analysis with the *runMeta()* function, and then display the first two rows of the results. In this example, a vector of file names is passed into the *runMeta()* function; however a list of file information can be passed in as we will see in a later example below. See the *runMeta()* function in the user [**manual**](https://github.com/CBIIT/R-cometsAnalytics/blob/gh-pages/RcometsAnalytics-manual-v3.0.pdf) for complete documentation.

```{r}
ret <- RcometsAnalytics::runMeta(c(outfile1, outfile2))
ret$Results[1:2,]
```

### 5. Example using runAllMeta

The *runAllMeta()* function is for running meta-analyses in super-batch mode. The function takes three arguments, with the first argument being a character vector of file names and/or folder names. The second argument is the output folder to write the results to, and the third argument is an optional file for meta-analysis options. The result files "outfile1" and "outfile2" created in section 3 above will be used for this meta-analysis. Instead of passing in the file names, the folder name where these files reside (the working directory) will be passed in. The output from *runAllMeta()* will also be written to the working directory.

```{r}
tmp <- RcometsAnalytics::runAllMeta(getwd(), getwd())
```

The output files names created by *runAllMeta()* will be of the form \<model name\>\_\_meta\_\_\<date\>.ext.

```{r}
res.file <- paste0(getwd(), "/AgeAdjustedForBMI__meta__", Sys.Date(), ".xlsx")
```

Read in the table of results and compare to the previous meta-analysis performed in section 4. The results should be the same.

```{r}
res <- as.data.frame(readxl::read_excel(res.file, "Results"))
all.equal(ret$Results, res)
```

Delete the files created

```{r}
file.remove(c(outfile1, outfile2, res.file))
```

```{r}
#sessionInfo()
```


# How to get involved
* Cohort membership https://cssi.cancer.gov/comets#membership
* Individual Membership https://cssi.cancer.gov/sites/default/files/comets-consortium-membership-application-v3.pdf
* Project Proposal https://cssi.cancer.gov/comets/projects
* Interest Groups https://cssi.cancer.gov/comets/interest-groups 
* Give Scientific Presentation at Monthly Meeting

# Useful Links
-	COMETS Analytics: http://comets-analytics.org/ 
-	COMETS public site: https://cssi.cancer.gov/comets 

# References
Temprosa M, Moore SC, Zanetti KA, et al. COMETS Analytics: An Online Tool for Analyzing and Meta-Analyzing Metabolomics Data in Large Research Consortia. Am J Epidemiol. Jan 1 2022;191(1):147-158. doi:10.1093/aje/kwab120

Yu B, Zanetti KA, Temprosa et al.  The Consortium of Metabolomics Studies (COMETS): Metabolomics in 47 Prospective Cohort Studies. Am J Epidem. 2019 Jun 1;188(6):991-1012.


