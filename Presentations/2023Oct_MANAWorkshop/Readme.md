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
| 10:15-10:55 | Single cohort analyses (Ewy Mathé and Nicole Prince) |
| |  * Input File (Ewy Mathé) |
| |   * R package analysis and output (Nicole Prince)  |
| |   * Exercise (Nicole Prince)  |
| 10:55-11:00 | BREAK |
| 11:00-11:25 | Meta-Analysis (Ella Temprosa) |
| | * Input File | 
| |   * Meta-analysis in R and Output |
| |   * Exercise |
| 11:25-11:40 | Questions and Discussion |
| 11:30-11:45 | Wrap up (Ella Temprosa) |

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
You can find the Vignette with the example input sheet [here](https://github.com/nicole-prince/R-cometsAnalytics/blob/master/RPackageSource/vignettes/COMETS_vignette_MANA.html)

# 3. Meta-Analysis Analyses
(Coming soon)

# Useful Links
Useful links:
-	COMETS portal: https://epi.grants.cancer.gov/cometsportal 
-	COMETS Analytics: http://comets-analytics.org/ 
-	COMETS public site: https://cssi.cancer.gov/comets 



