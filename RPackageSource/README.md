
# [![Build status](https://ci.appveyor.com/api/projects/status/github/CBIIT/R-cometsAnalytics?branch=master)](https://ci.appveyor.com/project/Mathelab/r-cometsanalytics)
[![Build status](https://github.com/CBIIT/R-cometsAnalytics/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/CBIIT/R-cometsAnalytics/actions)

# COMETS Analytics

COMETS Analytics supports and streamlines consortium-based analyses of metabolomics data. The software is continuously being developed by Ewy Mathé (Division of Preclinical Innovation, National Center for Advancing Translational Sciences) with input and testing from Marinella Temprosa (Dept. of Biostatistics and Bioinformatics, Milken Institute School of Public Health, George Washington University), Steve Moore (Division of Cancer Epidemiology and Genetics, National Cancer Institute) and the COMETS Early Career Interest Group.  The web interface is implemented by Phyllip Cho at NCI's CBIIT and the R package by Bill Wheeler at IMS. Constructive feedback is provided by the COMETS Data Infrastructure Working Group and our users.

COMETS Analytics was designed to simplify meta-analysis at the consortia level. Users prepare data input, and then the software takes care of checking the data and model integrity, performs data analyses securely, and aggregates results in a standardized format. Further details on the vision for implementing the software and the current features available can be found [here](https://pubmed.ncbi.nlm.nih.gov/33889934/).

The software is available in two different flavors:
1) The [Comets-Analytics](http://comets-analytics.org/) web application, which is public and user-friendly.
2) The RcometsAnalytics R package, which is publicly available in this repo.

Of note, this R package contains all the functions that are used by the COMETS Analytics web application, but can also be run independently. Currently, the web application is up-to-date with this version, v3.0, of the R package yet the meta-analysis function is only available in the R package.   

## Main Features
Broadly, The current version of the software supports the following analyses:
 
- Harmoniization of metabolite names using a curated mapping of names across different platforms.
- Correlation, partial correlations, regression modeling and survival analyses.
- Meta-analysis (when single cohort analyses are performed with COMETS Analytics).
- Chemical and biochemical pathway enrichment using [RaMP-DB](https://github.com/ncats/RaMP-DB).

# Installation and Loading the R package

To install and load the RcometsAnalytics package, simply type the following:

```
if (!require("devtools")) {
    install.packages("devtools")
}

library(devtools)
devtools::install_github("CBIIT/R-cometsAnalytics/RPackageSource@master")

library(RcometsAnalytics)
```

# Documentation and Example Data

# News
All changes, including history of new features, changes, bug fixes, etc., are logged under our [NEWS](https://github.com/CBIIT/R-cometsAnalytics/blob/master/RPackageSource/NEWS) section.  

# Vignette
To view a detailed vignette that outlines the basic steps for running:
- Single cohort analyses using the RcometsAnalytics package [Click here](https://cbiit.github.io/R-cometsAnalytics/cometsvignette_v3.0.html).
- Meta-analyses [Click here](https://cbiit.github.io/R-cometsAnalytics/Vignette_MetaAnalyses_v3.0.html) 

## Example Data
We have provided an example input file for users to mimic when creating their own.  This data can be found in the folder inst/extdata.

# Manual
To view the user manual listing all the documentation for functions contained within the RcometsAnalytics package, [Click here](https://github.com/CBIIT/R-cometsAnalytics/blob/gh-pages/RcometsAnalytics-manual-v2.1.pdf).  The group is currently working on updating this manual.

# Citation
When using the COMETS Analytics web application and/or this associate package, please cite the following manuscript:

Temprosa M, Moore SC, Zanetti KA, Appel N, Ruggieri D, Mazzilli KM, Chen KL, Kelly RS, Lasky-Su JA, Loftfield E, McClain K, Park B, Trijsburg L, Zeleznik OA, Mathé EA. COMETS Analytics: An Online Tool for Analyzing and Meta-Analyzing Metabolomics Data in Large Research Consortia. Am J Epidemiol. 2022 Jan  1;191(1):147-158. doi: 10.1093/aje/kwab120. PMID: 33889934.

# Contact
If you encounter any issues, please report an issue in the Issues tab (preferred method) or email comets.analytics@gmail.com.  We are always open to comments so please reach out to us should you have any questions or suggestions .
