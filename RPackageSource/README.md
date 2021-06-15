# [![Build Status](https://travis-ci.org/CBIIT/R-cometsAnalytics.svg?branch=master)](https://travis-ci.org/CBIIT/R-cometsAnalytics)
# [![Build status](https://ci.appveyor.com/api/projects/status/github/CBIIT/R-cometsAnalytics?branch=master)](https://ci.appveyor.com/project/Mathelab/r-cometsanalytics)

# R Comets-Analytics
[Comets-Analytics](http://comets-analytics.org/) supports and streamlines consortium-based analyses of metabolomics data. The software is continuously being developed and maintained by the COMETS Data Infrastructure Working Group with web interface support from NCI's CBIIT and R package development support from IMS.
COMETS Analytics was designed to simplify meta-analysis at the consortia level.  Users prepare data input, and then the software takes care of checking the data integrity, performs data analyses securely, and aggregates results in a standardized format.  Further details on the vision for implementing the software and the current features available can be found here.
Accessibility
The software can be run two different ways:
1. A standalone R package “R-cometsAnalytics” that encapsulates our core algorithms and functionality, allowing the software to be run locally. The GitHub repository for the development of COMETS Analytics is publicly available at https://github.com/CBIIT/R-cometsAnalytics/ under a GPL-3 license.
2. A web-based app (https://www.comets-analytics.org) developed as a user friendly interface to the R package using HTML5.  This app operates on secure cloud-based servers that delete data after analyses.

All underlying statistical analyses and data processing use the R-cometsAnalytics R package so that using the R package or the app will produce the same results.

# Version 
Current Version is 1.6: Released on 10/14/2019 with analytic module for unadjusted and partial correlation analyses. Complete details of the version history are documented in the GitHub repository https://github.com/CBIIT/R-cometsAnalytics/.
Upcoming Version 2.0: Analytic module with generalized linear models is in testing, expected release in 2021.

Previous releases can be found here: https://github.com/CBIIT/R-cometsAnalytics/releases

# Help
A companion vignette to run through analyses using the R package can be found [here](https://cbiit.github.io/R-cometsAnalytics/cometsvignette_v1.6.html).

A presentation of the software, its implementation and vision can be found [here](https://www.youtube.com/watch?reload=9&v=dWJ_fdibnms) and a preview of upcoming changes are presented [here](https://github.com/CBIIT/R-cometsAnalytics/tree/master/Presentations). 

For questions or help on COMETS Analytics app or R package, please send an e-mail to comets.analytics@gmail.com.

# Installation

To install R Comets-Analytics, simply type the following:

```
devtools::install_github("CBIIT/R-cometsAnalytics/RPackageSource")
```

If you are using a newer version of R (>4.0), you will need to reinstall specific d3heatmap and dplyr versions as follows:
```
devtools::install_github('rstudio/d3heatmap')
devtools::install_version('dplyr', version='0.8.5', repos='https://cloud.r-project.org/')
devtools::install_github("CBIIT/R-cometsAnalytics/RPackageSource")
```

# Example Data
We have provided an example input file for users to mimic when creating their own.  This data can be found in the folder inst/extdata.

# Contact/Help
For questions on help on COMETS Analytics app or R package, please send an e-mail to comets.analytics@gmail.com.
If you encouter any issues with the R package, please submit an issues using the Issue Tab in GitHub and we will get back to you as soon as we can. 

# Acknowledgements
We thank the National Cancer Institute (NCI) for supporting the development and expansion of COMETS Analytics, the NCI Center for Biomedical Informatics and Information Technology team for developing the app, the Information Management Services team for further developing the R package, and our users for providing feedback so we can continuously ameliorate the software.

R COMETS Package Development Team:  Ewy Mathé, Steve Moore, Ella Temprosa, Bill Wheeler, and Joe Zou.
Web application Development Team: Kailing Chen, Ewy Mathé, Steve Moore, Brian Park, and Ella Temprosa.
 
Metabolite Harmonization Team: Dave Ruggieri and Steve Moore

# Citation
Please site the following when using COMETS Analytics:
[Temprosa M, Moore SC, Zanetti KA, Appel N, Ruggieri D, Mazzilli KM, Chen KL, Kelly RS, Lasky-Su JA, Loftfield E, McClain K, Park B, Trijsburg L, Zeleznik OA, Mathé EA. COMETS Analytics: An online tool for analyzing and meta-analyzing metabolomics data in large research consortia. Am J Epidemiol. 2021 Apr 22:kwab120. doi: 10.1093/aje/kwab120. Epub ahead of print. PMID: 33889934.](https://pubmed.ncbi.nlm.nih.gov/33889934/)
 
A special thanks as well to the broader COMETS Data Infrastructure Group.




