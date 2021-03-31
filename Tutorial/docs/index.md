--- 
title: "COMETS Analytics Tutorial "
author: "Version 1.6"
date: "2021-02-02"
site: bookdown::bookdown_site
output:
  bookdown::gitbook:
    css: tufte.css
documentclass: book
cover-image: "static/COMETSLogo.png"
description: "This tutorial provides an overview and detailed instructions for the use of COMETS Analytics"
bibliography: [book.bib]
biblio-style: apalike
link-citations: yes
header-includes:
  - \usepackage{color}
  - \usepackage{marginnote}
---



# Introduction {-}
<div style="position:absolute; margin-top:-180px; margin-left:600px;" ><img src="static/COMETSLogo.png" width="60%">
</div>
[COMETS](https://epi.grants.cancer.gov/comets/), the **CO**nsortium of **MET**abolomics **S**tudies, has a major goal to produce jointly coordinated, multi-cohort, high-impact publications devoted to advancing the methods and scientific understanding of the human metabolome and its relationship to disease etiology and prognosis. Since its inception in 2014, COMETS has grown to include more than 80 cohorts worldwide. 

[COMETS Analytics](http://www.comets-analytics.org) serves as the infrastructure to facilitate and coordinate data analysis efforts. This tutorial gives an overview and detailed instructions on the use of COMETS Analytics in support of COMETS. 

<div style="line-height: 0.7;font-size: 12px">
Prepared by the COMETS Tutorial Group:
<ul>
<li> Ella Temprosa
<li> Steve Moore
<li> Oana Zeleznik
<li> Laura Trijsburg
<li> Rachel Kelly
<li> Erikka Loftfield
<li> Kathleen McClain
<li> Kaitlyn Mazzilli
</ul>
</div>



## Overview {-}
For each project, the cohort-level analytic flow consists of 3 steps to produce the standardized output required for central meta-analyses.
Multiple projects can be bundled for each run to minimize burden on cohort analytic resources.  
<a href="static/cometsflow.PNG" target="_blank"><img src="static/cometsflow.PNG" style="width: 80%"></a>



This tutorial serves as a guide through this cycle. You can register for access to COMETS Analytics using the instructions in Chapter \@ref(register) [Registration]. For a quick walk-through the cycle, you can use the sample input file and follow the directions in Chapter \@ref(quickstart) [Get Started with the Sample File]. Once you are ready to conduct your cohort-specific analyses, see the Chapter \@ref(cohort) [Conduct Cohort Data Analyses]. For more technical details, refer to the Chapter \@ref(manual) [Manual] and [FAQ] for answers to commonly asked questions.



