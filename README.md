
<!-- README.md is generated from README.Rmd. Please edit that file -->
CyChecks
========

<!-- 
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) 
-->
*Overview*

The goal of CyChecks is to allow users easy access to the publicly available data concerning Iowa State University (the Cyclones) employee salaries.

Some of the services this access could offer include:

1.  Aiding job seekers in negotiating starting salaries.
2.  Shedding light on possible pay inequities with respect to gender
3.  Seeing what the highest paid positions are at the university.

*Background*

The state of Iowa offers a large amount of public data at [this site](https://data.iowa.gov/). You can access the data by signing up for an API token [here](https://dev.socrata.com/foundry/data.iowa.gov/s3p7-wy6w). Iowa state employee salaries are available at [this site](https://data.iowa.gov/State-Finances/State-of-Iowa-Salary-Book/s3p7-wy6w). Using an API token, CyChecks provides a function to easily get data for any given year.

The data from this site does not include the employee's home department. Unfortunately, to our knowledge the data linking names to departments is not easily accessible. ISU's Human Resources Department kindly provided a list of employees with their home departments and associated colleges valid as of January 1 2019. Since acquiring this information is not reproducible to the average user, we've included a full dataset (with names anonymized) of all salary info (2007 - 2018) cross-referenced by department in the CyChecks package. We realize employees that have left before 2019 will not have their department listed in this dataset. This isn't ideal, but a reality of trying to display this sort of data.

*Summary*

This package currently includes:

1.  A function to:

<!-- -->

1.  download data directly from the iowa.gov website
2.  anonymize names of downloaded data
3.  simplify professor and post-doc position titles
4.  quickly identify departments with possible gender pay inequities

<!-- -->

1.  Data from:
    1.  Employee/salary/department dataset for 2007-2018
    2.  Employee/salary/department/college dataset assumed to be valid for 2018
2.  A shiny app for visualizing data

Installation
------------

You can install the development version CyChecks from [github](https://CRAN.R-project.org) with:

``` r
devtools::install_github("https://github.com/vanichols/CyChecks")
```

Example
-------

You can access the data using our XX function.

``` r
## basic example code
```

[![Travis build status](https://travis-ci.org/vanichols/CyChecks.svg?branch=master)](https://travis-ci.org/vanichols/CyChecks)
