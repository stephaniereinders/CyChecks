##Steps to make package a website

# Install release version from CRAN
install.packages("pkgdown")

#Usage:run the code below to create the docs folder and build the website
pkgdown::build_site()

#pushed the docs folder to our GitHub repo
#now able to change settings on our CyChecks GitHub repo/package -->

#Referenced
#build website for package [using] (https://pkgdown.r-lib.org/index.html) and https://pkgdown.r-lib.org/articles/pkgdown.html

#use the below code to create package vignette
usethis::use_vignette("name of vignette")

