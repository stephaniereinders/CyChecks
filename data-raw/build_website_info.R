##Steps to make package a website

#step1: Install release version from CRAN
install.packages("pkgdown")

#step2: Run the code below to create the docs folder and build the website
pkgdown::build_site()

#step3: Push the docs folder to our GitHub repo
#now able to change settings on our CyChecks GitHub repo/package -->

#Referenced
#here is a helpful [link] ((https://pkgdown.r-lib.org/index.html) that describes how to build a website for your package. You can also look [here] (https://pkgdown.r-lib.org/articles/pkgdown.html) as well.

#use the below code to create a package vignette (for report to include additonal info)
usethis::use_vignette("name of vignette")

