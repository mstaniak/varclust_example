my_install <- function(package_name="", repo="https://cran.rstudio.com"){
  if(!require(package_name, character.only=TRUE)){
    install.packages(package_name, repos = repo)
  }
}

my_install("devtools")
my_install("dplyr")
my_install("tidyr")
my_install("readr")
my_install("stringr")
my_install("ggplot2")
my_install("ggmap")
my_install("lubridate")
if(!require("varclust")){
  devtools::install_github("psobczyk/varclust")
}
