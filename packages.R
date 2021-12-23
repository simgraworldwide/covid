# define required packages
requiredPackages <- c(
  "DBI",
  "here",
  "janitor",
  "lubridate",
  "odbc",
  "renv",
  "ROracle",
  "rvest",
  "tidyverse"
)


# install/load required packages:
if (exists("requiredPackages")) {
  # install required packages that are not installed yet:
  new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  # load required packages:
  lapply(requiredPackages, library, character.only = TRUE)
}
rm(new.packages, requiredPackages)


# # connect db
# source("~/db_connect.R")
