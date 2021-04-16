## Checking for installed packages and installing them if not found
list.of.packages <- c("shiny", "shinythemes","readxl","writexl","tidyverse","dplyr","shinydashboard")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0) {
  install.packages(new.packages, dependencies = TRUE) 
  print(paste0("The following package was installed:", new.packages)) 
    } else if (length(new.packages) == 0) {
  print("All packages were already installed previously")
    }

## Running libraries
library("shiny")
library("shinythemes")
library("readxl")
library("writexl")
library("tidyverse")
library("dplyr")
library("shinydashboard")
library("activityinfo")
library("r4vstyle")
library("plotly")
library("datamods")