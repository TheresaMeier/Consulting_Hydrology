############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Pre-Processing

## load libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
library(zoo)
library(roll)
library(caret)
library(car)
source("Codes/01_Data_Preparation/01_Data_preparation_functions.R", encoding = "UTF-8")

path = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten"

## Generate member-specific data
member = c("kbe", "kbj", "kbo", "kbt", "kby", "kcd", "kci", "kcn", "kcs", "kcx")

for (mb in member){
  assign(mb, aggregate_data(mb, path))
}

## Generate full data set

hydro = full_data(path)

## Generate summer and winter data sets

hydro_summer = summer_data(hydro)
hydro_winter = winter_data(hydro)
