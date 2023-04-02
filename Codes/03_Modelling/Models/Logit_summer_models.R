############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Summer Models

## Load libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
library(zoo)
library(mgcv)
library(roll)
library(caret)
library(pROC)
library(car)

## Load the data
path = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten"
data_summer <- fread(paste0(path,"/Datensätze_aggregiert/hydro_summer.txt"), data.table = FALSE)[,-1]

data_summer <- data_summer %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

w.level = levels(data_summer$name_waterlevel)
member = levels(data_summer$member)

################################################################################
############################ Inclusion Criteria ################################
################################################################################

## Table for snow storage
snowstorage_summer = as.data.frame(matrix(data = 0, nrow = 10, ncol = 98))
rownames(snowstorage_summer) = levels(data_summer$member)
colnames(snowstorage_summer) = levels(data_summer$name_waterlevel)

# Inclusion criterion for summer models: maximum snow storage in each member 
# has to be greater than 1cm
for (mb in member) {
  for (level in colnames(snowstorage_summer)){
    snowstorage_summer[mb, level] = max(data_summer[data_summer$member == mb & data_summer$name_waterlevel == level,]$avg_snowstorage, na.rm = TRUE)
  }
}

snowstorage_summer = as.data.frame(t(snowstorage_summer))
for (i in c(1:98)){
  snowstorage_summer$model[i] = if_else(length(snowstorage_summer[i,][snowstorage_summer[i,]>10]) == 10,1,0)
}


##  Table for low-flow events
lowlevel_summer = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
colnames(lowlevel_summer) = levels(data_summer$member)
rownames(lowlevel_summer) = levels(data_summer$name_waterlevel)

# Inclusion criterion: at least one low-flow event in each member per catchment
for (mb in member) {
  for (level in rownames(lowlevel_summer)){
    lowlevel_summer[level,mb] = sum(data_summer[data_summer$name_waterlevel == level & data_summer$member == mb,]$lowlevel)
  }
}

for (i in c(1:98)){
  lowlevel_summer$model[i] = if_else(length(lowlevel_summer[i,][lowlevel_summer[i,]>50]) == 10,1,0)
}

################################################################################
############################### Logit models ###################################
################################################################################

for (mb in member) {
  print(mb)
  for (level in w.level){
    if (snowstorage_summer[level,"model"] == 1 & lowlevel_summer[level, "model"] == 1){
      logit_summer <- gam(lowlevel ~ 
                            # Main effects for all drivers
                            avg_airtmp + avg_relhum + avg_glorad +
                            avg_soilwater + avg_snowstorage +
                            avg_precip +
                            YY +
                            # Continous interactions between drivers
                            avg_airtmp:avg_precip +
                            avg_airtmp:avg_snowstorage+
                            avg_airtmp:avg_soilwater,
                          family = binomial, data = data_summer[data_summer$member == mb & data_summer$name_waterlevel == level,])
      saveRDS(logit_summer, file = paste0(path, "/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb, "_summer_", level, ".RDS"))
    }
    if(snowstorage_summer[level,"model"] == 0 & lowlevel_summer[level, "model"] == 1) {
      logit_summer <- gam(lowlevel ~ 
                            # Main effects for all drivers
                            avg_airtmp + avg_relhum + avg_glorad +
                            avg_soilwater + 
                            avg_precip +
                            YY +
                            # Continous interactions between drivers
                            avg_airtmp:avg_precip +
                            avg_airtmp:avg_soilwater,
                          family = binomial, data = data_summer[data_summer$member == mb & data_summer$name_waterlevel == level,])
      saveRDS(logit_summer, file = paste0(path, "/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb, "_summer_", level, ".RDS"))
    }
  }
}

################################################################################
######################## Averaging over different members ######################
################################################################################

## Coefficients for summer
coefs.wl.summer.avg = as.data.frame(matrix(data=0, nrow = 97, ncol = 11))
colnames(coefs.wl.summer.avg) = c("(Intercept)", "avg_airtmp", "avg_relhum", "avg_glorad", "avg_soilwater",
                                  "avg_snowstorage", "avg_precip", "YY", "avg_airtmp:avg_precip", "avg_airtmp:avg_snowstorage", "avg_airtmp:avg_soilwater")
row.names(coefs.wl.summer.avg) = levels(data_summer$name_waterlevel)[-30]

# Create tables for all members
for (mb in member){
  assign(paste0("coefs.wl.summer.", mb), coefs.wl.summer.avg)
}


for (mb in member){
  print(mb)
  for (level in row.names(eval.summer.kbe)){
    model = readRDS(paste0(path, "/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    for (var in colnames(coefs.wl.summer.avg)){
      coefs.wl.summer.avg[level,var] =  coefs.wl.summer.avg[level,var] + coefficients(model)[var]
      
      if (mb == "kbe"){
        coefs.wl.summer.kbe[level,var] = coefficients(model)[var]
      }
      if (mb == "kbj"){
        coefs.wl.summer.kbj[level,var] = coefficients(model)[var]
      }
      if (mb == "kbo"){
        coefs.wl.summer.kbo[level,var] = coefficients(model)[var]
      }
      if (mb == "kbt"){
        coefs.wl.summer.kbt[level,var] = coefficients(model)[var]
      }
      if (mb == "kby"){
        coefs.wl.summer.kby[level,var] = coefficients(model)[var]
      }
      if (mb == "kcd"){
        coefs.wl.summer.kcd[level,var] = coefficients(model)[var]
      }
      if (mb == "kci"){
        coefs.wl.summer.kci[level,var] = coefficients(model)[var]
      }
      if (mb == "kcn"){
        coefs.wl.summer.kcn[level,var] = coefficients(model)[var]
      }
      if (mb == "kcs"){
        coefs.wl.summer.kcs[level,var] = coefficients(model)[var]
      }
      if (mb == "kcx"){
        coefs.wl.summer.kcx[level,var] = coefficients(model)[var]
      }
    }
  }
}

coefs.wl.summer.avg[,c(1:11)] = coefs.wl.summer.avg[,c(1:11)] / 10
write.table(coefs.wl.summer.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.avg", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kbe, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kbe", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kbj, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kbj", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kbo, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kbo", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kbt, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kbt", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kby, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kby", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kcd, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kcd", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kci, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kci", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kcn, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kcn", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kcs, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kcs", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.summer.kcx, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coefs.summer.kcx", sep="\t", col.names = TRUE, row.names = TRUE)
