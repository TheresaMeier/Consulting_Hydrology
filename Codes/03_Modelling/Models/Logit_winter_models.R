############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Winter Models

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

## load the data
path = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten"

data_winter <- fread(paste0(path,"/Datensätze_aggregiert/hydro_winter.txt"), data.table = FALSE)[,-1]

data_winter <- data_winter %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

w.level = levels(data_winter$name_waterlevel)
member = levels(data_winter$member)

################################################################################
############################### Logit models ###################################
################################################################################

for (mb in member) {
  print(mb)
  for (level in w.level){
    logit_winter <- gam(lowlevel ~ 
                         # Main effects for all drivers
                         avg_airtmp + avg_relhum + avg_glorad +
                         avg_soilwater + avg_snowstorage +
                         avg_precip +
                         YY +
                         # Continous interactions between drivers
                         avg_airtmp:avg_precip +
                         avg_airtmp:avg_snowstorage +
                         avg_airtmp:avg_soilwater,
                       family = binomial, data = data_winter[data_winter$member == mb & data_winter$name_waterlevel == level,])
    saveRDS(logit_winter, file = paste0(path,"/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb, "_winter_", level, ".RDS"))
  }
}

################################################################################
######################## Averaging over different members ######################
################################################################################

# Coefficients for winter
coefs.wl.winter.avg = as.data.frame(matrix(data=0, nrow = 98, ncol = 11))
colnames(coefs.wl.winter.avg) = c("(Intercept)", "avg_airtmp", "avg_relhum", "avg_glorad", "avg_soilwater",
                                  "avg_snowstorage", "avg_precip", "YY", "avg_airtmp:avg_precip", "avg_airtmp:avg_snowstorage", "avg_airtmp:avg_soilwater")
row.names(coefs.wl.winter.avg) = levels(data_winter$name_waterlevel)

# Create tables for all members
for (mb in member){
  assign(paste0("coefs.wl.winter.", mb), coefs.wl.winter.avg)
}


for (mb in member){
  for (level in row.names(eval.winter.kcn)){
    model = readRDS(paste0(path, "/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
   for (var in colnames(coefs.wl.winter.avg)){
     coefs.wl.winter.avg[level,var] =  coefs.wl.winter.avg[level,var] + coefficients(model)[var]

     if (mb == "kbe"){
       coefs.wl.winter.kbe[level,var] = coefficients(model)[var]
     }
     if (mb == "kbj"){
       coefs.wl.winter.kbj[level,var] = coefficients(model)[var]
     }
     if (mb == "kbo"){
       coefs.wl.winter.kbo[level,var] = coefficients(model)[var]
     }
     if (mb == "kbt"){
       coefs.wl.winter.kbt[level,var] = coefficients(model)[var]
     }
     if (mb == "kby"){
       coefs.wl.winter.kby[level,var] = coefficients(model)[var]
     }
     if (mb == "kcd"){
       coefs.wl.winter.kcd[level,var] = coefficients(model)[var]
     }
     if (mb == "kci"){
       coefs.wl.winter.kci[level,var] = coefficients(model)[var]
     }
     if (mb == "kcn"){
       coefs.wl.winter.kcn[level,var] = coefficients(model)[var]
     }
     if (mb == "kcs"){
       coefs.wl.winter.kcs[level,var] = coefficients(model)[var]
     }
     if (mb == "kcx"){
       coefs.wl.winter.kcx[level,var] = coefficients(model)[var]
     }
   }
  }
}

coefs.wl.winter.avg[,c(1:11)] = coefs.wl.winter.avg[,c(1:11)] / 10
write.table(coefs.wl.winter.avg, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.avg"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kbe, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kbe"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kbj, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kbj"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kbo, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kbo"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kbt, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kbt"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kby, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kby"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kcd, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kcd"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kci, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kci"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kcn, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kcn"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kcs, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kcs"), sep="\t", col.names = TRUE, row.names = TRUE)
write.table(coefs.wl.winter.kcx, file = paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coefs.winter.kcx"), sep="\t", col.names = TRUE, row.names = TRUE)
