############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Analysis of Significance

## Load libraries
library(mgcv)
library(data.table)
library(dplyr)

## Load the data
path = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten"
hydro <- fread(paste0(path, "/Datensätze_aggregiert/hydro.txt"), data.table = FALSE)[,-1]

hydro <- hydro %>%
  mutate(member = as.factor(member),
       name_waterlevel = as.factor(name_waterlevel))

coefs.summer.avg = fread(paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coef/summer/coefs.summer.avg"),
                         data.table = FALSE)
coefs.summer.avg$V1 = gsub("X", "", levels(hydro$name_waterlevel)[-30])
colnames(coefs.summer.avg)[1] = "name_waterlevel"

coefs.winter.avg = fread(paste0(path, "/R Scripts/Model/Models_waterlevel/tables/coef/winter/coefs.winter.avg"),
                         data.table = FALSE)
coefs.winter.avg$V1 = gsub("X", "", levels(hydro$name_waterlevel))
colnames(coefs.winter.avg)[1] = "name_waterlevel"


w.level = levels(hydro$name_waterlevel)
member = levels(hydro$member)

################################################################################
################################ summer ########################################
################################################################################

### Airtmp
coef.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_airtmp.summer) = levels(hydro$member)

pval.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_airtmp.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    coef.avg_airtmp.summer[level, mb] = coefficients(model)["avg_airtmp"]
    pval.avg_airtmp.summer[level, mb] = summary(model)$p.pv["avg_airtmp"]
  }
}
pval.avg_airtmp.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_airtmp.summer[i,"significant"] = if_else(length(pval.avg_airtmp.summer[i,][pval.avg_airtmp.summer[i,]<0.05/10]) == 11,1,0)
}
coefs.summer.avg$sig_airtmp = ifelse(pval.avg_airtmp.summer$significant== 0, NA, coefs.summer.avg$avg_airtmp)

### Precip

coef.avg_precip.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_precip.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_precip.summer) = levels(hydro$member)

pval.avg_precip.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_precip.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_precip.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_precip.summer[level, mb] = coefficients(model)["avg_precip"]
    pval.avg_precip.summer[level, mb] = summary(model)$p.pv["avg_precip"]
  }
}
pval.avg_precip.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_precip.summer[i,"significant"] = if_else(length(pval.avg_precip.summer[i,][pval.avg_precip.summer[i,]<0.05/10]) == 11,1,0)
}
coefs.summer.avg$sig_precip = ifelse(pval.avg_precip.summer$significant== 0, NA, coefs.summer.avg$avg_precip)

### Soil Water

coef.avg_soilwater.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_soilwater.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_soilwater.summer) = levels(hydro$member)

pval.avg_soilwater.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_soilwater.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_soilwater.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_soilwater.summer[level, mb] = coefficients(model)["avg_soilwater"]
    pval.avg_soilwater.summer[level, mb] = summary(model)$p.pv["avg_soilwater"]
  }
}
pval.avg_soilwater.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_soilwater.summer[i,"significant"] = if_else(length(pval.avg_soilwater.summer[i,][pval.avg_soilwater.summer[i,]<0.05/10]) == 11,1,0)
}
coefs.summer.avg$sig_soilwater = ifelse(pval.avg_soilwater.summer$significant== 0, NA, coefs.summer.avg$avg_soilwater)

### Snow Storage

coef.avg_snowstorage.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_snowstorage.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_snowstorage.summer) = levels(hydro$member)

pval.avg_snowstorage.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_snowstorage.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_snowstorage.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_snowstorage.summer[level, mb] = coefficients(model)["avg_snowstorage"]
    pval.avg_snowstorage.summer[level, mb] = summary(model)$p.pv["avg_snowstorage"]
  }
}
pval.avg_snowstorage.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_snowstorage.summer[i,"significant"] = if_else(length(pval.avg_snowstorage.summer[i,][pval.avg_snowstorage.summer[i,]<0.05/10]) == 11 &
                                                           !is.na(pval.avg_snowstorage.summer[i,1]),1,0)
}
coefs.summer.avg$sig_snowstorage = ifelse(pval.avg_snowstorage.summer$significant== 0, NA, coefs.summer.avg$avg_snowstorage)

### Relative Humidity

coef.avg_relhum.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_relhum.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_relhum.summer) = levels(hydro$member)

pval.avg_relhum.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_relhum.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_relhum.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_relhum.summer[level, mb] = coefficients(model)["avg_relhum"]
    pval.avg_relhum.summer[level, mb] = summary(model)$p.pv["avg_relhum"]
  }
}
pval.avg_relhum.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_relhum.summer[i,"significant"] = if_else(length(pval.avg_relhum.summer[i,][pval.avg_relhum.summer[i,]<0.05/10]) == 11,1,0)
}
coefs.summer.avg$sig_relhum = ifelse(pval.avg_relhum.summer$significant== 0, NA, coefs.summer.avg$avg_relhum)

### Radiation

coef.avg_glorad.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_glorad.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_glorad.summer) = levels(hydro$member)

pval.avg_glorad.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_glorad.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_glorad.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_glorad.summer[level, mb] = coefficients(model)["avg_glorad"]
    pval.avg_glorad.summer[level, mb] = summary(model)$p.pv["avg_glorad"]
  }
}
pval.avg_glorad.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_glorad.summer[i,"significant"] = if_else(length(pval.avg_glorad.summer[i,][pval.avg_glorad.summer[i,]<0.05/10]) == 11,1,0)
}
coefs.summer.avg$sig_glorad = ifelse(pval.avg_glorad.summer$significant== 0, NA, coefs.summer.avg$avg_glorad)

# Year

coef.YY.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.YY.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.YY.summer) = levels(hydro$member)

pval.YY.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.YY.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.YY.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.YY.summer[level, mb] = coefficients(model)["YY"]
    pval.YY.summer[level, mb] = summary(model)$p.pv["YY"]
  }
}
pval.YY.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.YY.summer[i,"significant"] = if_else(length(pval.YY.summer[i,][pval.YY.summer[i,]<0.05/10]) == 11,1,0)
}
coefs.summer.avg$sig_YY = ifelse(pval.YY.summer$significant== 0, NA, coefs.summer.avg$YY)

### Interaction temperature: precipitation

coef.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_airtmp.summer) = levels(hydro$member)

pval.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_airtmp.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_airtmp.summer[level, mb] = coefficients(model)["avg_airtmp:avg_precip"]
    pval.avg_airtmp.summer[level, mb] = summary(model)$p.pv["avg_airtmp:avg_precip"]
  }
}
pval.avg_airtmp.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_airtmp.summer[i,"significant"] = if_else(length(pval.avg_airtmp.summer[i,][pval.avg_airtmp.summer[i,]<0.05/10]) == 11,1,0)
}

coefs.summer.avg$sig_airtmp_precip = ifelse(pval.avg_airtmp.summer$significant== 0, NA, coefs.summer.avg$avg_airtmp)

### Interaction temperature:soil water

coef.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_airtmp.summer) = levels(hydro$member)

pval.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_airtmp.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_airtmp.summer[level, mb] = coefficients(model)["avg_airtmp:avg_soilwater"]
    pval.avg_airtmp.summer[level, mb] = summary(model)$p.pv["avg_airtmp:avg_soilwater"]
  }
}
pval.avg_airtmp.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_airtmp.summer[i,"significant"] = if_else(length(pval.avg_airtmp.summer[i,][pval.avg_airtmp.summer[i,]<0.05/10]) == 11,1,0)
}

coefs.summer.avg$sig_airtmp_soilwater = ifelse(pval.avg_airtmp.summer$significant== 0, NA, coefs.summer.avg$avg_airtmp)

### Interaction temperature:snow storage

coef.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(coef.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(coef.avg_airtmp.summer) = levels(hydro$member)

pval.avg_airtmp.summer = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
rownames(pval.avg_airtmp.summer) = levels(hydro$name_waterlevel)[-30]
colnames(pval.avg_airtmp.summer) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level[-30]){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_summer/", mb,"_summer_", level,".RDS"))
    
    coef.avg_airtmp.summer[level, mb] = coefficients(model)["avg_airtmp:avg_snowstorage"]
    pval.avg_airtmp.summer[level, mb] = summary(model)$p.pv["avg_airtmp:avg_snowstorage"]
  }
}
pval.avg_airtmp.summer$significant = rep(0,97)

for (i in c(1:97)){
  pval.avg_airtmp.summer[i,"significant"] = if_else(length(pval.avg_airtmp.summer[i,][pval.avg_airtmp.summer[i,]<0.05/10]) == 11 &
                                                      !is.na(pval.avg_airtmp.summer[i,1]),1,0)
}
coefs.summer.avg$sig_airtmp_snowstorage = ifelse(pval.avg_airtmp.summer$significant== 0, NA, coefs.summer.avg$avg_airtmp)

write.table(coefs.summer.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coef/summer/coefs.summer.avg", sep="\t", col.names = TRUE, row.names = TRUE)


################################################################################
################################ winter ########################################
################################################################################

### Airtmp

coef.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_airtmp.winter) = levels(hydro$member)

pval.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_airtmp.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_airtmp.winter[level, mb] = coefficients(model)["avg_airtmp"]
    pval.avg_airtmp.winter[level, mb] = summary(model)$p.pv["avg_airtmp"]
  }
}

pval.avg_airtmp.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_airtmp.winter[i,"significant"] = if_else(length(pval.avg_airtmp.winter[i,][pval.avg_airtmp.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_airtmp = ifelse(pval.avg_airtmp.winter$significant== 0, NA, coefs.winter.avg$avg_airtmp)

### Precip

coef.avg_precip.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_precip.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_precip.winter) = levels(hydro$member)

pval.avg_precip.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_precip.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_precip.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_precip.winter[level, mb] = coefficients(model)["avg_precip"]
    pval.avg_precip.winter[level, mb] = summary(model)$p.pv["avg_precip"]
  }
}

pval.avg_precip.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_precip.winter[i,"significant"] = if_else(length(pval.avg_precip.winter[i,][pval.avg_precip.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_precip = ifelse(pval.avg_precip.winter$significant== 0, NA, coefs.winter.avg$avg_precip)

### Soil Water

coef.avg_soilwater.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_soilwater.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_soilwater.winter) = levels(hydro$member)

pval.avg_soilwater.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_soilwater.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_soilwater.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_soilwater.winter[level, mb] = coefficients(model)["avg_soilwater"]
    pval.avg_soilwater.winter[level, mb] = summary(model)$p.pv["avg_soilwater"]
  }
}

pval.avg_soilwater.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_soilwater.winter[i,"significant"] = if_else(length(pval.avg_soilwater.winter[i,][pval.avg_soilwater.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_soilwater = ifelse(pval.avg_soilwater.winter$significant== 0, NA, coefs.winter.avg$avg_soilwater)

### Snow Storage

coef.avg_snowstorage.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_snowstorage.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_snowstorage.winter) = levels(hydro$member)

pval.avg_snowstorage.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_snowstorage.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_snowstorage.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_snowstorage.winter[level, mb] = coefficients(model)["avg_snowstorage"]
    pval.avg_snowstorage.winter[level, mb] = summary(model)$p.pv["avg_snowstorage"]
  }
}

pval.avg_snowstorage.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_snowstorage.winter[i,"significant"] = if_else(length(pval.avg_snowstorage.winter[i,][pval.avg_snowstorage.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_snowstorage = ifelse(pval.avg_snowstorage.winter$significant== 0, NA, coefs.winter.avg$avg_snowstorage)

### Relative Humidity

coef.avg_relhum.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_relhum.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_relhum.winter) = levels(hydro$member)

pval.avg_relhum.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_relhum.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_relhum.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_relhum.winter[level, mb] = coefficients(model)["avg_relhum"]
    pval.avg_relhum.winter[level, mb] = summary(model)$p.pv["avg_relhum"]
  }
}

pval.avg_relhum.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_relhum.winter[i,"significant"] = if_else(length(pval.avg_relhum.winter[i,][pval.avg_relhum.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_relhum = ifelse(pval.avg_relhum.winter$significant== 0, NA, coefs.winter.avg$avg_relhum)

### Radiation

coef.avg_glorad.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_glorad.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_glorad.winter) = levels(hydro$member)

pval.avg_glorad.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_glorad.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_glorad.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_glorad.winter[level, mb] = coefficients(model)["avg_glorad"]
    pval.avg_glorad.winter[level, mb] = summary(model)$p.pv["avg_glorad"]
  }
}

pval.avg_glorad.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_glorad.winter[i,"significant"] = if_else(length(pval.avg_glorad.winter[i,][pval.avg_glorad.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_glorad = ifelse(pval.avg_glorad.winter$significant== 0, NA, coefs.winter.avg$avg_glorad)

### Year

coef.YY.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.YY.winter) = levels(hydro$name_waterlevel)
colnames(coef.YY.winter) = levels(hydro$member)

pval.YY.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.YY.winter) = levels(hydro$name_waterlevel)
colnames(pval.YY.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.YY.winter[level, mb] = coefficients(model)["YY"]
    pval.YY.winter[level, mb] = summary(model)$p.pv["YY"]
  }
}

pval.YY.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.YY.winter[i,"significant"] = if_else(length(pval.YY.winter[i,][pval.YY.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_YY = ifelse(pval.YY.winter$significant== 0, NA, coefs.winter.avg$YY)

### Interaction temperature:precipitation

coef.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_airtmp.winter) = levels(hydro$member)

pval.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_airtmp.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_airtmp.winter[level, mb] = coefficients(model)["avg_airtmp:avg_precip"]
    pval.avg_airtmp.winter[level, mb] = summary(model)$p.pv["avg_airtmp:avg_precip"]
  }
}

pval.avg_airtmp.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_airtmp.winter[i,"significant"] = if_else(length(pval.avg_airtmp.winter[i,][pval.avg_airtmp.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_airtmp_precip = ifelse(pval.avg_airtmp.winter$significant== 0, NA, coefs.winter.avg$avg_airtmp)

### Interaction temperature:soil water

coef.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_airtmp.winter) = levels(hydro$member)

pval.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_airtmp.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_airtmp.winter[level, mb] = coefficients(model)["avg_airtmp:avg_soilwater"]
    pval.avg_airtmp.winter[level, mb] = summary(model)$p.pv["avg_airtmp:avg_soilwater"]
  }
}

pval.avg_airtmp.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_airtmp.winter[i,"significant"] = if_else(length(pval.avg_airtmp.winter[i,][pval.avg_airtmp.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_airtmp_soilwater = ifelse(pval.avg_airtmp.winter$significant== 0, NA, coefs.winter.avg$avg_airtmp)

### Interaction temperature:snow storage

coef.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(coef.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(coef.avg_airtmp.winter) = levels(hydro$member)

pval.avg_airtmp.winter = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
rownames(pval.avg_airtmp.winter) = levels(hydro$name_waterlevel)
colnames(pval.avg_airtmp.winter) = levels(hydro$member)

for (mb in member){
  print(mb)
  for (level in w.level){
    model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_winter/", mb,"_winter_", level,".RDS"))
    
    coef.avg_airtmp.winter[level, mb] = coefficients(model)["avg_airtmp:avg_snowstorage"]
    pval.avg_airtmp.winter[level, mb] = summary(model)$p.pv["avg_airtmp:avg_snowstorage"]
  }
}

pval.avg_airtmp.winter$significant = rep(0,98)

for (i in c(1:98)){
  pval.avg_airtmp.winter[i,"significant"] = if_else(length(pval.avg_airtmp.winter[i,][pval.avg_airtmp.winter[i,]<0.05/10]) == 11,1,0)
}

coefs.winter.avg$sig_airtmp_snowstorage = ifelse(pval.avg_airtmp.winter$significant== 0, NA, coefs.winter.avg$avg_airtmp)

write.table(coefs.winter.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coef/winter/coefs.winter.avg", sep="\t", col.names = TRUE, row.names = TRUE)


