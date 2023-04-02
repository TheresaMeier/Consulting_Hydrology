############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Predictions for more extreme climate scenarios

## Load required libraries
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
data_summer <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro_summer.txt", data.table = FALSE)[,-1]
 
data_summer <- data_summer %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

data_winter <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro_winter.txt", data.table = FALSE)[,-1]

data_winter <- data_winter %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

member = levels(data_summer$member)

################################################################################
####################### Predictions for actual scenario ########################
## Evaluation table
eval_summer = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer) = levels(data_summer$member)

eval_winter = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter) = levels(data_winter$name_waterlevel)
colnames(eval_winter) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
          
        }
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                     sum(data_eval_summer_2$preds>0.4),
                                     sum(data_eval_summer_3$preds>0.4),
                                     sum(data_eval_summer_4$preds>0.4),
                                     sum(data_eval_summer_5$preds>0.4),
                                     sum(data_eval_summer_6$preds>0.4),
                                     sum(data_eval_summer_7$preds>0.4),
                                     sum(data_eval_summer_8$preds>0.4),
                                     sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season), df_eval)
  }
}

write.table(eval_summer, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_original.txt", sep="\t", col.names = TRUE)
write.table(eval_winter, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_original.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 1 ########################
eval_summer_scenario1 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario1) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario1) = levels(data_summer$member)

eval_winter_scenario1 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario1) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario1) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
          
        }
        data_eval$avg_airtmp = data_eval$avg_airtmp + 3
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario1"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario1"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario1"), df_eval)
  }
}

write.table(eval_summer_scenario1, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario1.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario1, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario1.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 2 ########################

eval_summer_scenario2 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario2) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario2) = levels(data_summer$member)

eval_winter_scenario2 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario2) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario2) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip05
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip15
          
        }
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario2"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario2"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario2"), df_eval)
  }
}

write.table(eval_summer_scenario2, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario2.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario2, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario2.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 3 ########################

eval_summer_scenario3 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario3) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario3) = levels(data_summer$member)

eval_winter_scenario3 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario3) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario3) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip05
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip15
          
        }
        data_eval$avg_airtmp = data_eval$avg_airtmp + 3
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario3"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario3"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario3"), df_eval)
  }
}

write.table(eval_summer_scenario3, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario3.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario3, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario3.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 4 ########################

eval_summer_scenario4 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario4) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario4) = levels(data_summer$member)

eval_winter_scenario4 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario4) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario4) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
        }
        data_eval$avg_snowstorage = data_eval$avg_snowstorage - data_eval$avg_snowstorage_unscaled 
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario4"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario4"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario4"), df_eval)
  }
}

write.table(eval_summer_scenario4, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario4.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario4, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario4.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 5 ########################

eval_summer_scenario5 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario5) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario5) = levels(data_summer$member)

eval_winter_scenario5 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario5) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario5) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
        }
        data_eval$avg_airtmp = data_eval$avg_airtmp + 3
        data_eval$avg_snowstorage = data_eval$avg_snowstorage - data_eval$avg_snowstorage_unscaled 
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario5"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario5"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario5"), df_eval)
  }
}

write.table(eval_summer_scenario5, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario5.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario5, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario5.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 6 ########################

eval_summer_scenario6 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario6) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario6) = levels(data_summer$member)

eval_winter_scenario6 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario6) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario6) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip05
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip15
          
        }
        data_eval$avg_snowstorage = data_eval$avg_snowstorage - data_eval$avg_snowstorage_unscaled 
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario6"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario6"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario6"), df_eval)
  }
}

write.table(eval_summer_scenario6, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario6.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario6, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario6.txt", sep="\t", col.names = TRUE)

####################### Predictions for future scenario 7 ########################

eval_summer_scenario7 = as.data.frame(matrix(data = 0, nrow = 97, ncol = 10))
rownames(eval_summer_scenario7) = levels(data_summer$name_waterlevel)[-30]
colnames(eval_summer_scenario7) = levels(data_summer$member)

eval_winter_scenario7 = as.data.frame(matrix(data = 0, nrow = 98, ncol = 10))
rownames(eval_winter_scenario7) = levels(data_winter$name_waterlevel)
colnames(eval_winter_scenario7) = levels(data_winter$member)

for (season in c("summer", "winter")){
  for (mb in levels(data_summer$member)){
    print(mb)
    if (season == "summer"){
      w.levels = levels(data_summer$name_waterlevel)[-30]
    }
    else {
      w.levels = levels(data_summer$name_waterlevel)
    }
    
    for (level in w.levels){
      print(level)
      model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/", mb,"/", mb,"_", season,"/", mb,"_", season, "_", level,".RDS"))
      # test sets for evaluation of auc
      member2 = member[member != mb]
      for (i in c(1:9)){
        if (season == "summer"){
          data_eval <- data_summer[data_summer$member == member2[i] &
                                     data_summer$name_waterlevel == level &
                                     data_summer$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip05
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     data_winter$YY == 2010,]
          data_eval$avg_precip = data_eval$avg_precip15
          
        }
        data_eval$avg_airtmp = data_eval$avg_airtmp + 3
        data_eval$avg_snowstorage = data_eval$avg_snowstorage - data_eval$avg_snowstorage_unscaled 
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      
      df_eval = get(paste0("eval_", season,"scenario7"))
      df_eval[level,mb] = mean(c(sum(data_eval_summer_1$preds>0.4), 
                                 sum(data_eval_summer_2$preds>0.4),
                                 sum(data_eval_summer_3$preds>0.4),
                                 sum(data_eval_summer_4$preds>0.4),
                                 sum(data_eval_summer_5$preds>0.4),
                                 sum(data_eval_summer_6$preds>0.4),
                                 sum(data_eval_summer_7$preds>0.4),
                                 sum(data_eval_summer_8$preds>0.4),
                                 sum(data_eval_summer_9$preds>0.4)))
      assign(paste0("eval_",season), df_eval)
    }
  }
  for (i in c(1:97)){
    df_eval = get(paste0("eval_", season,"scenario7"))
    df_eval$average[i] = round(mean(as.numeric(df_eval[i,c(1:10)])),0)
    df_eval$median[i] = round(median(as.numeric(df_eval[i,c(1:10)])),0)
    assign(paste0("eval_",season,"scenario7"), df_eval)
  }
}

write.table(eval_summer_scenario7, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer_scenario7.txt", sep="\t", col.names = TRUE)
write.table(eval_winter_scenario7, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter_scenario7.txt", sep="\t", col.names = TRUE)
