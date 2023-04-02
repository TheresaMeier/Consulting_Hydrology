############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### ROC-Analysis - Predictions for summer and winter models
  
## Load libraries
library(data.table)
library(dplyr)
library(mgcv)
library(pROC)

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

# Evaluation tables for number of low-flow events
events.summer.kbe = as.data.frame(matrix(data=0, nrow = 97 * 9, ncol = 2+184))
colnames(events.summer.kbe) = c("level", "threshold", paste(data_summer[data_summer$YY == 2020 & data_summer$name_waterlevel == "X10001" & data_summer$member == "kbe",]$MM, "-", 
                                data_summer[data_summer$YY == 2020 & data_summer$name_waterlevel == "X10001" & data_summer$member == "kbe",]$DD))
events.summer.kbe$level = rep(levels(data_summer$name_waterlevel)[-30],each = 9)
events.summer.kbe$threshold = rep(seq(0.1,0.9,by=0.1),97)

events.winter.kbe = as.data.frame(matrix(data=0, nrow = 98 * 9, ncol = 2+181))
colnames(events.winter.kbe) = c("level", "threshold", paste(data_winter[data_winter$YY == 2020 & data_winter$name_waterlevel == "X10001" & data_winter$member == "kbe",]$MM, "-", 
                                                            data_winter[data_winter$YY == 2020 & data_winter$name_waterlevel == "X10001" & data_winter$member == "kbe",]$DD))
events.winter.kbe$level = rep(levels(data_winter$name_waterlevel),each = 9)
events.winter.kbe$threshold = rep(seq(0.1,0.9,by=0.1),98)

for (mb in member){
  assign(paste0("events.summer.", mb), events.summer.kbe)
  assign(paste0("events.winter.", mb), events.winter.kbe)
}


for (season in c("summer", "winter")){
  for (mb in member){
    print(mb)
    
    if (season == "summer"){
      w.levels = row.names(tpr.summer.kbe)
    }
    else {
      w.levels = row.names(tpr.winter.kbe)
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
                                     !is.na(data_summer$avg_soilwater),]
        }
        else{
          data_eval <- data_winter[data_winter$member == member2[i] &
                                     data_winter$name_waterlevel == level &
                                     !is.na(data_winter$avg_soilwater),]
        }
        data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)
        assign(paste0("data_eval_", i), data_eval)
      }
      data_eval <- NULL
    
      mean_preds = rowMeans(cbind(data_eval_summer_1$preds,data_eval_summer_2$preds,
                                  data_eval_summer_3$preds,data_eval_summer_4$preds,
                                  data_eval_summer_5$preds,data_eval_summer_6$preds,
                                  data_eval_summer_7$preds,data_eval_summer_8$preds,
                                  data_eval_summer_9$preds))
      
      for (cut in seq(0.1,0.9,by=0.1)){
        preds_lowlevel <- ifelse(mean_preds > as.numeric(cut), 1, 0)
        
        df_events <- get(paste0("events.", season, ".", mb))
        df_events[df_events$level == level & df_events$threshold == cut,c(3:186)] = preds_lowlevel
        assign(paste0("events.", season, ".", mb), df_events)
      }
    }
  }
}

for (season in c("summer", "winter")){
  for (mb in member){
    df_events <- get(paste0("events.", season, ".", mb))
    for (i in c(1:nrow(df_events))){
      df_events$sum[i] = sum(df_events[i,c(3:186)])
    }
    df_events = df_events[,c(1,2,187,3:186)]
    assign(paste0("events.", season, ".", mb), df_events)
    write.table(get(paste0("events.", season, ".", mb)), file = paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/events/", season, "/events.", season, ".", mb, ".txt"), sep="\t", col.names = TRUE, row.names = TRUE)
    
  }
}

events.summer.avg = rbindlist(list(events.summer.kbe, events.summer.kbj, events.summer.kbo,
                                events.summer.kbt, events.summer.kby, events.summer.kcd,
                                events.summer.kci, events.summer.kcn, events.summer.kcs,
                                events.summer.kcx))[,lapply(.SD, mean),list(level, threshold)]

events.summer.avg[,c(4:187)] = round(events.summer.avg[,c(4:187)],0)

for (i in c(1:nrow(events.summer.avg))) {
  events.summer.avg$sum[i] = sum(events.summer.avg[i,c(4:187)])
}
write.table(events.summer.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/events/summer/events.summer.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)

events.winter.avg = rbindlist(list(events.winter.kbe, events.winter.kbj, events.winter.kbo,
                                   events.winter.kbt, events.winter.kby, events.winter.kcd,
                                   events.winter.kci, events.winter.kcn, events.winter.kcs,
                                   events.winter.kcx))[,lapply(.SD, mean),list(level, threshold)]

events.winter.avg[,c(4:184)] = round(events.winter.avg[,c(4:184)],0)

for (i in c(1:nrow(events.winter.avg))) {
  events.winter.avg$sum[i] = sum(events.winter.avg[i,c(4:184)])
}
write.table(events.winter.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/events/winter/events.winter.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)
