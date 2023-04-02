############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### ROC Analysis for determining threshold

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

## Evaluation tables for TPR, FPR and TDR
tpr.summer.kbe = as.data.frame(matrix(data=0, nrow = 97, ncol = 10))
colnames(tpr.summer.kbe) = c("level", seq(0.1,0.9, by= 0.1))
rownames(tpr.summer.kbe) = levels(data_summer$name_waterlevel)[-30]
tpr.summer.kbe$level = levels(data_summer$name_waterlevel)[-30]

tpr.winter.kbe = as.data.frame(matrix(data=0, nrow = 98, ncol = 10))
colnames(tpr.winter.kbe) = c("level", seq(0.1,0.9, by= 0.1))
rownames(tpr.winter.kbe) = levels(data_winter$name_waterlevel)
tpr.winter.kbe$level = levels(data_winter$name_waterlevel)

for (mb in member){
  assign(paste0("tpr.summer.", mb), tpr.summer.kbe)
  assign(paste0("tpr.winter.", mb), tpr.winter.kbe)
  
  assign(paste0("fpr.summer.", mb), tpr.summer.kbe)
  assign(paste0("fpr.winter.", mb), tpr.winter.kbe)
  
  assign(paste0("tdr.summer.", mb), tpr.summer.kbe)
  assign(paste0("tdr.winter.", mb), tpr.winter.kbe)
}

## Perform Analysis
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
      
      for (cut in colnames(tpr.summer.kbe)[c(2:10)]){
        for (i in c(1:9)){
          df = get(paste0("data_eval_", i))
          df$preds_lowlevel <- ifelse(df$preds > as.numeric(cut), 1, 0)
          assign(paste0("data_eval_", i), df)
        }
        
        # TDR
        df_tdr <- get(paste0("tdr.", season, ".", mb))
        df_tdr[level, cut] = mean(c(sum(data_eval_1[data_eval_1$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_1$preds_lowlevel),
                                    sum(data_eval_2[data_eval_2$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_2$preds_lowlevel),
                                    sum(data_eval_3[data_eval_3$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_3$preds_lowlevel),
                                    sum(data_eval_4[data_eval_4$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_4$preds_lowlevel),
                                    sum(data_eval_5[data_eval_5$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_5$preds_lowlevel),
                                    sum(data_eval_6[data_eval_6$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_6$preds_lowlevel),
                                    sum(data_eval_7[data_eval_7$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_7$preds_lowlevel),
                                    sum(data_eval_8[data_eval_8$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_8$preds_lowlevel),
                                    sum(data_eval_9[data_eval_9$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_9$preds_lowlevel)), na.rm = TRUE)
        
        assign(paste0("tdr.", season, ".", mb), df_tdr)
        
        # FPR
        df_tpr <- get(paste0("tpr.", season, ".", mb))
        df_tpr[level, cut] = mean(c(sum(data_eval_1[data_eval_1$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_1$lowlevel),
                                    sum(data_eval_2[data_eval_2$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_2$lowlevel),
                                    sum(data_eval_3[data_eval_3$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_3$lowlevel),
                                    sum(data_eval_4[data_eval_4$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_4$lowlevel),
                                    sum(data_eval_5[data_eval_5$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_5$lowlevel),
                                    sum(data_eval_6[data_eval_6$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_6$lowlevel),
                                    sum(data_eval_7[data_eval_7$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_7$lowlevel),
                                    sum(data_eval_8[data_eval_8$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_8$lowlevel),
                                    sum(data_eval_9[data_eval_9$lowlevel == 1,]$preds_lowlevel)/sum(data_eval_9$lowlevel)),na.rm = TRUE)
        assign(paste0("tpr.", season, ".", mb), df_tpr)
        
        # FPR
        df_fpr <- get(paste0("fpr.", season, ".", mb))
        df_fpr[level, cut] = mean(c(sum(data_eval_1[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_2[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_3[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_4[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_5[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_6[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_7[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_8[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0),
                                    sum(data_eval_9[data_eval_1$lowlevel == 0,]$preds_lowlevel)/sum(data_eval_1$lowlevel == 0)),na.rm = TRUE)
        assign(paste0("fpr.", season, ".", mb), df_fpr)
      }
    }
  }
}
## Store evaluation tables
for (season in c("summer", "winter")){
  for (mb in member){
    write.table(get(paste0("tpr.", season, ".", mb)), file = paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TPR/tpr.", season, ".", mb, ".txt"), sep="\t", col.names = TRUE, row.names = TRUE)
    write.table(get(paste0("fpr.", season, ".", mb)), file = paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/FPR/fpr.", season, ".", mb, ".txt"), sep="\t", col.names = TRUE, row.names = TRUE)
    write.table(get(paste0("tdr.", season, ".", mb)), file = paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TDR/tdr.", season, ".", mb, ".txt"), sep="\t", col.names = TRUE, row.names = TRUE)
  }
}

## Calculate averages
# Summer
tpr.summer.avg = rbindlist(list(tpr.summer.kbe, tpr.summer.kbj, tpr.summer.kbo,
                        tpr.summer.kbt, tpr.summer.kby, tpr.summer.kcd,
                        tpr.summer.kci, tpr.summer.kcn, tpr.summer.kcs,
                        tpr.summer.kcx))[,lapply(.SD, mean),list(level)]

fpr.summer.avg = rbindlist(list(fpr.summer.kbe, fpr.summer.kbj, fpr.summer.kbo,
                                fpr.summer.kbt, fpr.summer.kby, fpr.summer.kcd,
                                fpr.summer.kci, fpr.summer.kcn, fpr.summer.kcs,
                                fpr.summer.kcx))[,lapply(.SD, mean),list(level)]

tdr.summer.avg = rbindlist(list(tdr.summer.kbe, tdr.summer.kbj, tdr.summer.kbo,
                                tdr.summer.kbt, tdr.summer.kby, tdr.summer.kcd,
                                tdr.summer.kci, tdr.summer.kcn, tdr.summer.kcs,
                                tdr.summer.kcx))[,lapply(.SD, mean),list(level)]

write.table(tpr.summer.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TPR/tpr.summer.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(fpr.summer.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/FPR/fpr.summer.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(tdr.summer.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TDR/tdr.summer.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)

# Winter
tpr.winter.avg = rbindlist(list(tpr.winter.kbe, tpr.winter.kbj, tpr.winter.kbo,
                                tpr.winter.kbt, tpr.winter.kby, tpr.winter.kcd,
                                tpr.winter.kci, tpr.winter.kcn, tpr.winter.kcs,
                                tpr.winter.kcx))[,lapply(.SD, mean),list(level)]

fpr.winter.avg = rbindlist(list(fpr.winter.kbe, fpr.winter.kbj, fpr.winter.kbo,
                                fpr.winter.kbt, fpr.winter.kby, fpr.winter.kcd,
                                fpr.winter.kci, fpr.winter.kcn, fpr.winter.kcs,
                                fpr.winter.kcx))[,lapply(.SD, mean),list(level)]

tdr.winter.avg = rbindlist(list(tdr.winter.kbe, tdr.winter.kbj, tdr.winter.kbo,
                                tdr.winter.kbt, tdr.winter.kby, tdr.winter.kcd,
                                tdr.winter.kci, tdr.winter.kcn, tdr.winter.kcs,
                                tdr.winter.kcx))[,lapply(.SD, mean),list(level)]
write.table(tpr.winter.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TPR/tpr.winter.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(fpr.winter.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/FPR/fpr.winter.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)
write.table(tdr.winter.avg, file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TDR/tdr.winter.avg.txt", sep="\t", col.names = TRUE, row.names = TRUE)
