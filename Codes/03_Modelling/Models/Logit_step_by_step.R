############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Step by step derivation of the model for catchment X30801

## Load libraries
library(data.table)
library(mgcv)
library(pROC)
library(ggplot2)

## Load the data
data_summer <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro_summer.txt", data.table = FALSE)[,-1]

data_summer <- data_summer %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

w.level = levels(data_summer$name_waterlevel)
member = levels(data_summer$member)

eval = as.data.frame(matrix(0,10,4))
colnames(eval) = c("model", "AIC", "AUC","dev")
eval$model = c(1:10)

############################### baseline model #################################

logit_model1 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     # avg_airtmp + avg_relhum + avg_glorad +
                     # avg_soilwater + avg_snowstorage +
                     # avg_precip +
                     YY ,#+
                     # # Continous interactions between drivers
                     # avg_airtmp:avg_precip +
                     # avg_airtmp:avg_snowstorage+
                     # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 1, "dev"] = summary(logit_model1)$dev.expl
eval[eval$model == 1, "AIC"] = AIC(logit_model1)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1 = data_summer[data_summer$member == member2[1] &
                                        data_summer$name_waterlevel == "X30801" &
                                                                          !is.na(data_summer$avg_soilwater),]
data_eval_summer_1$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_1)

data_eval_summer_2 = data_summer[data_summer$member == member2[2]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_2$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_2)

data_eval_summer_3 = data_summer[data_summer$member == member2[3]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_3$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_3)

data_eval_summer_4 = data_summer[data_summer$member == member2[4]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_4$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_4)

data_eval_summer_5 = data_summer[data_summer$member == member2[5]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_5$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_5)

data_eval_summer_6 = data_summer[data_summer$member == member2[6]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_6$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_6)

data_eval_summer_7 = data_summer[data_summer$member == member2[7]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_7$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_7)

data_eval_summer_8 = data_summer[data_summer$member == member2[8]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_8$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_8)

data_eval_summer_9 = data_summer[data_summer$member == member2[9]  &
                            data_summer$name_waterlevel == "X30801" &
                            !is.na(data_summer$avg_soilwater),]
data_eval_summer_9$preds = predict.gam(logit_model1, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 1, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 2 #################################

logit_model2 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     # avg_airtmp + avg_relhum + avg_glorad +
                     # avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY ,#+
                   # # Continous interactions between drivers
                   # avg_airtmp:avg_precip +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 2, "dev"] = summary(logit_model2)$dev.expl
eval[eval$model == 2, "AIC"] = AIC(logit_model2)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model2, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 2, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 3 #################################

logit_model3 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + #avg_relhum + avg_glorad +
                     # avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY ,#+
                   # # Continous interactions between drivers
                   # avg_airtmp:avg_precip +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 3, "dev"] = summary(logit_model3)$dev.expl
eval[eval$model == 3, "AIC"] = AIC(logit_model3)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model3, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 3, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 4 #################################

logit_model4 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + #avg_relhum + avg_glorad +
                     avg_soilwater + #avg_snowstorage +
                     avg_precip +
                     YY ,#+
                   # # Continous interactions between drivers
                   # avg_airtmp:avg_precip +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 4, "dev"] = summary(logit_model4)$dev.expl
eval[eval$model == 4, "AIC"] = AIC(logit_model4)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model4, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 4, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 5 #################################

logit_model5 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + #avg_relhum + avg_glorad +
                     avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY ,#+
                   # # Continous interactions between drivers
                   # avg_airtmp:avg_precip +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 5, "dev"] = summary(logit_model5)$dev.expl
eval[eval$model == 5, "AIC"] = AIC(logit_model5)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model5, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 5, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 6 #################################

logit_model6 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + avg_relhum + #avg_glorad +
                     avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY ,#+
                   # # Continous interactions between drivers
                   # avg_airtmp:avg_precip +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 6, "dev"] = summary(logit_model6)$dev.expl
eval[eval$model == 6, "AIC"] = AIC(logit_model6)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model6, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 6, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))


############################### model 7 #################################

logit_model7 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + avg_relhum + avg_glorad +
                     avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY ,#+
                   # # Continous interactions between drivers
                   # avg_airtmp:avg_precip +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 7, "dev"] = summary(logit_model7)$dev.expl
eval[eval$model == 7, "AIC"] = AIC(logit_model7)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model7, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 7, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))


############################### model 8 #################################

logit_model8 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + avg_relhum + avg_glorad +
                     avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY +
                     # Continous interactions between drivers
                     avg_airtmp:avg_precip,# +
                   # avg_airtmp:avg_snowstorage+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 8, "dev"] = summary(logit_model8)$dev.expl
eval[eval$model == 8, "AIC"] = AIC(logit_model8)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model8, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 8, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 9 #################################

logit_model9 = gam(lowlevel ~ 
                     # Main effects for all drivers
                     avg_airtmp + avg_relhum + avg_glorad +
                     avg_soilwater + avg_snowstorage +
                     avg_precip +
                     YY +
                     # Continous interactions between drivers
                     avg_airtmp:avg_precip +
                     avg_airtmp:avg_snowstorage,#+
                   # avg_airtmp:avg_soilwater,
                   family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 9, "dev"] = summary(logit_model9)$dev.expl
eval[eval$model == 9, "AIC"] = AIC(logit_model9)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model9, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 9, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                    auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

############################### model 10 #################################

logit_model10 = gam(lowlevel ~ 
                      # Main effects for all drivers
                      avg_airtmp + avg_relhum + avg_glorad +
                      avg_soilwater + avg_snowstorage +
                      avg_precip +
                      YY +
                      # Continous interactions between drivers
                      avg_airtmp:avg_precip +
                      avg_airtmp:avg_snowstorage +
                      avg_airtmp:avg_soilwater,
                    family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval[eval$model == 10, "dev"] = summary(logit_model10)$dev.expl
eval[eval$model == 10, "AIC"] = AIC(logit_model10)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model10, type = "response", newdata = data_eval_summer_9)

eval[eval$model == 10, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                     auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

################################################################################
######################## Evaluation: other interactions ######################## 
################################################################################

eval_alternative = as.data.frame(matrix(0, nrow = 2, ncol = 4))
colnames(eval_alternative) = c("model", "AIC", "AUC","dev")
eval_alternative$model = c(11,12)


############################### model 11 #################################

logit_model11 = gam(lowlevel ~ 
                      # Main effects for all drivers
                      avg_airtmp + avg_relhum + avg_glorad +
                      avg_soilwater + avg_snowstorage +
                      avg_precip +
                      YY +
                      # Continous interactions between drivers
                      avg_airtmp:avg_precip +
                      avg_airtmp:avg_snowstorage +
                      avg_precip:avg_soilwater,
                    family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval_alternative[eval_alternative$model == 11, "dev"] = summary(logit_model11)$dev.expl
eval_alternative[eval_alternative$model == 11, "AIC"] = AIC(logit_model11)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model11, type = "response", newdata = data_eval_summer_9)

eval_alternative[eval_alternative$model == 11, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))


############################### model 12 #################################

logit_model12 = gam(lowlevel ~ 
                      # Main effects for all drivers
                      avg_airtmp + avg_relhum + avg_glorad +
                      avg_soilwater + avg_snowstorage +
                      avg_precip +
                      YY +
                      # Continous interactions between drivers
                      avg_airtmp:avg_precip +
                      avg_airtmp:avg_snowstorage +
                      avg_precip:avg_snowstorage,
                    family = binomial, data = data_summer[data_summer$member == "kbe" & data_summer$name_waterlevel == "X30801",])

eval_alternative[eval_alternative$model == 12, "dev"] = summary(logit_model12)$dev.expl
eval_alternative[eval_alternative$model == 12, "AIC"] = AIC(logit_model12)

# test sets for evaluation of auc
member2 = member[member != "kbe"]
data_eval_summer_1$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_1)
data_eval_summer_2$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_2)
data_eval_summer_3$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_3)
data_eval_summer_4$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_4)
data_eval_summer_5$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_5)
data_eval_summer_6$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_6)
data_eval_summer_7$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_7)
data_eval_summer_8$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_8)
data_eval_summer_9$preds = predict.gam(logit_model12, type = "response", newdata = data_eval_summer_9)

eval_alternative[eval_alternative$model == 12, "AUC"] = mean(auc(roc(lowlevel ~ preds, data = data_eval_summer_1)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_2)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_3)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_4)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_5)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_6)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_7)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_8)),
                                                             auc(roc(lowlevel ~ preds, data = data_eval_summer_9)))

################################################################################
############################### Visualization ##################################
################################################################################

ggplot(data=eval, aes(x=model,  y=AIC)) +
  geom_line() + theme_bw() + ylim(c(850,2600)) + scale_x_continuous(breaks = seq(1,10, by=1)) +
  xlab("Model") +
  geom_vline(xintercept = 1, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 2, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 3, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 4, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 5, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 6, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 7, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 8, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 9, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 10, linetype="dotted", 
             color = "darkblue", size=0.5)  + 
  geom_text(aes(x=1.3, label=as.character(round(eval$AIC[1],0)), y=2580), colour="darkblue", angle=0, size = 5) +
  geom_text(aes(x=9.8, label=as.character(round(eval$AIC[10],0)), y=920), colour="darkblue", angle=0, size = 5) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Evaluation/AIC.pdf")

ggplot(data=eval, aes(x=model,  y=AUC)) +
  geom_line() + theme_bw() + scale_x_continuous(breaks = seq(1,10, by=1)) + ylim(c(0.59,1)) +
  xlab("Model") +
  geom_vline(xintercept = 1, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 2, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 3, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 4, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 5, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 6, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 7, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 8, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 9, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 10, linetype="dotted", 
             color = "darkblue", size=0.5)  + 
  geom_text(aes(x=1.25, label=as.character(round(eval$AUC[1],2)), y=0.609), colour="darkblue", angle=0, size = 5) +
  geom_text(aes(x=9.8, label=as.character(round(eval$AUC[10],2)), y=0.98), colour="darkblue", angle=0, size = 5) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Evaluation/AUC.pdf")

  
options("scipen"=100, "digits"=4)
ggplot(data=eval, aes(x=model,  y=dev)) +
  geom_line() + theme_bw() + scale_x_continuous(breaks = seq(1,10, by=1)) + 
  ylim(c(0,0.7)) + ylab("Deviance explained") + xlab("Model")+
  geom_vline(xintercept = 1, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 2, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 3, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 4, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 5, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 6, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 7, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 8, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 9, linetype="dotted", 
             color = "darkblue", size=0.5) +
  geom_vline(xintercept = 10, linetype="dotted", 
             color = "darkblue", size=0.5)  + 
  geom_text(aes(x=1.4, label=as.character(round(eval$dev[1],4)), y=0.01), colour="darkblue", angle=0, size = 5) +
  geom_text(aes(x=9.8, label=as.character(round(eval$dev[10],2)), y=0.68), colour="darkblue", angle=0, size = 5) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 

ggsave("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Evaluation/dev.pdf")

