############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### ROC Analysis - Visualization

## Load libraries
library(data.table)
library(dplyr)
library(mgcv)
library(pROC)

## Load tables
tpr.summer.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TPR/tpr.summer.avg.txt", data.table = FALSE)[,-1]
fpr.summer.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/FPR/fpr.summer.avg.txt", data.table = FALSE)[,-1]
tdr.summer.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TDR/tdr.summer.avg.txt", data.table = FALSE)[,-1]

tpr.winter.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TPR/tpr.winter.avg.txt", data.table = FALSE)[,-1]
fpr.winter.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/FPR/fpr.winter.avg.txt", data.table = FALSE)[,-1]
tdr.winter.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/TDR/tdr.winter.avg.txt", data.table = FALSE)[,-1]

################################### summer #####################################

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/cutoff_summer.pdf")
par(mfrow = c(2,2))
plot(seq(0.1,0.9, by = 0.1), tpr.summer.avg[4,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Donau-Kelheimwinzer")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.summer.avg[4,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.summer.avg[4,c(2:10)], type = "l", col = "blue")
lines(rep(0.35, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.4, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("bottomright",legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.35, 0, "0.35", cex = 0.4)
text(0.4, 0, "0.4", cex = 0.4)

plot(seq(0.1,0.9, by = 0.1), tpr.summer.avg[97,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Sächsische-Saale-Hof")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.summer.avg[97,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.summer.avg[97,c(2:10)], type = "l", col = "blue")
lines(rep(0.35, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.45, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("bottomright", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.35, 0, "0.35", cex = 0.4)
text(0.45, 0, "0.45", cex = 0.4)

plot(seq(0.1,0.9, by = 0.1), tpr.summer.avg[48,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Main-Würzburg")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.summer.avg[48,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.summer.avg[48,c(2:10)], type = "l", col = "blue")
lines(rep(0.3, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.45, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("bottomright", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.3, 0, "0.3", cex = 0.4)
text(0.45, 0, "0.45", cex = 0.4)

plot(seq(0.1,0.9, by = 0.1), tpr.summer.avg[73,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Inn-Innsbruck")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.summer.avg[73,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.summer.avg[73,c(2:10)], type = "l", col = "blue")
lines(rep(0.3, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.35, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("right", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.3, 0, "0.3", cex = 0.4)
text(0.35, 0, "0.35", cex = 0.4)

dev.off()

## Plot of one example ROC curve
model = readRDS(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/kbe/kbe_summer/kbe_summer_X30801.RDS"))

# Test set for evaluation of auc
data_eval= data_summer[data_summer$member == "kbt" &
                         data_summer$name_waterlevel == "X30801" &
                         !is.na(data_summer$avg_soilwater),]
data_eval$preds = predict.gam(model, type = "response", newdata = data_eval)

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/ROC_X30801_summer.pdf")
auc(roc(lowlevel~preds, data = data_eval, plot = T, direction = "<", main = "ROC curve for catchment Sanna-Landeck-Bruggen in summer"))
dev.off()


################################### winter #####################################

pdf( file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/cutoff_winter.pdf")
par(mfrow = c(2,2))
plot(seq(0.1,0.9, by = 0.1), tpr.winter.avg[4,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Donau-Kelheimwinzer")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.winter.avg[4,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.winter.avg[4,c(2:10)], type = "l", col = "blue")
lines(rep(0.25, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.35, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("right", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.25, 0, "0.25", cex = 0.4)
text(0.35, 0, "0.35", cex = 0.4)

plot(seq(0.1,0.9, by = 0.1), tpr.winter.avg[97,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Sächsische-Saale-Hof")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.winter.avg[97,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.winter.avg[97,c(2:10)], type = "l", col = "blue")
lines(rep(0.25, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.35, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("bottomright", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.25, 0, "0.25", cex = 0.4)
text(0.35, 0, "0.35", cex = 0.4)

plot(seq(0.1,0.9, by = 0.1), tpr.winter.avg[48,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Main-Würzburg")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.winter.avg[48,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.winter.avg[48,c(2:10)], type = "l", col = "blue")
lines(rep(0.4, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.3, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("bottomright", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.4, 0, "0.4", cex = 0.4)
text(0.3, 0, "0.3", cex = 0.4)

plot(seq(0.1,0.9, by = 0.1), tpr.winter.avg[73,c(2:10)], type = "l", ylim = c(0,1), xlab = "threshold", ylab = "", main = "Inn-Innsbruck")
lines(seq(0.1,0.9, by = 0.1), 1-fpr.winter.avg[73,c(2:10)], type = "l", col = "red")
lines(seq(0.1,0.9, by = 0.1), tdr.winter.avg[73,c(2:10)], type = "l", col = "blue")
lines(rep(0.25, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
lines(rep(0.35, 9),seq(0.05,1, length.out = 9), type = "l", col = "darkgrey", lty=2, lwd = 2)
legend("right", legend = c("TPR", "1-FPR", "TDR"), col = c("black", "red", "blue"),lty = 1, cex = 0.8)
text(0.25, 0, "0.25", cex = 0.4)
text(0.35, 0, "0.35", cex = 0.4)

dev.off()
