############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### ROC events - Visualization for member "kbt"

## Load libraries
library(data.table)
library(dplyr)
library(rgdal)
library(sp)

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

## Analysis for 2020 and member "kbt"
data_summer_comp = data_summer[data_summer$YY == 2020, c("date", "MM", "DD", "member", "name_waterlevel", "lowlevel", "lowlevel_intensity")]
data_summer_comp_avg = data_summer_comp[data_summer_comp$member == "kbt",]
data_summer_comp_avg$lowlevel_intensity2 = ifelse(data_summer_comp_avg$lowlevel_intensity<0, data_summer_comp_avg$lowlevel_intensity, NA)
data_summer_comp_avg$lowlevel_intensity2 = ifelse(data_summer_comp_avg$name_waterlevel == "X10702", NA,data_summer_comp_avg$lowlevel_intensity2)

data_winter_comp = data_winter[data_winter$YY == 2020, c("date", "MM", "DD", "member", "name_waterlevel", "lowlevel", "lowlevel_intensity")]
data_winter_comp_avg = data_winter_comp[data_winter_comp$member == "kbt",]
data_winter_comp_avg$lowlevel_intensity2 = ifelse(data_winter_comp_avg$lowlevel_intensity<0, data_winter_comp_avg$lowlevel_intensity, NA)
data_winter_comp_avg$lowlevel_intensity2 = ifelse(data_winter_comp_avg$name_waterlevel == "X10702", NA,data_winter_comp_avg$lowlevel_intensity2)

## Load event tables

events.summer.kbt <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/events/summer/events.summer.kbt.txt", data.table = FALSE)[,-1]
for (cut in seq(0.1,0.9, by=0.1)){
  events.summer.kbt[nrow(events.summer.kbt)+1,] =c("X10702", cut, 0, rep(NA,184))
}
events.winter.kbt <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/tables/events/winter/events.winter.kbt.txt", data.table = FALSE)[,-1]

## Load map
map = readOGR(
  dsn = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/shapefile/",
  layer = "EZG_OHNE_Reservoir_UTM32",
  verbose = FALSE
)

## Sort data according to grid code
data_summer_comp_avg = data_summer_comp_avg[order(match(gsub("X","", data_summer_comp_avg$name_waterlevel), c(map$gridcode))),]
events.summer.kbt = events.summer.kbt[order(match(gsub("X","", events.summer.kbt$level), c(map$gridcode))),]

data_winter_comp_avg = data_winter_comp_avg[order(match(gsub("X","", data_winter_comp_avg$name_waterlevel), c(map$gridcode))),]
events.winter.kbt = events.winter.kbt[order(match(gsub("X","", events.winter.kbt$level), c(map$gridcode))),]

#################################### summer ####################################

## include events according to threshold
map$roc_0.1 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.1,]$sum)
map$roc_0.2 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.2,]$sum)
map$roc_0.3 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.3,]$sum)
map$roc_0.4 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.4,]$sum)
map$roc_0.5 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.5,]$sum)
map$roc_0.6 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.6,]$sum)
map$roc_0.7 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.7,]$sum)
map$roc_0.8 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.8,]$sum)
map$roc_0.9 = as.numeric(events.summer.kbt[events.summer.kbt$threshold == 0.9,]$sum)

pal_brown = colorRampPalette(c("#F2E7D4","#bc8013"))

## Plot maps according to threshold
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.1.pdf")
spplot(map, zcol="roc_0.1", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.1", cex = 1.5), par.settings = list(
  axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.2.pdf")
spplot(map, zcol="roc_0.2", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.2", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.3.pdf")
spplot(map, zcol="roc_0.3", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.3", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.4.pdf")
spplot(map, zcol="roc_0.4", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.4", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.5.pdf")
spplot(map, zcol="roc_0.5", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.5", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.6.pdf")
spplot(map, zcol="roc_0.6", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.6", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.7.pdf")
spplot(map, zcol="roc_0.7", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.7", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.8.pdf")
spplot(map, zcol="roc_0.8", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.8", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_0.9.pdf")
spplot(map, zcol="roc_0.9", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("Days of predicted low flow - Threshold 0.9", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

## Include "real events"

data_summer_comp_events <- data_summer_comp_avg %>%
  group_by(name_waterlevel) %>%
  summarise(lowlevel = sum(lowlevel))

data_summer_comp_intensity <- data_summer_comp_avg %>%
  group_by(name_waterlevel) %>%
  summarise(lowlevel_intensity = min(lowlevel_intensity))

data_summer_comp_intensity$lowlevel_intensity = ifelse(data_summer_comp_events$lowlevel == 0, NA,data_summer_comp_intensity$lowlevel_intensity)

data_summer_comp_events = data_summer_comp_events[order(match(gsub("X","", data_summer_comp_events$name_waterlevel), c(map$gridcode))),]
data_summer_comp_intensity = data_summer_comp_intensity[order(match(gsub("X","", data_summer_comp_intensity$name_waterlevel), c(map$gridcode))),]

map$true_events = data_summer_comp_events$lowlevel
map$true_intensity_min = ifelse(data_summer_comp_intensity$lowlevel_intensity<0,data_summer_comp_intensity$lowlevel_intensity,NA)

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_true.pdf")
spplot(map, zcol = "true_events", at = seq(from=1, to=200, by=10), col.regions = c(pal_brown(20)),
       ylab = list("True number of days of low flow", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/summer/summer_intensity.pdf")
spplot(map, zcol = "true_intensity_min", xlab = " Maximal intensity of true days of low flow in summer of 2020")
dev.off()

#################################### winter ####################################

## Include events according to threshold
map$roc_0.1 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.1,]$sum)
map$roc_0.2 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.2,]$sum)
map$roc_0.3 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.3,]$sum)
map$roc_0.4 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.4,]$sum)
map$roc_0.5 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.5,]$sum)
map$roc_0.6 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.6,]$sum)
map$roc_0.7 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.7,]$sum)
map$roc_0.8 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.8,]$sum)
map$roc_0.9 = as.numeric(events.winter.kbt[events.winter.kbt$threshold == 0.9,]$sum)

## Plot maps according to threshold
pal_brown = colorRampPalette(c("#F2E7D4","#bc8013"))

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.1.pdf")
spplot(map, zcol="roc_0.1", at = seq(from=1, to=190, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.1")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.2.pdf")
spplot(map, zcol="roc_0.2", at = seq(from=1, to=190, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.2")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.3.pdf")
spplot(map, zcol="roc_0.3", at = seq(from=1, to=140, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.3")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.4.pdf")
spplot(map, zcol="roc_0.4", at = seq(from=1, to=140, by=10), col.regions = c(pal_brown(19)), ylab = list("Days of predicted low flow - Threshold 0.4", cex = 1.5), par.settings = list(
  axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.5.pdf")
spplot(map, zcol="roc_0.5", at = seq(from=1, to=140, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.5")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.6.pdf")
spplot(map, zcol="roc_0.6", at = seq(from=1, to=190, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.6")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.7.pdf")
spplot(map, zcol="roc_0.7", at = seq(from=1, to=190, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.7")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.8.pdf")
spplot(map, zcol="roc_0.8", at = seq(from=1, to=190, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.8")
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_0.9.pdf")
spplot(map, zcol="roc_0.9", at = seq(from=1, to=190, by=10), col.regions = c(pal_brown(19)), xlab = "Days of predicted low flow - Winter 2020 - Threshold 0.9")
dev.off()

## Include "real events"

data_winter_comp_events <- data_winter_comp_avg %>%
  group_by(name_waterlevel) %>%
  summarise(lowlevel = sum(lowlevel))

data_winter_comp_intensity <- data_winter_comp_avg %>%
  group_by(name_waterlevel) %>%
  summarise(lowlevel_intensity = min(lowlevel_intensity))

data_winter_comp_intensity$lowlevel_intensity = ifelse(data_winter_comp_events$lowlevel == 0, NA,data_winter_comp_intensity$lowlevel_intensity)

data_winter_comp_events = data_winter_comp_events[order(match(gsub("X","", data_winter_comp_events$name_waterlevel), c(map$gridcode))),]
data_winter_comp_intensity = data_winter_comp_intensity[order(match(gsub("X","", data_winter_comp_intensity$name_waterlevel), c(map$gridcode))),]

map$true_events = data_winter_comp_events$lowlevel
map$true_intensity_min = ifelse(data_winter_comp_intensity$lowlevel_intensity<0,data_winter_comp_intensity$lowlevel_intensity,NA)

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_true.pdf")
spplot(map, zcol = "true_events", at = seq(from=1, to=140, by=10), col.regions = c(pal_brown(13)),       
       ylab = list("True number of days of low flow", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/ROC Analysis/plots/winter/winter_intensity.pdf")
spplot(map, zcol = "true_intensity_min", xlab = "Maximal Intensity of true days of low flow in winter of 2020")
dev.off()


