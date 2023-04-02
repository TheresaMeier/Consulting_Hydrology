############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Visualization of the climate scenarios

## Load libraries
library(data.table)
library(sp)
library(rgdal)

## Load the data

map = readOGR(
  dsn = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/shapefile/",
  layer = "EZG_OHNE_Reservoir_UTM32",
  verbose = FALSE
)

data_summer <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro_summer.txt", data.table = FALSE)[,-1]

data_summer <- data_summer %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

data_winter <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro_winter.txt", data.table = FALSE)[,-1]

data_winter <- data_winter %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel))

member = levels(data_summer$member)

## Evaluation tables

for (scen in c("_original", "_scenario1", "_scenario2", "_scenario3", "_scenario4", "_scenario5", "_scenario6", "_scenario7")){
  # summer
  assign(paste0("eval_summer", scen),  fread(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/summer",scen, ".txt"), data.table = FALSE)[,-1])
  df_eval = get(paste0("eval_summer", scen))
  rownames(df_eval) = levels(data_summer$name_waterlevel)[-30]
  df_eval[nrow(df_eval)+1,] = NA
  rownames(df_eval)[nrow(df_eval)] = levels(data_summer$name_waterlevel)[30]
  assign(paste0("eval_summer", scen), df_eval)
  
  # winter
  assign(paste0("eval_winter", scen),  fread(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/tables/winter",scen, ".txt"), data.table = FALSE)[,-1])
  df_eval = get(paste0("eval_winter", scen))
  rownames(df_eval) = levels(data_summer$name_waterlevel)
  assign(paste0("eval_winter", scen), df_eval)
  
}

## Order data in the same way as in map

for (season in c("summer", "winter")){
  for (scen in c("_original", "_scenario1", "_scenario2", "_scenario3", "_scenario4", "_scenario5", "_scenario6", "_scenario7")){
    df_eval = get(paste0("eval_", season, scen))
    df_eval = df_eval[order(match(gsub("X","", rownames(df_eval)), c(map$gridcode))),]
    
    assign(paste0("eval_", season, scen), df_eval)
  }
}


## Add values to map

map$lowlevel_summer_mean = eval_summer_original$average
map$lowlevel_summer_scenario1_mean = eval_summer_scenario1$average
map$lowlevel_summer_scenario2_mean = eval_summer_scenario2$average
map$lowlevel_summer_scenario3_mean = eval_summer_scenario3$average
map$lowlevel_summer_scenario4_mean = eval_summer_scenario4$average
map$lowlevel_summer_scenario5_mean = eval_summer_scenario5$average
map$lowlevel_summer_scenario6_mean = eval_summer_scenario6$average
map$lowlevel_summer_scenario7_mean = eval_summer_scenario7$average

map$diff_summer_s1 = eval_summer_scenario1$average - eval_summer_original$average
map$diff_summer_s2 = eval_summer_scenario2$average - eval_summer_original$average
map$diff_summer_s3 = eval_summer_scenario3$average - eval_summer_original$average
map$diff_summer_s4 = eval_summer_scenario4$average - eval_summer_original$average
map$diff_summer_s5 = eval_summer_scenario5$average - eval_summer_original$average
map$diff_summer_s6 = eval_summer_scenario6$average - eval_summer_original$average
map$diff_summer_s7 = eval_summer_scenario7$average - eval_summer_original$average

map$lowlevel_winter_mean = eval_winter_original$average
map$lowlevel_winter_scenario1_mean = eval_winter_scenario1$average
map$lowlevel_winter_scenario2_mean = eval_winter_scenario2$average
map$lowlevel_winter_scenario3_mean = eval_winter_scenario3$average
map$lowlevel_winter_scenario4_mean = eval_winter_scenario4$average
map$lowlevel_winter_scenario5_mean = eval_winter_scenario5$average
map$lowlevel_winter_scenario6_mean = eval_winter_scenario6$average
map$lowlevel_winter_scenario7_mean = eval_winter_scenario7$average

map$diff_winter_s1 = eval_winter_scenario1$average - eval_winter_original$average
map$diff_winter_s2 = eval_winter_scenario2$average - eval_winter_original$average
map$diff_winter_s3 = eval_winter_scenario3$average - eval_winter_original$average
map$diff_winter_s4 = eval_winter_scenario4$average - eval_winter_original$average
map$diff_winter_s5 = eval_winter_scenario5$average - eval_winter_original$average
map$diff_winter_s6 = eval_winter_scenario6$average - eval_winter_original$average
map$diff_winter_s7 = eval_winter_scenario7$average - eval_winter_original$average

################################ Plots #########################################

pal_brown = colorRampPalette(c("#F2E7D4","#bc8013"))
pal_blue = colorRampPalette(c("blue", "#d0d0d0"))
pal_red = colorRampPalette(c("#d0d0d0", "red"))
pal_red_light = colorRampPalette(c("#d0d0d0", "#F28181"))

############################### summer #########################################

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_original.pdf")
spplot(map, zcol = "lowlevel_summer_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario1.pdf")
spplot(map, zcol = "lowlevel_summer_scenario1_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 

dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario2.pdf")
spplot(map, zcol = "lowlevel_summer_scenario2_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario3.pdf")
spplot(map, zcol = "lowlevel_summer_scenario3_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario4.pdf")
spplot(map, zcol = "lowlevel_summer_scenario4_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario5.pdf")
spplot(map, zcol = "lowlevel_summer_scenario5_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario6.pdf")
spplot(map, zcol = "lowlevel_summer_scenario6_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_scenario7.pdf")
spplot(map, zcol = "lowlevel_summer_scenario7_mean", at = seq(from=1, to=71, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s1.pdf")
spplot(map, zcol = "diff_summer_s1", at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8], pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s2.pdf")
spplot(map, zcol = "diff_summer_s2", at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8],pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s3.pdf")
spplot(map, zcol = "diff_summer_s3", at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8], pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s4.pdf")
spplot(map, zcol = "diff_summer_s4",  at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8],pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s5.pdf")
spplot(map, zcol = "diff_summer_s5", at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8],pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s6.pdf")
spplot(map, zcol = "diff_summer_s6",  at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8],pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s7.pdf")
spplot(map, zcol = "diff_summer_s7", at = seq(from=-7, to=30, by=1), col.regions = c(pal_blue(8)[-8],pal_red(30)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/summer_diff_s7_rescaled.pdf")
spplot(map, zcol = "diff_summer_s7", at = seq(from=-7, to=28, by=1), col.regions = c(pal_blue(8)[-8],pal_red(28)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

#################################### winter ####################################
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_original.pdf")
spplot(map, zcol = "lowlevel_winter_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario1.pdf")
spplot(map, zcol = "lowlevel_winter_scenario1_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario2.pdf")
spplot(map, zcol = "lowlevel_winter_scenario2_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario3.pdf")
spplot(map, zcol = "lowlevel_winter_scenario3_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario4.pdf")
spplot(map, zcol = "lowlevel_winter_scenario4_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario5.pdf")
spplot(map, zcol = "lowlevel_winter_scenario5_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario6.pdf")
spplot(map, zcol = "lowlevel_winter_scenario6_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_scenario7.pdf")
spplot(map, zcol = "lowlevel_winter_scenario7_mean", at = seq(from=1, to=40, by=3), col.regions = c(pal_brown(14)), 
       ylab = list("Number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s1.pdf")
spplot(map, zcol = "diff_winter_s1", at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s2.pdf")
spplot(map, zcol = "diff_winter_s2",  at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s3.pdf")
spplot(map, zcol = "diff_winter_s3", at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s4.pdf")
spplot(map, zcol = "diff_winter_s4",  at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s5.pdf")
spplot(map, zcol = "diff_winter_s5",  at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s6.pdf")
spplot(map, zcol = "diff_winter_s6", at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s7.pdf")
spplot(map, zcol = "diff_winter_s7", at = seq(from=-11, to=12, by=1), col.regions = c(pal_blue(12)[-12], pal_red(12)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/winter_diff_s7_rescaled.pdf")
spplot(map, zcol = "diff_winter_s7", at = seq(from=-11, to=4, by=1), col.regions = c(pal_blue(12)[-12], pal_red_light(4)),
       ylab = list("Differences in number of low-flow events", cex = 1.5), 
       par.settings = list(axis.text = list(cex = 1.5))) 
dev.off()

############################# Plots for IWSM ###################################

s1 = spplot(map, zcol = "lowlevel_summer_mean", at = seq(from=1, to=61, by=5), col.regions = c(pal_brown(15)), 
       ylab = list("Number of low-flow events", cex = 1), 
       par.settings = list(axis.text = list(cex = 1))) 
s2 = spplot(map, zcol = "diff_summer_s7", at = seq(from=-7, to=28, by=1), col.regions = c(pal_blue(8)[-8],pal_red(28)),
       ylab = list("Differences in number of events", cex = 1), 
       par.settings = list(axis.text = list(cex = 1)) )

library(ggpubr)

ggarrange(s1,s2, ncol = 2)
ggsave(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Szenarios/plots/meier_scenario.pdf")

