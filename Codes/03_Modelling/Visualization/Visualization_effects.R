############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Visualization of estimated coefficients

## Load libraries
library(data.table)
library(sp)
library(rgdal)
library(cowplot)
library(ggplot2)

## Load data
# Load coefficients of models summer
coefs.summer.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coef/summer/coefs.summer.avg",
                         data.table = FALSE)[,-1]
coefs.summer.avg[nrow(coefs.summer.avg) + 1,] = NA
coefs.summer.avg[nrow(coefs.summer.avg),1] = "10702"

# Load coefficients of models winter
coefs.winter.avg = fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts//Model/Models_waterlevel/tables/coef/winter/coefs.winter.avg",
                         data.table = FALSE)[,-1]

# Load map 
map = readOGR(
  dsn = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/shapefile/",
  layer = "EZG_OHNE_Reservoir_UTM32",
  verbose = FALSE
)

# Sort coefficients according to map gridcode
coefs.summer.avg = coefs.summer.avg[order(match(coefs.summer.avg$name_waterlevel, c(map$gridcode))),]
coefs.winter.avg = coefs.winter.avg[order(match(coefs.winter.avg$name_waterlevel, c(map$gridcode))),]

## Color palette for plots
pal_blue = colorRampPalette(c("blue", "#d0d0d0"))
pal_red = colorRampPalette(c("#d0d0d0", "red"))
pal_red_light = colorRampPalette(c("#d0d0d0", "#FFAAAA"))

################################## precip ######################################

map$precip = coefs.summer.avg$avg_precip
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/precip_summer.pdf")
spplot(map, zcol = "precip", 
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 

dev.off()

map$sig_precip = coefs.summer.avg$sig_precip
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/precip_summer_sig.pdf")
spplot(map, zcol = "sig_precip", 
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - summer - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$precip = coefs.winter.avg$avg_precip
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/precip_winter.pdf")
spplot(map, zcol = "precip",
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_precip = coefs.winter.avg$sig_precip
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/precip_winter_sig.pdf")
spplot(map, zcol = "sig_precip", 
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - winter - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

# Interaction effect with airtmp

map$airtmp_precip = coefs.summer.avg$avg_precip + 
  5 * coefs.summer.avg$`avg_airtmp:avg_precip`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_precip_summer_5more.pdf")
spplot(map, zcol = "airtmp_precip",
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - 5°C more - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.summer.avg$avg_precip -
  5 * coefs.summer.avg$`avg_airtmp:avg_precip`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_precip_summer_5less.pdf")
spplot(map, zcol = "airtmp_precip",
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - 5°C less - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.winter.avg$avg_precip + 
  5 * coefs.winter.avg$`avg_airtmp:avg_precip`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_precip_winter_5more.pdf")
spplot(map, zcol = "airtmp_precip",
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - 5°C more - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.winter.avg$avg_precip -
  5 * coefs.winter.avg$`avg_airtmp:avg_precip`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_precip_winter_5less.pdf")
spplot(map, zcol = "airtmp_precip",
       at = seq(from=-1.8, to=0.2, by=0.01), col.regions = c(pal_blue(180), pal_red_light(20)), ylab = list("log odds - precipitation - 5°C less - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.summer.avg$`avg_airtmp:avg_precip`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_precip_summer.pdf")
spplot(map, zcol = "airtmp_precip",
            at = seq(from=-0.08, to=0.07, by=0.001), col.regions = c(pal_blue(80), pal_red(70)), ylab = list("log odds - temperature:precipitation - summer", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.summer.avg$sig_airtmp_precip
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_precip_summer_sig.pdf")
spplot(map, zcol = "airtmp_precip",
            at = seq(from=-0.08, to=0.07, by=0.001), col.regions = c(pal_blue(80), pal_red(70)), ylab = list("log odds - temperature:precipitation - summer - significance", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.winter.avg$`avg_airtmp:avg_precip`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_precip_winter.pdf")
spplot(map, zcol = "airtmp_precip",
            at = seq(from=-0.08, to=0.07, by=0.001), col.regions = c(pal_blue(80), pal_red(70)), ylab = list("log odds - temperature:precipitation - winter", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_precip = coefs.winter.avg$sig_airtmp_precip
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_precip_winter_sig.pdf")
spplot(map, zcol = "airtmp_precip",
            at = seq(from=-0.08, to=0.07, by=0.001), col.regions = c(pal_blue(80), pal_red(70)), ylab = list("log odds - temperature:precipitation - winter - significance", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

############################## airtmp ##########################################

map$airtmp = coefs.summer.avg$avg_airtmp
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_summer.pdf")
spplot(map, zcol = "airtmp",
       at = seq(from=-2, to=3.6, by=0.05), col.regions = c(pal_blue(40), pal_red(72)), ylab = list("log odds - temperature - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_airtmp = coefs.summer.avg$sig_airtmp
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_summer_sig.pdf")
spplot(map, zcol = "sig_airtmp",
       at = seq(from=-2, to=3.6, by=0.05), col.regions = c(pal_blue(40), pal_red(72)), ylab = list("log odds - temperature - summer - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp = coefs.winter.avg$avg_airtmp
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_winter.pdf")
spplot(map, zcol = "airtmp",
       at = seq(from=-2, to=3.6, by=0.05), col.regions = c(pal_blue(40), pal_red(72)), ylab = list("log odds - temperature - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_winter_rescale.pdf")
spplot(map, zcol = "airtmp",
      at = seq(from=-0.5, to=0.1, by=0.01), col.regions = c(pal_blue(50), pal_red_light(10)),ylab = list("log odds - temperature - winter", cex = 1.5), par.settings = list(
        axis.text = list(cex = 1.5))) 
dev.off()

map$sig_airtmp = coefs.winter.avg$sig_airtmp
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_winter_rescale_sig.pdf")
spplot(map, zcol = "sig_airtmp",
            at = seq(from=-0.5, to=0.1, by=0.01), col.regions = c(pal_blue(50), pal_red_light(10)), ylab = list("log odds - temperature - winter - significance", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

################################## relhum ######################################

map$relhum = 10*coefs.summer.avg$avg_relhum
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/relhum_summer.pdf")
spplot(map, zcol = "relhum",
       at = seq(from=-2.1, to=0.3, by=0.05), col.regions = c(pal_blue(42), pal_red_light(6)), ylab = list("log odds - humidity - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_relhum = 10*coefs.summer.avg$sig_relhum
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/relhum_summer_sig.pdf")
spplot(map, zcol = "sig_relhum",
       at = seq(from=-2.1, to=0.3, by=0.05), col.regions = c(pal_blue(42), pal_red_light(6)), ylab = list("log odds - humidity - summer - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$relhum = 10*coefs.winter.avg$avg_relhum
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/relhum_winter.pdf")
spplot(map, zcol = "relhum",
       at = seq(from=-2.1, to=0.3, by=0.05), col.regions = c(pal_blue(42), pal_red_light(6)), ylab = list("log odds - humidity - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_relhum = 10*coefs.winter.avg$sig_relhum
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/relhum_winter_sig.pdf")
spplot(map, zcol = "sig_relhum",
       at = seq(from=-2.1, to=0.3, by=0.05), col.regions = c(pal_blue(42), pal_red_light(6)), ylab = list("log odds - humidity - winter - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

################################# glorad #######################################

map$glorad = 100*coefs.summer.avg$avg_glorad
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/glorad_summer.pdf")
spplot(map, zcol = "glorad", 
       at = seq(from=-1.5, to=0.6, by=0.01), col.regions = c(pal_blue(150), pal_red(60)), ylab = list("log odds - radiation - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_glorad = 100*coefs.summer.avg$sig_glorad
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/glorad_summer_sig.pdf")
spplot(map, zcol = "sig_glorad", 
       at = seq(from=-1.5, to=0.6, by=0.01), col.regions = c(pal_blue(150), pal_red(60)), ylab = list("log odds - radiation - summer - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$glorad = 100*coefs.winter.avg$avg_glorad
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/glorad_winter.pdf")
spplot(map, zcol = "glorad", 
       at = seq(from=-1.5, to=0.6, by=0.01), col.regions = c(pal_blue(150), pal_red(60)), ylab = list("log odds - radiation - winter ", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_glorad = 100*coefs.winter.avg$sig_glorad
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/glorad_winter_sig.pdf")
spplot(map, zcol = "sig_glorad", 
       at = seq(from=-1.5, to=0.6, by=0.01), col.regions = c(pal_blue(150), pal_red(60)), ylab = list("log odds - radiation - winter - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

############################ soilwater #########################################

map$soilwater= coefs.summer.avg$avg_soilwater
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/soilwater_summer.pdf")
spplot(map, zcol = "soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_soilwater= coefs.summer.avg$sig_soilwater
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/soilwater_summer_sig.pdf")
spplot(map, zcol = "sig_soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - summer - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$soilwater= coefs.winter.avg$avg_soilwater
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/soilwater_winter.pdf")
spplot(map, zcol = "soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_soilwater= coefs.winter.avg$sig_soilwater
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/soilwater_winter_sig.pdf")
spplot(map, zcol = "sig_soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - winter - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

# Interaction effect with temperature

map$airtmp_soilwater = coefs.summer.avg$avg_soilwater + 
  5 * coefs.summer.avg$`avg_airtmp:avg_soilwater`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_soilwater_summer_5more.pdf")
spplot(map, zcol = "airtmp_soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - 5°C more - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.summer.avg$avg_soilwater -
  5 * coefs.summer.avg$`avg_airtmp:avg_soilwater`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_soilwater_summer_5less.pdf")
spplot(map, zcol = "airtmp_soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - 5°C less - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.winter.avg$avg_soilwater + 
  5 * coefs.winter.avg$`avg_airtmp:avg_soilwater`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_soilwater_winter_5more.pdf")
spplot(map, zcol = "airtmp_soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - 5°C more - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.winter.avg$avg_soilwater -
  5 * coefs.winter.avg$`avg_airtmp:avg_soilwater`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_soilwater_winter_5less.pdf")
spplot(map, zcol = "airtmp_soilwater", 
       at = seq(from=-3.6, to=0.4, by=0.05), col.regions = c(pal_blue(72), pal_red_light(8)), ylab = list("log odds - soil water - 5°C less - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.summer.avg$`avg_airtmp:avg_soilwater`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_soilwater_summer.pdf")
spplot(map, zcol = "airtmp_soilwater",
            at = seq(from=-0.05, to=0.2, by=0.001), col.regions = c(pal_blue(50), pal_red(200)), ylab = list("log odds - temperature:soil water - summer", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.summer.avg$sig_airtmp_soilwater
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_soilwater_summer_sig.pdf")
spplot(map, zcol = "airtmp_soilwater",
       at = seq(from=-0.05, to=0.2, by=0.001), col.regions = c(pal_blue(50), pal_red(200)), ylab = list("log odds - temperature:soil water - summer - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.winter.avg$`avg_airtmp:avg_soilwater`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_soilwater_winter.pdf")
spplot(map, zcol = "airtmp_soilwater",
       at = seq(from=-0.05, to=0.2, by=0.001), col.regions = c(pal_blue(50), pal_red(200)), ylab = list("log odds - temperature:soil water - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_soilwater = coefs.winter.avg$sig_airtmp_soilwater
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_soilwater_winter_sig.pdf")
spplot(map, zcol = "airtmp_soilwater",
       at = seq(from=-0.05, to=0.2, by=0.001), col.regions = c(pal_blue(50), pal_red(200)), ylab = list("log odds - temperature:soil water - winter - significance", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

############################## snow storage #####################################

map$snowstorage = coefs.summer.avg$avg_snowstorage
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_summer.pdf")
spplot(map, zcol = "snowstorage", 
       at = seq(from=-7.5, to=0.1, by=0.1), col.regions = c(pal_blue(75), pal_red_light(1)), ylab = list("log odds - snow storage - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$snowstorage = coefs.summer.avg$avg_snowstorage *100
map_sub = map[map$Id > 67 & !(map$Id %in% c(5,6,7,8,9, 54, 60)),]
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_summer_alps10.pdf")
spplot(map_sub, zcol = "snowstorage", 
       at = seq(from=-125, to=0, by=1), col.regions = c(pal_blue(140)),  ylab = list("log odds - snow storage (10cm) - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_snowstorage = coefs.summer.avg$sig_snowstorage
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_summer_sig.pdf")
spplot(map, zcol = "sig_snowstorage", 
             at = seq(from=-7.5, to=0.1, by=0.1), col.regions = c(pal_blue(75), pal_red_light(1)), ylab = list("log odds - snow storage - summer - significance", cex = 1.5), par.settings = list(
               axis.text = list(cex = 1.5))) 
dev.off()

map$snowstorage = coefs.winter.avg$avg_snowstorage 
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_winter.pdf")
spplot(map, zcol = "snowstorage", 
       at = seq(from=-7.5, to=0.1, by=0.1), col.regions = c(pal_blue(75), pal_red_light(1)), ylab = list("log odds - snow storage - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_winter_rescale.pdf")
spplot(map, zcol = "snowstorage", 
       at = seq(from=-2.1, to=0.1, by=0.01), col.regions = c(pal_blue(210), pal_red_light(10)), ylab = list("log odds - snow storage - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$snowstorage = coefs.winter.avg$avg_snowstorage * 100
map_sub = map[map$Id > 67 & !(map$Id %in% c(5,6,7,8,9, 54, 60)),]
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_winter_alps10.pdf")
spplot(map_sub, zcol = "snowstorage", 
       at = seq(from=-17, to=1, by=0.25), col.regions = c(pal_blue(68), pal_red_light(4)),   ylab = list("log odds - snow storage (10cm) - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$sig_snowstorage = coefs.winter.avg$sig_snowstorage
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/snowstorage_winter_rescale_sig.pdf")
spplot(map, zcol = "sig_snowstorage", 
       at = seq(from=-2.1, to=0.1, by=0.01), col.regions = c(pal_blue(210), pal_red_light(10)), ylab = list("log odds - snow storage - winter - significance", cex = 1.5), par.settings = list(
               axis.text = list(cex = 1.5))) 
dev.off()

# Interaction effect with temperature

map$airtmp_snowstorage = coefs.summer.avg$avg_snowstorage + 
  5 * coefs.summer.avg$`avg_airtmp:avg_snowstorage`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_snowstorage_summer_5more.pdf")
spplot(map, zcol = "airtmp_snowstorage",
       at = seq(from=-7.5, to=0.1, by=0.1), col.regions = c(pal_blue(75), pal_red_light(1)), ylab = list("log odds - snow storage - 5°C more - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.summer.avg$avg_snowstorage - 
  5 * coefs.summer.avg$`avg_airtmp:avg_snowstorage`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_snowstorage_summer_5less.pdf")
spplot(map, zcol = "airtmp_snowstorage",
       at = seq(from=-7.5, to=0.1, by=0.1), col.regions = c(pal_blue(75), pal_red_light(1)), ylab = list("log odds - snow storage - 5°C less - summer", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.winter.avg$avg_snowstorage + 
  5 * coefs.winter.avg$`avg_airtmp:avg_snowstorage`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_snowstorage_winter_5more.pdf")
spplot(map, zcol = "airtmp_snowstorage",
       at = seq(from=-2.1, to=0.1, by=0.01), col.regions = c(pal_blue(210), pal_red_light(10)), ylab = list("log odds - snow storage - 5°C more - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.winter.avg$avg_snowstorage - 
  5 * coefs.winter.avg$`avg_airtmp:avg_snowstorage`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/int_airtmp_snowstorage_winter_5less.pdf")
spplot(map, zcol = "airtmp_snowstorage",
       at = seq(from=-2.1, to=0.1, by=0.01), col.regions = c(pal_blue(210), pal_red_light(10)), ylab = list("log odds - snow storage - 5°C less - winter", cex = 1.5), par.settings = list(
         axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.summer.avg$`avg_airtmp:avg_snowstorage`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_snowstorage_summer.pdf")
spplot(map, zcol = "airtmp_snowstorage",
            at = seq(from=-0.1, to=0.45, by=0.01), col.regions = c(pal_blue(10), pal_red(45)), ylab = list("log odds - temperature:snow storage - summer", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.summer.avg$sig_airtmp_snowstorage
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_snowstorage_summer_sig.pdf")
spplot(map, zcol = "airtmp_snowstorage",
             at = seq(from=-0.1, to=0.45, by=0.01), col.regions = c(pal_blue(10), pal_red(45)), ylab = list("log odds - temperature:snow storage - summer - significance", cex = 1.5), par.settings = list(
               axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.winter.avg$`avg_airtmp:avg_snowstorage`
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_snowstorage_winter.pdf")
spplot(map, zcol = "airtmp_snowstorage",
            at = seq(from=-0.1, to=0.03, by=0.001), col.regions = c(pal_blue(100),pal_red_light(30)), ylab = list("log odds - temperature:snow storage - winter", cex = 1.5), par.settings = list(
              axis.text = list(cex = 1.5))) 
dev.off()

map$airtmp_snowstorage = coefs.winter.avg$sig_airtmp_snowstorage
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/airtmp_snowstorage_winter_sig.pdf")
spplot(map, zcol = "airtmp_snowstorage",
              at = seq(from=-0.1, to=0.03, by=0.001), col.regions = c(pal_blue(100),pal_red_light(30)), ylab = list("log odds - temperature:snow storage - winter - significance", cex = 1.5), par.settings = list(
                axis.text = list(cex = 1.5))) 
dev.off()

