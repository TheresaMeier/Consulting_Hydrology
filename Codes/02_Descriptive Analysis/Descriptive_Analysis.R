############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Descriptive Analysis

## Load libraries
library(data.table)
library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
library(ggcorrplot)
library(grDevices)
library(RColorBrewer)

## Load the data 
hydro <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro.txt", data.table = FALSE)[,-1]

hydro <- hydro %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel),
         river = as.factor(river))

map = readOGR(
  dsn = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/shapefile/",
  layer = "EZG_OHNE_Reservoir_UTM32",
  verbose = FALSE
)

# Choose member "kbt" for analysis
data_kbt = hydro %>%
  filter(member == "kbt")

# Data set for yearly averages 
data_mean_YY = aggregate(list(data_kbt$lowlevel_days, data_kbt$glorad, data_kbt$relhum, data_kbt$airtmp, data_kbt$precip, data_kbt$soilwater,data_kbt$snowstorage), by = list(data_kbt$YY,  data_kbt$name_waterlevel), mean, na.rm = TRUE)
colnames(data_mean_YY) = c("YY", "name_waterlevel","lowlevel_days", "glorad", "relhum", "airtmp", "precip", "soilwater","snowstorage")
data_mean_YY$name_waterlevel <- gsub("X", "", data_mean_YY$name_waterlevel)
data_mean_YY = data_mean_YY[order(match(data_mean_YY$name_waterlevel, c(map$gridcode))),]   # order according to gridcode

############################ lowlevel events ###################################

data_lowlevel_summer = as.data.frame(matrix(0, nrow = 98, ncol = 4))
colnames(data_lowlevel_summer) = seq(1990,2020,by=10)
rownames(data_lowlevel_summer) = levels(data_kbt$name_waterlevel)

for (i in colnames(data_lowlevel_summer)){
  for(level in rownames(data_lowlevel_summer)){
    data_lowlevel_summer[level, i] = sum(data_kbt[data_kbt$name_waterlevel == level & data_kbt$YY == i & data_kbt$hydro_year == "summer",]$lowlevel)
  }
}

data_lowlevel_winter = as.data.frame(matrix(0, nrow = 98, ncol = 4))
colnames(data_lowlevel_winter) = seq(1990,2020,by=10)
rownames(data_lowlevel_winter) = levels(data_kbt$name_waterlevel)

for (i in colnames(data_lowlevel_winter)){
  for(level in rownames(data_lowlevel_winter)){
    data_lowlevel_winter[level, i] = sum(data_kbt[data_kbt$name_waterlevel == level & data_kbt$YY == i & data_kbt$hydro_year == "winter",]$lowlevel)
  }
}

data_lowlevel_summer = data_lowlevel_summer[order(match(rownames(data_lowlevel_summer), c(map$gridcode))),]
data_lowlevel_winter = data_lowlevel_winter[order(match(rownames(data_lowlevel_winter), c(map$gridcode))),]

map$lowlevel_1990 =  data_lowlevel_summer$'1990' + data_lowlevel_winter$'1990'
map$lowlevel_2000 =  data_lowlevel_summer$'2000' + data_lowlevel_winter$'2000'
map$lowlevel_2010 =  data_lowlevel_summer$'2010' + data_lowlevel_winter$'2010'
map$lowlevel_2020 =  data_lowlevel_summer$'2020' + data_lowlevel_winter$'2020'

map$lowlevel_2020_summer = data_lowlevel_summer$'2020' 
map$lowlevel_2020_winter = data_lowlevel_winter$'2020' 

## Visualization
pal_brown = colorRampPalette(c("#F2E7B4", "#bc8013"), interpolate = "linear")

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/lowlevel_1990.pdf" )
spplot(map, zcol = "lowlevel_1990", at = seq(from=1, to=305, by=5), col.regions = c(pal_brown(60)),
       ylab =list("days of low-flow - 1990", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/lowlevel_2000.pdf" )
spplot(map, zcol = "lowlevel_2000", at = seq(from=1, to=305, by=5), col.regions = c(pal_brown(60)),
       ylab =list("days of low-flow - 2000", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/lowlevel_2010.pdf" )
spplot(map, zcol = "lowlevel_2010", at = seq(from=1, to=305, by=5), col.regions = c(pal_brown(60)),
       ylab =list("days of low-flow - 2010", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/lowlevel_2020.pdf" )
spplot(map, zcol = "lowlevel_2020", at = seq(from=1, to=305, by=5), col.regions = c(pal_brown(60)),
       ylab =list("days of low-flow - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/lowlevel_2020_summer.pdf" )
spplot(map, zcol = "lowlevel_2020_summer", at = seq(from=1, to=170, by=5), col.regions = c(pal_brown(60)),
       ylab =list("days of low-flow - summer - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()
pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/lowlevel_2020_winter.pdf" )
spplot(map, zcol = "lowlevel_2020_winter", at = seq(from=1, to=170, by=5), col.regions = c(pal_brown(60)),
       ylab =list("days of low-flow - winter - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

############################# precipitation ####################################
map$mean_precip_2020 =  data_mean_YY[data_mean_YY$YY=="2020",]$precip

pal_blue = colorRampPalette(c("slateblue2", "blue3"), interpolate = "linear")
pal_blue_light = colorRampPalette(c("slategray1", "slateblue2"), interpolate = "linear")


pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/mean_precip_2020.pdf" )
spplot(map, zcol = "mean_precip_2020",at = seq(from=1.79, to=5.05, by=0.05), col.regions = c(pal_blue_light(35), pal_blue(31)[-1]),
       ylab =list("in mm - mean precipitation - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

# Plot drainage and precip in one plot for one year and one water level
cols = c("Drainage" = "black", "Precipitation" = "blue3")

ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], 
       aes(x = date)) +
  geom_line(aes(y = drainage, col = "Drainage")) + 
  geom_line(aes(y = 120 * precip, col = "Precipitation")) +
  scale_y_continuous(name = "Drainage in m³/s", sec.axis = sec_axis(~./120, name = "Precipitation in mm")) + 
  labs(color = "Legend") + scale_color_manual(values = cols) + xlab("Date") + theme_bw()
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/precip_vs_drainage.pdf" )

############################# air temperature ##################################
map$mean_airtmp_2020 =  data_mean_YY[data_mean_YY$YY=="2020",]$airtmp

pal_red = colorRampPalette(c("palegoldenrod", "red3"))

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/mean_airtmp_2020.pdf" )
spplot(map, zcol = "mean_airtmp_2020", at = seq(from=0.5, to=11, by=0.1), col.regions = c(pal_red(105)), 
       ylab = list("in °C - mean temperature - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

# Plot drainage and airtmp in one plot for one year and one water level
cols = c("Drainage" = "black", "Temperature" = "red3")

ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], 
       aes(x = date)) +
  geom_line(aes(y = drainage, col = "Drainage")) + 
  geom_line(aes(y = 120 * airtmp, col = "Temperature")) +
  scale_y_continuous(name = "Drainage in m³/s", sec.axis = sec_axis(~./120, name = "Temperature in °C")) + 
  labs(color = "Legend") + scale_color_manual(values = cols) + xlab("Date") + theme_bw()


ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/airtmp_vs_drainage.pdf" )

############################### snow storage  ##################################

map$mean_snowstorage_2020 =  data_mean_YY[data_mean_YY$YY=="2020",]$snowstorage

pal_purple = colorRampPalette(c("white", "purple3"))

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/mean_snowstorage_2020.pdf")
spplot(map, zcol = "mean_snowstorage_2020", at = seq(from=0, to=260, by=1),  col.regions = c(pal_purple(261)[-1]), 
       ylab = list("in mm - mean snow storage - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

# Plot drainage and snowstorage in one plot for one year and one water level
cols = c("Drainage" = "black", "Snow Storage" = "purple3")

ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], 
       aes(x = date)) +
  geom_line(aes(y = drainage, col = "Drainage")) + 
  geom_line(aes(y = 7*snowstorage, col = "Snow Storage")) +
  scale_y_continuous(name = "Drainage in m³/s", sec.axis = sec_axis(~./7, name = "Snow storage in mm")) + 
  labs(color = "Legend") + scale_color_manual(values = cols) + theme_bw()+ xlab("Date")

ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/snowstorage_vs_drainage.pdf" )

################################ soil water ####################################

map$mean_soilwater_2020 =  data_mean_YY[data_mean_YY$YY=="2020",]$soilwater

pal_green= colorRampPalette(c("white", "forestgreen"))

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/mean_soilwater_2020.pdf" )
spplot(map, zcol = "mean_soilwater_2020", at = seq(from=0.18, to=0.44, by=0.005),  col.regions = c(pal_green(70)),
       ylab = list("in % - mean soil water - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

# Plot drainage and soilwater in one plot for one year and one water level
cols = c("Drainage" = "black", "Soil water" = "forestgreen")

ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], 
       aes(x = date)) +
  geom_line(aes(y = drainage, col = "Drainage")) + 
  geom_line(aes(y = 10000*soilwater, col = "Soil water")) +
  scale_y_continuous(name = "Drainage in m³/s", sec.axis = sec_axis(~./10000, name = "Soil water in %")) + 
  labs(color = "Legend") + scale_color_manual(values = cols) + theme_bw() + xlab("Date")

ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/soilwater_vs_drainage.pdf" )

################################# Humidity #################################
map$relhum_2020 =  data_mean_YY[data_mean_YY$YY=="2020",]$relhum

pal_pink= colorRampPalette(c("white", "deeppink"))

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/mean_relhum_2020.pdf" )
spplot(map, zcol = "relhum_2020",at = seq(from=0.76, to=0.85, by=0.001), col.regions = c(pal_pink(90)),
       ylab = list("in % - mean humidity - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

# Plot drainage and relhum in one plot for one year and one water level
cols = c("Drainage" = "black", "Humidity" = "deeppink")

ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], 
       aes(x = date)) +
  geom_line(aes(y = drainage, col = "Drainage")) + 
  geom_line(aes(y = 10000*relhum, col = "Humidity")) +
  scale_y_continuous(name = "Drainage in m³/s", sec.axis = sec_axis(~./10000, name = "Humidity in %")) + 
  labs(color = "Legend") + scale_color_manual(values = cols) + theme_bw()+ xlab("Date")

ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/relhum_vs_drainage.pdf" )

################################# Radiation #################################
map$glorad_2020 =  data_mean_YY[data_mean_YY$YY=="2020",]$glorad

pal_gold= colorRampPalette(c("white", "gold2"), interpolate = "linear")

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/mean_glorad_2020.pdf" )
spplot(map, zcol = "glorad_2020", at = seq(from=326, to=478, by=1), col.regions = c(pal_gold(152)),
       ylab = list("in Wh/m² - mean radiation - 2020", cex=1.5), par.settings=list(axis.text=list(cex=1.5)))
dev.off()

# Plot drainage and glorad in one plot for one year and one water level
cols = c("Drainage" = "black", "Radiation" = "gold2")

ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], 
       aes(x = date)) +
  geom_line(aes(y = drainage, col = "Drainage")) + 
  geom_line(aes(y = 10*glorad, col = "Radiation")) +
  scale_y_continuous(name = "Drainage in m³/s", sec.axis = sec_axis(~./10, name = "Radiation in Wh/m²")) + 
  labs(color = "Legend") + scale_color_manual(values = cols) + theme_bw() + xlab("Date")

ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/glorad_vs_drainage.pdf" )

################################# Correlations #################################

colnames(data_kbt)[c(16,17,19:22,24)] = c("temperature", "radiation", "precipitation", "humidity", "snow storage", "soil water", "drainage")
ggcorrplot(cor(data_kbt[data_kbt$hydro_year == "summer",c(16,17,19:22,24)], use = "complete.obs"),
           type = "lower",
           lab = TRUE,
           legend.title = "Correlation")
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/cor_summer.pdf", bg = "white" )

ggcorrplot(cor(data_kbt[data_kbt$hydro_year == "winter",c(16,17,19:22,24)], use = "complete.obs"),
           type = "lower",
           lab = TRUE,
           legend.title = "Correlation")
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/cor_winter.pdf", bg = "white" )


############################# Amount low level events ##########################

hydro_lowlevel = hydro %>%
  group_by(member, name_waterlevel) %>%
  summarize(lowlevel = sum(lowlevel))

hydro_lowlevel_mean = hydro_lowlevel %>%
  group_by(name_waterlevel) %>%
  summarize(lowlevel = mean(lowlevel))

hydro_lowlevel_mean$ratio = hydro_lowlevel_mean$lowlevel/
  nrow(hydro[hydro$name_waterlevel == "X10001" & hydro$member == "kbe",])


hydro_lowlevel_mean = hydro_lowlevel_mean[order(match(gsub("X","", hydro_lowlevel_mean$name_waterlevel), c(map$gridcode))),]

map$lowlevel_ratio = hydro_lowlevel_mean$ratio

pal_orange= colorRampPalette(c("white", "orangered"))

pdf(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/distribution_lowlevel.pdf")
spplot(map, zcol = "lowlevel_ratio",  at = seq(from=0, to=0.25, by=0.01),col.regions = pal_orange(25), xlab = "Percentage of low flow days averaged over all members")
dev.off()

## Plot drainage and NM7Q in one plot for one year and one water level
ggplot(data = data_kbt[data_kbt$YY == 2020 & data_kbt$name_waterlevel == "X10001",], aes(x = date)) +
  geom_line(aes(y = drainage, color = "Drainage")) + 
  geom_segment(aes(x=as.Date("2020-04-01"),xend=as.Date("2020-10-31"),y=838,yend=838, color = "NM7Q summer")) +
  geom_segment(aes(x=as.Date("2020-01-01"),xend=as.Date("2020-03-31"),y=760,yend=760, color = "NM7Q winter")) +
  geom_segment(aes(x=as.Date("2020-11-01"),xend=as.Date("2020-12-31"),y=760,yend=760, color = "NM7Q winter")) +
  scale_color_manual(name = "Legend", 
                     values = c("Drainage" = "black", "NM7Q summer" = "orange", "NM7Q winter" = "dodgerblue4")) +
  labs(y = "Drainage in m³/s", x = "Date") +
  theme_bw()

ggsave(file = "E:/Consulting//Deskriptiv/Plots/nm7q_vs_drainage.pdf" )


########################## Time-constant variables #############################

properties = read.csv("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/information waterlevel/Eigenschaften_Pegel.csv", sep = ";", header = TRUE)
properties <- properties %>%
  mutate(river = as.factor(case_when(startsWith(ID, "1") ~ "Danube",
                           startsWith(ID, "2") ~ "Main",
                           startsWith(ID, "3") ~ "Inn",
                           startsWith(ID, "4") ~ "Elbe")),
         ID = as.factor(gsub(",0","", ID)),
         landuse = as.factor(gsub(",0","",landuse)),
         landuse = ifelse(landuse %in% c(1,2,3,4,5,6,7,8,9),"Artificial Surfaces",
                          ifelse(landuse %in% c(12,15,16,20,21,22,212,220,221,420,421),"Agricultural Areas",
                                 ifelse(landuse %in% c(23,24,25, 27,29,124,223,224,225,323,324,423,424,425),"Forest/Semi-Natural Areas",
                                        ifelse(landuse %in% c(35,36), "Wetlands",
                                               ifelse(landuse %in% c(40,41), "Water Bodies",
                                                      ifelse(landuse %in% c(10,11,18,26), "Grassland",
                                                             ifelse(landuse %in% c(30,31,32,24), "Little or no Vegetation", NA))))))),
         landuse = as.factor(landuse))

properties = properties[order(match(properties$ID, c(map$gridcode))),]

pal <- colorRampPalette(brewer.pal(6, "BrBG"))
pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/landuse.pdf")
spplot(map, zcol = "landuse",  
       col.regions=pal(6),
       ylab = list("Land use", cex=1), par.settings=list(axis.text=list(cex=1)))
dev.off()

map$dgm = as.numeric(gsub(",",".",properties$dgm))
pal_or = colorRampPalette(c("#fbe3ad", "#fbb515"))
pal_brown = colorRampPalette(c( "#fbb515", "#4f2a00"))

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/dgm.pdf")
spplot(map, zcol = "dgm", 
       at = seq(from=100, to=1600, by=10), 
       col.regions = c(pal_or(70), pal_brown(80)),
       ylab = list("Ground elevation (DGM)", cex=1), par.settings=list(axis.text=list(cex=1)))
dev.off()

map$river = properties$river
pal <- colorRampPalette(brewer.pal(4, "Set2"))

pdf(file="/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/river.pdf")
spplot(map, zcol = "river", col.regions = pal(4),
       ylab = list("River basins", cex=1), par.settings=list(axis.text=list(cex=1)))
dev.off()



