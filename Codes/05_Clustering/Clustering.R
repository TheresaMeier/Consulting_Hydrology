############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### K-means/Hierarchical Clustering of estimated member-averaged coefficients

## Load libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
library(caret)
library(car)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(vtable)
library(maptools)
library(factoextra)
library(cluster)
library(RColorBrewer)

## Load the data
coefs.summer.avg = fread("C:/Users/elham/Documents/R/Consulting_Hydrology-master/Consulting_Hydrology-master/Codes/03_Modelling/tables/coef/summer/coefs.summer.avg",
                         data.table = FALSE)[,-1]
coefs.summer.avg <- coefs.summer.avg %>%
  mutate(avg_snowstorage = if_else(is.na(avg_snowstorage), 0 , avg_snowstorage),
         `avg_airtmp:avg_snowstorage` = if_else(is.na(`avg_airtmp:avg_snowstorage`), 0 , `avg_airtmp:avg_snowstorage`))

coefs.winter.avg = fread("C:/Users/elham/Documents/R/Consulting_Hydrology-master/Consulting_Hydrology-master/Codes/03_Modelling/tables/coef/winter/coefs.winter.avg",
                         data.table = FALSE)[,-1]

# Visualization
map = readOGR(
  dsn = "E:/Consulting/shapefile/",
  layer = "EZG_OHNE_Reservoir_UTM32",
  verbose = FALSE)

# Set seed 
set.seed(123)

############################### Clustering summer ##############################

## Prepare coefficients for clustering
# Standardizing 
coefs.summer.avg.scale = coefs.summer.avg[,-c(13:22)]
coefs.summer.avg.scale [-1] = as.data.frame(apply(coefs.summer.avg[,-c(1,2,13:22)],2,FUN = scale))

coefs.summer.avg.scale <- coefs.summer.avg.scale %>% 
  mutate(name_waterlevel = as.character(name_waterlevel)) %>%
  mutate(river = ifelse(startsWith(name_waterlevel, "1"), "Donau",
                        ifelse(startsWith(name_waterlevel, "2"), "Main",
                               ifelse(startsWith(name_waterlevel, "3"), "Inn",
                                      ifelse(startsWith(name_waterlevel, "4"), "Elbe", NA))))) %>%
  mutate(river2 = ifelse(river=="Elbe", paste0("E",1:5),
                         ifelse(river =="Donau",paste0("D",1:44), 
                                ifelse(river=="Main", paste0("M",1:23), 
                                       ifelse(river=="Inn", paste0("I",1:26), 
                                              "NA")))))

coefs.summer.avg.scale[nrow(coefs.summer.avg.scale) + 1,] = 0
coefs.summer.avg.scale[nrow(coefs.summer.avg.scale),1] = "10702"
coefs.summer.avg.scale[nrow(coefs.summer.avg.scale),13] = "Donau"
coefs.summer.avg.scale[nrow(coefs.summer.avg.scale),14] = "D44"

rownames(coefs.summer.avg.scale) <- coefs.summer.avg.scale$river2


## Elbow plot with euclidean distance 
pdf(file="E:/Consulting/Clustering/ellbow_summer.pdf")
fviz_nbclust(coefs.summer.avg.scale[,-c(1,13,14)], kmeans, method = "wss", 
             k.max = 10) +
  geom_vline(xintercept = 4, linetype = 2) +
  ggtitle("Elbow plot summer")
dev.off()

## K-means clustering with euclidean distance
km_4_summer <- kmeans(coefs.summer.avg.scale[,-c(1,13,14)], centers = 4, nstart = 25)

pdf(file="E:/Consulting/Clustering/clusterplot_summer.pdf")
fviz_cluster(km_4_summer, data = coefs.summer.avg.scale[,-c(1,13,14)], labelsize = 7) 
dev.off()


## Get the cluster assignments for each observation
coefs.summer.avg.scale <- cbind(coefs.summer.avg.scale, km_4 = km_4_summer$cluster)

## Mapping K-means clustering 
coefs.summer.avg.scale = coefs.summer.avg.scale[order(match(coefs.summer.avg.scale$name_waterlevel, c(map$gridcode))),]
map$km4_s <- as.factor(coefs.summer.avg.scale$km_4)

my_pal_s <- colorRampPalette(c("#f1ad61", "#00c7ba","lightgreen", "#c8a3e7"))
pdf(file="E:/Consulting/Clustering/clustermap_summer.pdf")
spplot(map, zcol = "km4_s", col.regions=my_pal_s(4))
dev.off()

## Cluster mean summer 
coefs.summer.avg[nrow(coefs.summer.avg) + 1,] = NA
coefs.summer.avg[nrow(coefs.summer.avg),1] = "10702"
coefs.summer.avg <- cbind(coefs.summer.avg, km_4 = km_4_summer$cluster)
sumtable(coefs.summer.avg,
         vars=c('(Intercept)', 'avg_precip', 'avg_airtmp', 'avg_glorad',  
                'avg_relhum', 'avg_snowstorage', 'avg_soilwater',
                "avg_airtmp:avg_precip", "avg_airtmp:avg_snowstorage", 
                "avg_airtmp:avg_soilwater"),
         group="km_4",
         summ = c('notNA(x)', 'mean(x)'),
         group.long = F, 
         title = "Cluster mean summer")

############################## Clustering winter ###############################

## Prepare coefficients for clustering
# Standardizing 
coefs.winter.avg.scale = coefs.winter.avg[,-c(13:22)]
coefs.winter.avg.scale [-1] = as.data.frame(apply(coefs.winter.avg[,-c(1,2,13:22)],2,FUN = scale))

coefs.winter.avg.scale <- coefs.winter.avg.scale %>% 
  mutate(name_waterlevel = as.character(name_waterlevel)) %>%
  mutate(river = ifelse(startsWith(name_waterlevel, "1"), "Donau",
                                   ifelse(startsWith(name_waterlevel, "2"), "Main",
                                                     ifelse(startsWith(name_waterlevel, "3"), "Inn",
                                                                       ifelse(startsWith(name_waterlevel, "4"), "Elbe", NA))))) %>%
  mutate(river2 = ifelse(river=="Elbe", paste0("E",1:5),
                        ifelse(river =="Donau",paste0("D",1:44), 
                               ifelse(river=="Main", paste0("M",1:23), 
                                      ifelse(river=="Inn", paste0("I",1:26), 
                                             "NA")))))
rownames(coefs.winter.avg.scale) <- coefs.winter.avg.scale$river2

## Elbow plot with euclidean distance
pdf(file="E:/Consulting/Clustering/ellbow_winter.pdf")
fviz_nbclust(coefs.winter.avg.scale[,-c(1, 13, 14)], kmeans, method = "wss", 
             k.max = 10) + 
  geom_vline(xintercept = 4, linetype = 2) +
  ggtitle("Elbow plot winter")
dev.off()

km_4_winter <- kmeans(coefs.winter.avg.scale[,-c(1, 13, 14)], centers = 4, nstart = 25)

pdf(file="E:/Consulting/Clustering/clusterplot_winter.pdf")
 fviz_cluster(km_4_winter, data = coefs.winter.avg.scale[,-c(1, 13, 14)], labelsize = 7) +
  ggtitle("Cluster plot winter")
dev.off()

## Get the cluster assignments for each observation
coefs.winter.avg.scale <- cbind(coefs.winter.avg.scale, km_4 = km_4_winter$cluster)

## Mapping K-means clustering 
coefs.winter.avg.scale = coefs.winter.avg.scale[order(match(coefs.winter.avg.scale$name_waterlevel, c(map$gridcode))),]
map$km4_w = coefs.winter.avg.scale$km_4

map$km4_w <- as.factor(coefs.winter.avg.scale$km_4)
my_pal_w <- colorRampPalette(c("#00c7ba", "#c8a3e7", "#f1ad61","lightgreen" ))
pdf(file="E:/Consulting/Clustering/clustermap_winter.pdf")
spplot(map, zcol = "km4_w", col.regions=my_pal_w(4)
       #, xlab="K-means cluster winter"
       )
dev.off()

# Cluster mean winter
coefs.winter.avg <- cbind(coefs.winter.avg, km_4 = km_4_winter$cluster)
sumtable(coefs.winter.avg,
         vars=c('(Intercept)', 'avg_precip', 'avg_airtmp', 'avg_glorad',  'avg_relhum', 'avg_snowstorage', 'avg_soilwater',"avg_airtmp:avg_precip", "avg_airtmp:avg_snowstorage", "avg_airtmp:avg_soilwater"),
         group="km_4",
         summ = c('notNA(x)', 'mean(x)', 'SD(x)'),
         group.long = F, 
         title = "Cluster mean winter")


