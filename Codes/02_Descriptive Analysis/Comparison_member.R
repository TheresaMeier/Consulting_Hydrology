############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Comparison of members

## Load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

## Load the data 

data_30y <- fread("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/Datensätze_aggregiert/hydro.txt", data.table = FALSE)[,-1]
data_30y <- data_30y %>%
  mutate(member = as.factor(member),
         name_waterlevel = as.factor(name_waterlevel),
         river = as.factor(river))

############################### temperature ####################################

data_member_airtmp = as.data.frame(matrix(data=0, nrow = 10, ncol = 31))
colnames(data_member_airtmp) = seq(1990,2020, by=1)
rownames(data_member_airtmp) = levels(data_30y$member)

for (mb in rownames(data_member_airtmp)){
  for (year in colnames(data_member_airtmp)){
    data_member_airtmp[mb, year] = mean(data_30y[data_30y$YY == year & data_30y$name_waterlevel == "X10001" & data_30y$member == mb,"airtmp"])
  }
}

data_member_airtmp["mean",] = colMeans(data_member_airtmp)
# boxplots with ggplot
data_member_airtmp_long <- gather(data_member_airtmp, year, average_airtmp, "1990":"2020")

ggplot(data_member_airtmp_long, aes(x=year, y=average_airtmp)) +
  geom_boxplot(fill = "gray") + labs(x="Year", y="Average temperature in °C") +
  theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/boxplot_airtmp.pdf", bg = "white")

########################### precipitation ######################################

data_member_precip = as.data.frame(matrix(data=0, nrow = 10, ncol = 31))
colnames(data_member_precip) = seq(1990,2020, by=1)
rownames(data_member_precip) = levels(data_30y$member)

for (mb in rownames(data_member_precip)){
  for (year in colnames(data_member_precip)){
    data_member_precip[mb, year] = mean(data_30y[data_30y$YY == year & data_30y$name_waterlevel == "X10001" & data_30y$member == mb,"precip"])
  }
}

data_member_precip["mean",] = colMeans(data_member_precip)

# boxplots with ggplot
data_member_precip_long <- gather(data_member_precip, year, average_precip, "1990":"2020")

ggplot(data_member_precip_long[data_member_precip_long$year %in% c(1990,2000,2010,2020),], aes(x=year, y=average_precip)) +
  geom_boxplot(fill = "gray") + labs(x="Year", y="Average precipitation in mm") +
  theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/boxplot_precip.pdf", bg = "white")

############################# low level ########################################

data_member_lowlevel = as.data.frame(matrix(data=0, nrow = 10, ncol = 31))
colnames(data_member_lowlevel) = seq(1990,2020, by=1)
rownames(data_member_lowlevel) = levels(data_30y$member)

for (mb in rownames(data_member_lowlevel)){
  for (year in colnames(data_member_lowlevel)){
    data_member_lowlevel[mb, year] = sum(data_30y[data_30y$YY == year & data_30y$name_waterlevel == "X10001" & data_30y$member == mb,"lowlevel"])
  }
}

data_member_lowlevel["mean",] = colMeans(data_member_lowlevel)

data_member_lowlevel_long <- gather(data_member_lowlevel, year, sum_lowlevel, "1990":"2020")

ggplot(data_member_lowlevel_long[data_member_lowlevel_long$year %in% c(1990,2000,2010,2020),], aes(x=year, y=sum_lowlevel)) +
  geom_boxplot(fill = "gray") + labs(x="Year", y="Days of low flow") +
  theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Deskriptiv/Plots/boxplot_lowlevel.pdf", bg = "white")
