# Consulting
# Coefficient distribution over all members

library(data.table)
library(tidyr)
library(ggplot2)

# Load coefficients of models summer 
member = c("avg", "kbe", "kbj", "kbo", "kbt", "kby", "kcd", "kci", "kcn", "kcs", "kcx")

for (mb in member){
  assign(paste0("coefs.summer.", mb), fread(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coef/summer/coefs.summer.", mb)))
}
coefs.summer.avg$V1 = NULL

colnames(coefs.summer.kbe)[1] = "name_waterlevel"
coefs.summer.kbe$member= rep("kbe",times=97)

colnames(coefs.summer.kbj)[1] = "name_waterlevel"
coefs.summer.kbj$member= rep("kbj",times=97)

colnames(coefs.summer.kbo)[1] = "name_waterlevel"
coefs.summer.kbo$member= rep("kbo",times=97)

colnames(coefs.summer.kbt)[1] = "name_waterlevel"
coefs.summer.kbt$member= rep("kbt",times=97)

colnames(coefs.summer.kby)[1] = "name_waterlevel"
coefs.summer.kby$member= rep("kby",times=97)

colnames(coefs.summer.kcd)[1] = "name_waterlevel"
coefs.summer.kcd$member= rep("kcd",times=97)

colnames(coefs.summer.kci)[1] = "name_waterlevel"
coefs.summer.kci[nrow(coefs.summer.kci) + 1,] = NA
coefs.summer.kci[nrow(coefs.summer.kci),1] = "X10702"
coefs.summer.kci$member= rep("kci",times=97)

colnames(coefs.summer.kcn)[1] = "name_waterlevel"
coefs.summer.kcn$member= rep("kcn",times=97)

colnames(coefs.summer.kcs)[1] = "name_waterlevel"
coefs.summer.kcs$member= rep("kcs",times=97)

colnames(coefs.summer.kcx)[1] = "name_waterlevel"
coefs.summer.kcx$member= rep("kcx",times=97)

coefs.summer.total <- rbind(coefs.summer.kbe, coefs.summer.kbj, coefs.summer.kbo, coefs.summer.kbt, coefs.summer.kby,  coefs.summer.kcd, coefs.summer.kci, coefs.summer.kcn,
                            coefs.summer.kcs, coefs.summer.kcx)


# Load coefficients of models winter

for (mb in member){
  assign(paste0("coefs.winter.", mb), fread(paste0("/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/tables/coef/winter/coefs.winter.", mb)))
}

coefs.winter.avg$V1 = NULL
colnames(coefs.winter.kbe)[1] = "name_waterlevel"
coefs.winter.kbe$member= rep("kbe",times=98)

colnames(coefs.winter.kbj)[1] = "name_waterlevel"
coefs.winter.kbj$member= rep("kbj",times=98)

colnames(coefs.winter.kbo)[1] = "name_waterlevel"
coefs.winter.kbo$member= rep("kbo",times=98)

colnames(coefs.winter.kbt)[1] = "name_waterlevel"
coefs.winter.kbt$member= rep("kbt",times=98)

colnames(coefs.winter.kby)[1] = "name_waterlevel"
coefs.winter.kby$member= rep("kby",times=98)

colnames(coefs.winter.kcd)[1] = "name_waterlevel"
coefs.winter.kcd$member= rep("kcd",times=98)

colnames(coefs.winter.kci)[1] = "name_waterlevel"
coefs.winter.kci$member= rep("kci",times=98)

colnames(coefs.winter.kcn)[1] = "name_waterlevel"
coefs.winter.kcn$member= rep("kcn",times=98)

colnames(coefs.winter.kcs)[1] = "name_waterlevel"
coefs.winter.kcs$member= rep("kcs",times=98)

colnames(coefs.winter.kcx)[1] = "name_waterlevel"
coefs.winter.kcx$member= rep("kcx",times=98)

coefs.winter.total <- rbind(coefs.winter.kbe, coefs.winter.kbj, coefs.winter.kbo, coefs.winter.kbt, coefs.winter.kby,  coefs.winter.kcd, coefs.winter.kci, coefs.winter.kcn,
                            coefs.winter.kcs, coefs.winter.kcx)

# Draw boxplot for X30801

coefs.summer.X30801 = coefs.summer.total[coefs.summer.total$name_waterlevel == "X30801",]
colnames(coefs.summer.X30801) = c("name_waterlevel", "Intercept", "temperature", "humidity", "radiation", "soil water", "snow storage", "precipitation", "year", "temperature:precipitation", "temperature:snow storage", "temperature:soil water", "member")
coefs.summer.X30801 = gather(coefs.summer.X30801, coef, value, "Intercept":"temperature:soil water")
coefs.winter.X30801 = coefs.winter.total[coefs.winter.total$name_waterlevel == "X30801",]
colnames(coefs.winter.X30801) = c("name_waterlevel", "Intercept", "temperature", "humidity", "radiation", "soil water", "snow storage", "precipitation", "year", "temperature:precipitation", "temperature:snow storage", "temperature:soil water", "member")
coefs.winter.X30801 = gather(coefs.winter.X30801, coef, value, "Intercept":"temperature:soil water")

ggplot(data = coefs.summer.X30801[coefs.summer.X30801$coef != "Intercept",], aes(x=coef, y=value)) + 
  geom_boxplot() + theme_bw() + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 14),
                                      axis.text.y = element_text(size = 14),
                                      axis.title.x = element_text(size = 14, face = "bold"),
                                      axis.title.y = element_text(size = 14, face = "bold")) + 
  xlab("Coefficients - summer") + ylab("Values")
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/boxplots/boxplot_X30801_summer.pdf", bg = "white" )

ggplot(data = coefs.winter.X30801[coefs.winter.X30801$coef != "Intercept",], aes(x=coef, y=value)) + 
  geom_boxplot() + theme_bw()  + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 14),
                                       axis.text.y = element_text(size = 14),
                                       axis.title.x = element_text(size = 14, face = "bold"),
                                       axis.title.y = element_text(size = 14, face = "bold")) + 
  xlab("Coefficients - winter") + ylab("Values")
ggsave(file = "/home/theresa/Schreibtisch/Theresa/STUDIUM/Master Statistics and Data Science/Consulting/Daten/R Scripts/Model/Models_waterlevel/Effectplots/boxplots/boxplot_X30801_winter.pdf", bg = "white" )
