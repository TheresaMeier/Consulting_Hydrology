############################ Consulting Hydrology ##############################
################## Theresa Meier, Nikita Paschan ###############################
################################################################################

### Functions for Data Pre-Processing

############# Data preparation and aggregation for each member #################

aggregate_data = function(member = "kbe", path){
  
  ## Load data
  # Tables for nm7q
  nm7q_raw_summer = read.delim(paste0(path, "/Zielvariablen/member_nm7q_s.txt"))
  nm7q_raw_winter = read.delim(paste0(path, "/Zielvariablen/member_nm7q_w.txt"))
  
  # Table for time-constant variables
  properties = read.csv(paste0(path, "/information waterlevel/Eigenschaften_Pegel.csv"), sep = ";", header = TRUE)
  
  # Drivers
  for (var in c("airtmp", "glorad", "groundwaterdepth", "precip", "relhum", "snowstorage", "soilwaterrootzone", "infiltration")){
    assign(paste0(var,"_long"), melt(data=fread(paste0(path,"/Treibervariablen/", var, "/", var, "_", member, "_1990-2010.txt"))
                                     , id.vars = c("YY", "MM", "DD","HH"), variable.name = "Pegel", value.name = var, na.rm = FALSE))
  }
  
  # Target
  abfluss_long = melt(data=fread(paste0(path,"/Zielvariablen/gerinneabfluss/gerinneabfluss_", member, "_1990_2020.txt"))
                     , id.vars = c("YY", "MM", "DD","HH"), variable.name = "Pegel", value.name = "abfluss", na.rm = FALSE)
  
  abfluss_long = abfluss_long %>%
    filter(Pegel %in% levels(airtmp_long$Pegel))
  
  # Create full data set
  data_long = airtmp_long %>%
    mutate(glorad = glorad_long$glorad,
           precip = precip_long$precip,
           groundwaterdepth = groundwaterdepth_long$groundwaterdepth,
           relhum = relhum_long$relhum,
           snowstorage = snowstorage_long$snowstorage,
           soilwater = soilwaterrootzone_long$soilwaterrootzone,
           infiltration = infiltration_long$infiltration,
           drainage = abfluss_long$abfluss, 
           date = as.Date(with(airtmp_long, paste(YY,MM,DD, sep = "-")), "%Y-%m-%d"),
           hydro_year = as.factor(ifelse(MM %in% c(5,6,7,8,9,10), "summer","winter"))) %>%
    rename(name_waterlevel = Pegel)
  
  ## Threshold for Classification of low-flow events
  # nm7q
  nm7q_summer = unlist(nm7q_raw_summer[nm7q_raw_summer$member == member,][-1])
  nm7q_winter = unlist(nm7q_raw_winter[nm7q_raw_winter$member == member,][-1])
  
  waterlevel = levels(data_long$name_waterlevel)
  data_long = add_column(.data = data_long,"nm7q" = rep(0,nrow(data_long)))
  
  for (wl in waterlevel){
    data_long[data_long$name_waterlevel == wl,]$nm7q = ifelse(data_long[data_long$name_waterlevel == wl,]$MM %in% c(5,6,7,8,9,10), nm7q_summer[paste0("X",wl)],
                                                            ifelse(data_long[data_long$name_waterlevel == wl,]$MM %in% c(1,2,3,4,11,12),nm7q_winter[paste0("X",wl)],NA))
  }
  
  data_long$lowlevel_3h = as.integer(data_long$drainage<data_long$nm7q)
  
  ## Aggregate 3 hourly data to daily data
  data_agg_1 = aggregate(list(data_long$airtmp, data_long$glorad, data_long$groundwaterdepth,data_long$relhum,
                             data_long$snowstorage, data_long$soilwater, data_long$infiltration, data_long$drainage, data_long$lowlevel_3h), 
                        by = list(data_long$date, data_long$YY, data_long$MM, data_long$DD, data_long$hydro_year, data_long$name_waterlevel, data_long$nm7q), mean)
  data_agg_1 = arrange(data_agg_1,Group.6, Group.2, Group.3)
  colnames(data_agg_1) = c("date", "YY", "MM", "DD", "hydro_year", "name_waterlevel", "nm7q", "airtmp", "glorad", "groundwaterdepth", "relhum",
                          "snowstorage", "soilwater", "infiltration", "drainage", "lowlevel_3h")
  data_agg_2 = aggregate(list(data_long$precip), by = list(data_long$date, data_long$YY, data_long$MM, data_long$DD, data_long$hydro_year, data_long$name_waterlevel, data_long$nm7q), sum)
  data_agg_2 = arrange(data_agg_2,Group.6, Group.2, Group.3)
  colnames(data_agg_2) = c("date", "YY", "MM", "DD", "hydro_year", "name_waterlevel", "nm7q", "precip")
  
  data_long = data_agg_1 %>%
    mutate(precip = data_agg_2$precip)
  data_agg_1 = NULL
  data_agg_2 = NULL
  
  ## Time-constant variables
  properties$ID = levels(data_long$name_waterlevel)
  data_long = add_column(.data = data_long,"latitude" = rep(0,nrow(data_long)))
  data_long = add_column(.data = data_long,"longitude" = rep(0,nrow(data_long)))
  data_long = add_column(.data = data_long,"dgm" = rep(0,nrow(data_long)))
  data_long = add_column(.data = data_long,"slope" = rep(0,nrow(data_long)))
  data_long = add_column(.data = data_long,"landuse" = rep(0,nrow(data_long)))
  data_long = add_column(.data = data_long,"exposition" = rep(0,nrow(data_long)))
  
  for (wl in waterlevel){
    data_long[data_long$name_waterlevel == wl,]$latitude = as.numeric(gsub(",",".", properties[properties$ID == wl,]$latitude))
    data_long[data_long$name_waterlevel == wl,]$longitude = as.numeric(gsub(",",".", properties[properties$ID == wl,]$longitude))
    data_long[data_long$name_waterlevel == wl,]$dgm = as.numeric(gsub(",",".", properties[properties$ID == wl,]$dgm))
    data_long[data_long$name_waterlevel == wl,]$slope = as.numeric(gsub(",",".", properties[properties$ID == wl,]$slope))
    data_long[data_long$name_waterlevel == wl,]$landuse = gsub(",",".", properties[properties$ID == wl,]$landuse)
    data_long[data_long$name_waterlevel == wl,]$exposition = as.numeric(gsub(",",".", properties[properties$ID == wl,]$exposition))
    # print (wl)
  }
  
  # Classification into rivers: Donau, Elbe, Main, Inn
  data_long$river = as.factor(ifelse(data_long$name_waterlevel %in% waterlevel[1:5], "Elbe", 
                                    ifelse(data_long$name_waterlevel %in% waterlevel[6:49], "Donau", 
                                           ifelse(data_long$name_waterlevel %in% waterlevel[50:72], "Main", 
                                                  ifelse(data_long$name_waterlevel %in% waterlevel[73:98], "Inn", 0)))))
  
  # Classifying landuse
  data_long$landuse = as.numeric(data_long$landuse)
  data_long$landuse = ifelse(data_long$landuse %in% c(1,2,3,4,5,6,7,8,9),"Artificial Surfaces",
                            ifelse(data_long$landuse %in% c(12,15,16,20,21,22,212,220,221,420,421),"Agricultural Areas",
                                   ifelse(data_long$landuse %in% c(23,24,25, 27,29,124,223,224,225,323,324,423,424,425),"Forest and semi-natural areas",
                                          ifelse(data_long$landuse %in% c(35,36), "Wetlands",
                                                 ifelse(data_long$landuse %in% c(40,41), "Water Bodies",
                                                        ifelse(data_long$landuse %in% c(10,11,18,26), "Grassland",
                                                               ifelse(data_long$landuse %in% c(30,31,32,24), "little or no vegetation", NA)))))))
  data_long$landuse = as.factor(data_long$landuse)
  
  # Modifying exposition
  data_long = data_long %>%
    mutate(exposition = if_else(exposition >= 0 & exposition < 45 | exposition >= 315 & exposition <=360, "North",
                              if_else(exposition >= 45 & exposition < 135, "East",
                                      if_else(exposition >= 135 & exposition < 225, "South",
                                              if_else(exposition >= 225 & exposition < 315, "West", "NA"))))) 
  ## Calculation lowlevel indicator
  # Classification low-flow event: drainage is at least 3 days in a row below nm7q
  data_long <- data_long %>%
    mutate(lowlevel_1d = ifelse(lowlevel_3h == 1,1,0),
           lowlevel = rep(0,nrow(data_long)),
           lowlevel_days = rep(0,nrow(data_long)),
           lowlevel_intensity = drainage - nm7q)
  count = 0
  j = 0
  k = 1
  nr_rows = as.numeric(count(data_long[data_long$name_waterlevel == "10001",]))
  
  for (wl in waterlevel){
    for (i in 1:nr_rows){
      if (data_long$lowlevel_1d[i+j] == 1){
        count = count + 1
      }
      
      else {count = 0}
      
      if (count == 3){
        data_long$lowlevel[i-2+j] = 1
        data_long$lowlevel[i-1+j] = 1
        data_long$lowlevel[i+j] = 1
        
        data_long$lowlevel_days[i-2+j] = 1
        data_long$lowlevel_days[i-1+j] = 2
        data_long$lowlevel_days[i+j] = 3
      } 
      else if (count > 3){
        data_long$lowlevel[i+j] = 1
        data_long$lowlevel_days[i+j] = count
      }
      
      else {
        data_long$lowlevel[i+j] = 0
        data_long$lowlevel_days[i+j] = 0
      }
      
      i = i+1 
    }
    j = j + nr_rows
    print(k)
    k = k + 1       # running index to monitor progress
  }
  
  ## Save data set
  write.table(data_long, file = paste0(member,".txt"), sep="\t", col.names = TRUE)
}

########################### Create full data set ###############################

full_data = function(path){
  member = c("kbe", "kbj", "kbo", "kbt", "kby", "kcd", "kci", "kcn", "kcs", "kcx")
  for (mb in member){
    assign(mb, fread(paste0("/Datensätze_aggregiert/", mb, ".txt"))[,-1])
  }
  
  kbe$member = rep("kbe", times = nrow(kbe))
  kbj$member = rep("kbj", times = nrow(kbj))
  kbo$member = rep("kbo", times = nrow(kbj))
  kbt$member = rep("kbt", times = nrow(kbt))
  kby$member = rep("kby", times = nrow(kby))
  kcd$member = rep("kcd", times = nrow(kcd))
  kci$member = rep("kci", times = nrow(kci))
  kcn$member = rep("kcn", times = nrow(kcn))
  kcs$member = rep("kcs", times = nrow(kcs))
  kcx$member = rep("kcx", times = nrow(kcx))
  
  hydro = rbind(kbe, kbj, kbo, kbt, kby, kcd, kci, kcn, kcs, kcx)
  write.table(hydro, file = "hydro.txt", sep="\t", col.names = TRUE)
}

############################ Data Summer #######################################

summer_data <- function(data_full){
  
  data_summer <- data_full %>%
    filter(MM %in% c(3,4,5,6,7,8,9,10)) %>%
    group_by(member, name_waterlevel) %>%
    mutate(avg_soilwater = rollmean(scale(100*soilwater, scale = FALSE), k = 60, fill = NA, align = "right"))    # 2 months
  
  data_summer <- data_summer %>%
    filter(MM %in% c(4,5,6,7,8,9,10)) %>%
    group_by(member, name_waterlevel) %>%
    mutate(avg_snowstorage = rollmean(scale(snowstorage, scale=FALSE), k = 30, fill = NA, align = "right")) %>%   # 1 month
    mutate(avg_snowstorage_unscaled = rollmean(snowstorage, k = 30, fill = NA, align = "right"))          # 1 month
  
  data_summer <- data_summer %>%
    filter(MM %in% c(4,5,6,7,8,9,10)) %>%
    filter(case_when(MM == 4 ~ DD > 23,
                     MM %in% c(5,6,7,8,9,10) ~ DD > 0)) %>%
    group_by(member, name_waterlevel) %>%
    mutate(avg_airtmp = rollmean(scale(airtmp, scale = FALSE), k = 7 , fill = NA, align = "right")) %>%
    mutate(avg_precip = rollmean(scale(precip, scale = FALSE), k = 7, fill = NA, align = "right")) %>%
    mutate(avg_precip05 = rollmean(0.5*precip, k = 7, fill = NA, align = "right") - mean(precip)) %>%
    mutate(avg_glorad = rollmean(scale(glorad, scale = FALSE), k = 7 ,fill = NA, align = "right")) %>%
    mutate(avg_relhum = rollmean(scale(100*relhum, scale = FALSE), k = 7, fill = NA, align = "right")) %>%
    mutate(avg_infiltration = rollmean(scale(infiltration, scale = FALSE), k = 7, fill = NA, align = "right"))
  
  
  data_summer <- data_summer %>%
    filter(hydro_year == "summer")
  
  write.table(data_summer, file = "hydro_summer.txt", sep="\t", col.names = TRUE)
}

############################## Data Winter #####################################

winter_data <- function(data_full){
  
  data_winter <- data_full %>%
    filter(MM %in% c(9,10,11,12,1,2,3,4)) %>%
    group_by(member, name_waterlevel) %>%
    mutate(avg_soilwater = rollmean(scale(100*soilwater, scale = FALSE), k = 60, fill = NA, align = "right"))        # 2 months
  
  data_winter <- data_winter %>%
    filter(MM %in% c(10,11,12,1,2,3,4)) %>%
    group_by(member, name_waterlevel) %>%
    mutate(avg_snowstorage = rollmean(scale(snowstorage, scale=FALSE), k = 30, fill = NA, align = "right"))  %>%         # 1 month
    mutate(avg_snowstorage_unscaled = rollmean(snowstorage, k = 30, fill = NA, align = "right")) 
  
  data_winter <- data_winter %>%
    filter(MM %in% c(10,11,12,1,2,3,4)) %>%
    filter(case_when(MM == 10 ~ DD > 23,
                     MM %in% c(11,12,1,2,3,4) ~ DD > 0)) %>%
    group_by(member, name_waterlevel) %>%
    mutate(avg_airtmp = rollmean(scale(airtmp, scale = FALSE), k = 7 , fill = NA, align = "right")) %>%
    mutate(avg_precip = rollmean(scale(precip, scale = FALSE), k = 7, fill = NA, align = "right")) %>%
    mutate(avg_precip15 = rollmean(1.5*precip, k = 7, fill = NA, align = "right") - mean(precip)) %>%
    mutate(avg_glorad = rollmean(scale(glorad, scale = FALSE), k = 7 ,fill = NA, align = "right")) %>%
    mutate(avg_relhum = rollmean(scale(100*relhum, scale = FALSE), k = 7, fill = NA, align = "right")) %>%
    mutate(avg_infiltration = rollmean(scale(infiltration, scale = FALSE), k = 7, fill = NA, align = "right"))
  
  data_winter <- data_winter %>%
    filter(hydro_year == "winter")
  
  
  write.table(data_winter, file = "hydro_winter.txt", sep="\t", col.names = TRUE)
}