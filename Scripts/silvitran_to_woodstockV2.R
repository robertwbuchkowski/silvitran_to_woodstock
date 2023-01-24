#set R packages
library(data.table)
library(spatialEco)
library(raster)
library(rasterize)
library(viridis)
library(dplyr)
library(tidyr)
require(ggplot2)
library(ggthemes)
library(rgdal)
library(sf)#Load shapefiles faster than "rgdal"
library(sp)
library(broom)
library(rgeos)
library(stars)
library(nngeo)
library(EnvStats)
library(gstat)
library(paletteer)
library(ggpubr)
library(cowplot)
library(exactextractr)
library(matrixStats)
library(gdata)
library(reshape2)
library(DescTools)
library(purrr)
<<<<<<< HEAD

#Forcing R not to use scientific notation
options(scipen = 999)    
=======
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af

#Percentile to use to define mean top Height and mean DBH
Percentile <- 0.90

#Number of decimal points to use in yield tables
<<<<<<< HEAD
dec <- 4
=======
dec <- 3
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af

Var.names <- read.csv("Variable_names.csv")
Var.names = dplyr::rename(Var.names, Names = ï..Names)

#opening product ratio csv
<<<<<<< HEAD
P_Ratios <- read.csv("Product_Ratio_V2.csv")
=======
P_Ratios <- read.csv("Product_Ratio.csv")
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af
P_Ratios = dplyr::rename(P_Ratios, Specie = ï..Specie)
P_Ratios$Specie <- as.factor(P_Ratios$Specie)
P_Ratios$Product <- as.factor(P_Ratios$Product)
P_Ratios = dplyr::rename(P_Ratios, DBH90_2 = Tree.diameter)
P_Ratios = dplyr::rename(P_Ratios, HGT90_2 = Tree_height)

<<<<<<< HEAD
#Subset to keep general cedar ratios
EC_Ratio <- subset(P_Ratios, Specie =="OC")
EC_Ratio <- EC_Ratio[,-c(1, 6)]
EC_log <- subset(EC_Ratio, Product =="Logs")
EC_stud <- subset(EC_Ratio, Product =="Stud")
EC_pulp <- subset(EC_Ratio, Product =="Pulp")
EC_biomass <- subset(EC_Ratio, Product =="Biom")
EC_Ratio <- as.data.table(EC_Ratio)
EC_log <- as.data.table(EC_log)
EC_stud <- as.data.table(EC_stud)
EC_pulp <- as.data.table(EC_pulp)
EC_biomass <- as.data.table(EC_biomass)

#subset to keep general spruce, fir ratios
SF_Ratio <- subset(P_Ratios, Specie =="WS")
SF_Ratio <- SF_Ratio[,-c(1, 6)]
SF_log <- subset(SF_Ratio, Product =="Logs")
SF_stud <- subset(SF_Ratio, Product =="Stud")
SF_pulp <- subset(SF_Ratio, Product =="Pulp")
SF_biomass <- subset(SF_Ratio, Product =="Biom")
SF_Ratio <- as.data.table(SF_Ratio)
SF_log <- as.data.table(SF_log)
SF_stud <- as.data.table(SF_stud)
SF_pulp <- as.data.table(SF_pulp)
SF_biomass <- as.data.table(SF_biomass)

#subset to keep general white pine ratios
WP_Ratio <- subset(P_Ratios, Specie =="WP")
WP_Ratio <- WP_Ratio[,-c(1, 6)]
WP_log <- subset(WP_Ratio, Product =="Logs")
WP_stud <- subset(WP_Ratio, Product =="Stud")
WP_pulp <- subset(WP_Ratio, Product =="Pulp")
WP_biomass <- subset(WP_Ratio, Product =="Biom")
WP_Ratio <- as.data.table(WP_Ratio)
WP_log <- as.data.table(WP_log)
WP_stud <- as.data.table(WP_stud)
WP_pulp <- as.data.table(WP_pulp)
WP_biomass <- as.data.table(WP_biomass)

#subset to keep general Intolerant Hardwood ratios
IH_Ratio <- subset(P_Ratios, Specie =="PO")
IH_Ratio <- IH_Ratio[,-c(1, 6)]
IH_log <- subset(IH_Ratio, Product =="Logs")
IH_stud <- subset(IH_Ratio, Product =="Stud")
IH_pulp <- subset(IH_Ratio, Product =="Pulp")
IH_biomass <- subset(IH_Ratio, Product =="Biom")
IH_Ratio <- as.data.table(IH_Ratio)
IH_log <- as.data.table(IH_log)
IH_stud <- as.data.table(IH_stud)
IH_pulp <- as.data.table(IH_pulp)
IH_biomass <- as.data.table(IH_biomass)

#subset to keep general Intolerant Hardwood ratios
TH_Ratio <- subset(P_Ratios, Specie =="AS")
TH_Ratio <- TH_Ratio[,-c(1, 6)]
TH_log <- subset(TH_Ratio, Product =="Logs")
TH_stud <- subset(TH_Ratio, Product =="Stud")
TH_pulp <- subset(TH_Ratio, Product =="Pulp")
TH_biomass <- subset(TH_Ratio, Product =="Biom")
TH_Ratio <- as.data.table(TH_Ratio)
TH_log <- as.data.table(TH_log)
TH_stud <- as.data.table(TH_stud)
TH_pulp <- as.data.table(TH_pulp)
TH_biomass <- as.data.table(TH_biomass)
=======
#Subset to keep general conifer ratios
OC_Ratio <- subset(P_Ratios, Specie =="OC")
OC_Ratio <- OC_Ratio[,-c(1, 6)]
OC_log <- subset(OC_Ratio, Product =="log")
OC_pulp <- subset(OC_Ratio, Product =="pulp")
OC_biomass <- subset(OC_Ratio, Product =="biomass")
OC_Ratio <- as.data.table(OC_Ratio)
OC_log <- as.data.table(OC_log)
OC_pulp <- as.data.table(OC_pulp)
OC_biomass <- as.data.table(OC_biomass)

#subset to keep general hardwood ratios
OH_Ratio <- subset(P_Ratios, Specie =="AS")
OH_Ratio <- OH_Ratio[,-c(1, 6)]
OH_log <- subset(OH_Ratio, Product =="log")
OH_pulp <- subset(OH_Ratio, Product =="pulp")
OH_biomass <- subset(OH_Ratio, Product =="biomass")
OH_Ratio <- as.data.table(OH_Ratio)
OH_log <- as.data.table(OH_log)
OH_pulp <- as.data.table(OH_pulp)
OH_biomass <- as.data.table(OH_biomass)
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af



#Baseline
XXXX_vol00 <- read.csv("BFIR/BFIR_Baseline_volume.csv")
XXXX_dbh00 <- read.csv("BFIR/BFIR_Baseline_DBH.csv")
XXXX_hig00 <- read.csv("BFIR/BFIR_Baseline_height.csv")
#RCP4.5
XXXX_vol45 <- read.csv("BFIR/BFIR_RCP45_volume.csv")
XXXX_dbh45 <- read.csv("BFIR/BFIR_RCP45_DBH.csv")
XXXX_hig45 <- read.csv("BFIR/BFIR_RCP45_height.csv")
#RCP8.5
XXXX_vol85 <- read.csv("BFIR/BFIR_RCP85_volume.csv")
XXXX_dbh85 <- read.csv("BFIR/BFIR_RCP85_DBH.csv")
XXXX_hig85 <- read.csv("BFIR/BFIR_RCP85_height.csv")

#MEAN_HEIGHT
#height integration to baseline yields
XXXX_hig00 <- as.data.table(XXXX_hig00)
XXXX_hig00 <- XXXX_hig00[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig00 <- aggregate(XXXX_hig00, by=list(Age=XXXX_hig00$Year), FUN=sum)
XXXX_hig00 <- as.data.table(XXXX_hig00)
XXXX_hig00 <- XXXX_hig00[,-c("Year")]
require(reshape2)
XXXX_hig00 <- setDT(XXXX_hig00)
XXXX_hig00 <- melt(XXXX_hig00, id.vars =  c("Age"))
XXXX_hig00$Age <- as.factor(XXXX_hig00$Age)
XXXX_hig00$value <- as.integer(XXXX_hig00$value)
XXXX_hig00$variable <- paste(XXXX_hig00$Age,"_", XXXX_hig00$variable)
XXXX_hig00 <- XXXX_hig00[,-c(1)]
XXXX_hig00 <- uncount(XXXX_hig00, value, .id = "Tree")
XXXX_hig00 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig00$variable), "_", fixed = TRUE)))
XXXX_hig00 = dplyr::rename(XXXX_hig00, Age = X1)
XXXX_hig00 = dplyr::rename(XXXX_hig00, Height = X3)
XXXX_hig00 <- XXXX_hig00[,-c(2)]
XXXX_hig00 <- as.data.table(XXXX_hig00)
XXXX_hig00$Height <- as.integer(XXXX_hig00$Height)
XXXX_hig00$Age <- as.integer(XXXX_hig00$Age)
#To get middle of height class
XXXX_hig00[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig00 <- XXXX_hig00 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))


#height integration to RCP45 yields
XXXX_hig45 <- as.data.table(XXXX_hig45)
XXXX_hig45 <- XXXX_hig45[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig45 <- aggregate(XXXX_hig45, by=list(Age=XXXX_hig45$Year), FUN=sum)
XXXX_hig45 <- as.data.table(XXXX_hig45)
XXXX_hig45 <- XXXX_hig45[,-c("Year")]
require(reshape2)
XXXX_hig45 <- setDT(XXXX_hig45)
XXXX_hig45 <- melt(XXXX_hig45, id.vars =  c("Age"))
XXXX_hig45$Age <- as.factor(XXXX_hig45$Age)
XXXX_hig45$value <- as.integer(XXXX_hig45$value)
XXXX_hig45$variable <- paste(XXXX_hig45$Age,"_", XXXX_hig45$variable)
XXXX_hig45 <- XXXX_hig45[,-c(1)]
XXXX_hig45 <- uncount(XXXX_hig45, value, .id = "Tree")
XXXX_hig45 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig45$variable), "_", fixed = TRUE)))
XXXX_hig45 = dplyr::rename(XXXX_hig45, Age = X1)
XXXX_hig45 = dplyr::rename(XXXX_hig45, Height = X3)
XXXX_hig45 <- XXXX_hig45[,-c(2)]
XXXX_hig45 <- as.data.table(XXXX_hig45)
XXXX_hig45$Height <- as.integer(XXXX_hig45$Height)
XXXX_hig45$Age <- as.integer(XXXX_hig45$Age)
#To get middle of height class
XXXX_hig45[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig45 <- XXXX_hig45 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))


#height integration to RCP85 yields
XXXX_hig85 <- as.data.table(XXXX_hig85)
XXXX_hig85 <- XXXX_hig85[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig85 <- aggregate(XXXX_hig85, by=list(Age=XXXX_hig85$Year), FUN=sum)
XXXX_hig85 <- as.data.table(XXXX_hig85)
XXXX_hig85 <- XXXX_hig85[,-c("Year")]
require(reshape2)
XXXX_hig85 <- setDT(XXXX_hig85)
XXXX_hig85 <- melt(XXXX_hig85, id.vars =  c("Age"))
XXXX_hig85$Age <- as.factor(XXXX_hig85$Age)
XXXX_hig85$value <- as.integer(XXXX_hig85$value)
XXXX_hig85$variable <- paste(XXXX_hig85$Age,"_", XXXX_hig85$variable)
XXXX_hig85 <- XXXX_hig85[,-c(1)]
XXXX_hig85 <- uncount(XXXX_hig85, value, .id = "Tree")
XXXX_hig85 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig85$variable), "_", fixed = TRUE)))
XXXX_hig85 = dplyr::rename(XXXX_hig85, Age = X1)
XXXX_hig85 = dplyr::rename(XXXX_hig85, Height = X3)
XXXX_hig85 <- XXXX_hig85[,-c(2)]
XXXX_hig85 <- as.data.table(XXXX_hig85)
XXXX_hig85$Height <- as.integer(XXXX_hig85$Height)
XXXX_hig85$Age <- as.integer(XXXX_hig85$Age)
#To get middle of height class
XXXX_hig85[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig85 <- XXXX_hig85 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))


#MEAN_DIAMETER and DENSITY
#dbh integration to baseline yields
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00 <- XXXX_dbh00[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh00 <- aggregate(XXXX_dbh00, by=list(Age=XXXX_dbh00$Year), FUN=sum)
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00 <- XXXX_dbh00[,-c("Year")]
#adding density
XXXX_dty00 <- XXXX_dbh00[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty00 <- XXXX_dbh00[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh00 <- XXXX_dbh00[,-c("DTYtot", "DTY9")]
XXXX_dty00 <- XXXX_dty00[,c("Age", "DTYtot", "DTY9")]
XXXX_dty00[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]

require(reshape2)
XXXX_dbh00 <- setDT(XXXX_dbh00)
XXXX_dbh00 <- melt(XXXX_dbh00, id.vars =  c("Age"))
XXXX_dbh00$Age <- as.factor(XXXX_dbh00$Age)
XXXX_dbh00$value <- as.integer(XXXX_dbh00$value)
XXXX_dbh00$variable <- paste(XXXX_dbh00$Age,"_", XXXX_dbh00$variable)
XXXX_dbh00 <- XXXX_dbh00[,-c(1)]
XXXX_dbh00 <- uncount(XXXX_dbh00, value, .id = "Tree")
XXXX_dbh00 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh00$variable), "_", fixed = TRUE)))
XXXX_dbh00 = dplyr::rename(XXXX_dbh00, Age = X1)
XXXX_dbh00 = dplyr::rename(XXXX_dbh00, DBH = X3)
XXXX_dbh00 <- XXXX_dbh00[,-c(2)]
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00$DBH <- as.integer(XXXX_dbh00$DBH)
XXXX_dbh00$Age <- as.integer(XXXX_dbh00$Age)
#To get middle of DBH class
XXXX_dbh00[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh00 <- XXXX_dbh00 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))


#dbh integration to baseline yields
XXXX_dbh45 <- as.data.table(XXXX_dbh45)
XXXX_dbh45 <- XXXX_dbh45[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh45 <- aggregate(XXXX_dbh45, by=list(Age=XXXX_dbh45$Year), FUN=sum)
XXXX_dbh45 <- as.data.table(XXXX_dbh45)
XXXX_dbh45 <- XXXX_dbh45[,-c("Year")]
XXXX_dty45 <- XXXX_dbh45[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty45 <- XXXX_dbh45[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh45 <- XXXX_dbh45[,-c("DTYtot", "DTY9")]
XXXX_dty45 <- XXXX_dty45[,c("Age", "DTYtot", "DTY9")]
XXXX_dty45[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh45 <- setDT(XXXX_dbh45)
XXXX_dbh45 <- melt(XXXX_dbh45, id.vars =  c("Age"))
XXXX_dbh45$Age <- as.factor(XXXX_dbh45$Age)
XXXX_dbh45$value <- as.integer(XXXX_dbh45$value)
XXXX_dbh45$variable <- paste(XXXX_dbh45$Age,"_", XXXX_dbh45$variable)
XXXX_dbh45 <- XXXX_dbh45[,-c(1)]
XXXX_dbh45 <- uncount(XXXX_dbh45, value, .id = "Tree")
XXXX_dbh45 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh45$variable), "_", fixed = TRUE)))
XXXX_dbh45 = dplyr::rename(XXXX_dbh45, Age = X1)
XXXX_dbh45 = dplyr::rename(XXXX_dbh45, DBH = X3)
XXXX_dbh45 <- XXXX_dbh45[,-c(2)]
XXXX_dbh45 <- as.data.table(XXXX_dbh45)
XXXX_dbh45$DBH <- as.integer(XXXX_dbh45$DBH)
XXXX_dbh45$Age <- as.integer(XXXX_dbh45$Age)
#To get middle of DBH class
XXXX_dbh45[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh45 <- XXXX_dbh45 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))


#dbh integration to baseline yields
XXXX_dbh85 <- as.data.table(XXXX_dbh85)
XXXX_dbh85 <- XXXX_dbh85[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh85 <- aggregate(XXXX_dbh85, by=list(Age=XXXX_dbh85$Year), FUN=sum)
XXXX_dbh85 <- as.data.table(XXXX_dbh85)
XXXX_dbh85 <- XXXX_dbh85[,-c("Year")]
XXXX_dty85 <- XXXX_dbh85[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty85 <- XXXX_dbh85[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh85 <- XXXX_dbh85[,-c("DTYtot", "DTY9")]
XXXX_dty85 <- XXXX_dty85[,c("Age", "DTYtot", "DTY9")]
XXXX_dty85[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh85 <- setDT(XXXX_dbh85)
XXXX_dbh85 <- melt(XXXX_dbh85, id.vars =  c("Age"))
XXXX_dbh85$Age <- as.factor(XXXX_dbh85$Age)
XXXX_dbh85$value <- as.integer(XXXX_dbh85$value)
XXXX_dbh85$variable <- paste(XXXX_dbh85$Age,"_", XXXX_dbh85$variable)
XXXX_dbh85 <- XXXX_dbh85[,-c(1)]
XXXX_dbh85 <- uncount(XXXX_dbh85, value, .id = "Tree")
XXXX_dbh85 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh85$variable), "_", fixed = TRUE)))
XXXX_dbh85 = dplyr::rename(XXXX_dbh85, Age = X1)
XXXX_dbh85 = dplyr::rename(XXXX_dbh85, DBH = X3)
XXXX_dbh85 <- XXXX_dbh85[,-c(2)]
XXXX_dbh85 <- as.data.table(XXXX_dbh85)
XXXX_dbh85$DBH <- as.integer(XXXX_dbh85$DBH)
XXXX_dbh85$Age <- as.integer(XXXX_dbh85$Age)
#To get middle of DBH class
XXXX_dbh85[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh85 <- XXXX_dbh85 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))


#VOLUME
#Calculating new variable for Baseline total volume
XXXX_vol00$VOLtot <- rowSums(XXXX_vol00[,2:ncol(XXXX_vol00)])
XXXX_vol00 = dplyr::rename(XXXX_vol00, Age = Year)
BFIR_rcp00 <- merge (XXXX_vol00, XXXX_hig00, by = "Age")
BFIR_rcp00 <- merge (BFIR_rcp00, XXXX_dbh00, by = "Age")
is.num <- sapply(BFIR_rcp00, is.numeric)
BFIR_rcp00[is.num] <- lapply(BFIR_rcp00[is.num], round, dec)
BFIR_rcp00 <- merge (BFIR_rcp00, XXXX_dty00, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp00))
names(BFIR_rcp00)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]


#Adding column to help with the merge of wood product ratios (Height are rounded to the closest multiple of 2)
BFIR_rcp00$HGT90_2 <- RoundTo(BFIR_rcp00$HGT90, multiple = 2, FUN = round)
BFIR_rcp00$DBH90_2 <- RoundTo(BFIR_rcp00$DBH90, multiple = 2, FUN = round)
<<<<<<< HEAD
#merging ratios for EASTERN CEDAR
BFIR_rcp00 <- merge(BFIR_rcp00, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, EClogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, ECstdR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, ECplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp00 <- merge(BFIR_rcp00, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, SFlogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, SFstdR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, SFplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp00 <- merge(BFIR_rcp00, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, WPlogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, WPstdR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, WPplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp00 <- merge(BFIR_rcp00, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, IHlogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, IHstdR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, IHplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp00 <- merge(BFIR_rcp00, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, THlogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, THstdR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, THplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, THbioR = Ratio)
=======
#merging ratios for conifers
BFIR_rcp00 <- merge(BFIR_rcp00, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, ClogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, CplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, CbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp00 <- merge(BFIR_rcp00, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, HlogR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, HplpR = Ratio)
BFIR_rcp00 <- merge(BFIR_rcp00, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp00 = dplyr::rename(BFIR_rcp00, HbioR = Ratio)
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af

#Rearrange the table
BFIR_rcp00 <- BFIR_rcp00[,-c(1, 2)]
BFIR_rcp00 <- BFIR_rcp00[order(BFIR_rcp00$Age),]
colnames(BFIR_rcp00)[colnames(BFIR_rcp00) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- "*Y NA BFIR ? ? ?"
tmp <- "BFIR/BFIR_Base.txt"
cat(txt, "\n", file = tmp)
<<<<<<< HEAD
write.fwf(BFIR_rcp00, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "   ")
=======
write.fwf(BFIR_rcp00, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af



#Calculating new variable for RCP45 total volume
XXXX_vol45$VOLtot <- rowSums(XXXX_vol45[,2:ncol(XXXX_vol45)])
XXXX_vol45 = dplyr::rename(XXXX_vol45, Age = Year)
BFIR_rcp45 <- merge (XXXX_vol45, XXXX_hig45, by = "Age")
BFIR_rcp45 <- merge (BFIR_rcp45, XXXX_dbh45, by = "Age")
BFIR_rcp45 <- merge (BFIR_rcp45, XXXX_dty45, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp45))
names(BFIR_rcp45)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
is.num <- sapply(BFIR_rcp45, is.numeric)
BFIR_rcp45[is.num] <- lapply(BFIR_rcp45[is.num], round, dec)

#Adding column to help with the merge of wood product ratios (Height are rounded to the closest multiple of 2)
BFIR_rcp45$HGT90_2 <- RoundTo(BFIR_rcp45$HGT90, multiple = 2, FUN = round)
BFIR_rcp45$DBH90_2 <- RoundTo(BFIR_rcp45$DBH90, multiple = 2, FUN = round)
<<<<<<< HEAD
#merging ratios for EASTERN CEDAR
BFIR_rcp45 <- merge(BFIR_rcp45, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, EClogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, ECstdR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, ECplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp45 <- merge(BFIR_rcp45, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, SFlogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, SFstdR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, SFplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp45 <- merge(BFIR_rcp45, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, WPlogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, WPstdR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, WPplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp45 <- merge(BFIR_rcp45, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, IHlogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, IHstdR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, IHplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp45 <- merge(BFIR_rcp45, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, THlogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, THstdR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, THplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, THbioR = Ratio)
=======
#merging ratios for conifers
BFIR_rcp45 <- merge(BFIR_rcp45, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, ClogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, CplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, CbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp45 <- merge(BFIR_rcp45, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, HlogR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, HplpR = Ratio)
BFIR_rcp45 <- merge(BFIR_rcp45, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45 = dplyr::rename(BFIR_rcp45, HbioR = Ratio)

>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af
#Rearrange the table
BFIR_rcp45 <- BFIR_rcp45[,-c(1, 2)]
BFIR_rcp45 <- BFIR_rcp45[order(BFIR_rcp45$Age),]
colnames(BFIR_rcp45)[colnames(BFIR_rcp45) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- "*Y NA BFIR ? ? ? ?"
tmp <- "BFIR/BFIR_RCP45.txt"
cat(txt, "\n", file = tmp)
<<<<<<< HEAD
write.fwf(BFIR_rcp45, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "   ")
=======
write.fwf(BFIR_rcp45, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "  ")
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af



#Calculating new variable for RCP85 total volume
XXXX_vol85$VOLtot <- rowSums(XXXX_vol85[,2:ncol(XXXX_vol85)])
XXXX_vol85 = dplyr::rename(XXXX_vol85, Age = Year)
BFIR_rcp85 <- merge (XXXX_vol85, XXXX_hig85, by = "Age")
BFIR_rcp85 <- merge (BFIR_rcp85, XXXX_dbh85, by = "Age")
BFIR_rcp85 <- merge (BFIR_rcp85, XXXX_dty85, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp85))
names(BFIR_rcp85)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
is.num <- sapply(BFIR_rcp85, is.numeric)
BFIR_rcp85[is.num] <- lapply(BFIR_rcp85[is.num], round, dec)

#Adding column to help with the merge of wood product ratios (Height are rounded to the closest multiple of 2)
BFIR_rcp85$HGT90_2 <- RoundTo(BFIR_rcp85$HGT90, multiple = 2, FUN = round)
BFIR_rcp85$DBH90_2 <- RoundTo(BFIR_rcp85$DBH90, multiple = 2, FUN = round)
<<<<<<< HEAD
#merging ratios for EASTERN CEDAR
BFIR_rcp85 <- merge(BFIR_rcp85, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, EClogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, ECstdR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, ECplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp85 <- merge(BFIR_rcp85, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, SFlogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, SFstdR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, SFplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp85 <- merge(BFIR_rcp85, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, WPlogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, WPstdR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, WPplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp85 <- merge(BFIR_rcp85, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, IHlogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, IHstdR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, IHplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp85 <- merge(BFIR_rcp85, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, THlogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, THstdR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, THplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, THbioR = Ratio)
=======
#merging ratios for conifers
BFIR_rcp85 <- merge(BFIR_rcp85, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, ClogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, CplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, CbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp85 <- merge(BFIR_rcp85, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, HlogR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, HplpR = Ratio)
BFIR_rcp85 <- merge(BFIR_rcp85, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85 = dplyr::rename(BFIR_rcp85, HbioR = Ratio)
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af

#Rearrange the table
BFIR_rcp85 <- BFIR_rcp85[,-c(1, 2)]
BFIR_rcp85 <- BFIR_rcp85[order(BFIR_rcp85$Age),]
colnames(BFIR_rcp85)[colnames(BFIR_rcp85) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- "*Y NA BFIR ? ? ? ?"
tmp <- "BFIR/BFIR_RCP85.txt"
cat(txt, "\n", file = tmp)
<<<<<<< HEAD
write.fwf(BFIR_rcp85, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "   ")

#saving as csv
write.csv(BFIR_rcp00,"BFIR/BFIR_Base.csv", row.names = FALSE)
write.csv(BFIR_rcp00,"BFIR/BFIR_RCP45.csv", row.names = FALSE)
write.csv(BFIR_rcp00,"BFIR/BFIR_RCP85.csv", row.names = FALSE)
=======
write.fwf(BFIR_rcp85, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "  ")
>>>>>>> c4b99fdeb84de56969a57f89db466d38a69a19af
