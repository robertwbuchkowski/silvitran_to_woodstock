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

#Percentile to use to define mean top Height and mean DBH
Percentile <- 0.90

#Number of decimal points to use in yield tables
dec <- 3

Var.names <- read.csv("Variable_names.csv")
Var.names = dplyr::rename(Var.names, Names = ï..Names)

#opening product ratio csv
P_Ratios <- read.csv("Product_Ratio.csv")
P_Ratios = dplyr::rename(P_Ratios, Specie = ï..Specie)
P_Ratios$Specie <- as.factor(P_Ratios$Specie)
P_Ratios$Product <- as.factor(P_Ratios$Product)
P_Ratios = dplyr::rename(P_Ratios, DBH90_2 = Tree.diameter)
P_Ratios = dplyr::rename(P_Ratios, HGT90_2 = Tree_height)

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

#Rearrange the table
BFIR_rcp00 <- BFIR_rcp00[,-c(1, 2)]
BFIR_rcp00 <- BFIR_rcp00[order(BFIR_rcp00$Age),]
colnames(BFIR_rcp00)[colnames(BFIR_rcp00) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- "*Y NA BFIR ? ? ?"
tmp <- "BFIR/BFIR_Base.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp00, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")



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

#Rearrange the table
BFIR_rcp45 <- BFIR_rcp45[,-c(1, 2)]
BFIR_rcp45 <- BFIR_rcp45[order(BFIR_rcp45$Age),]
colnames(BFIR_rcp45)[colnames(BFIR_rcp45) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- "*Y NA BFIR ? ? ? ?"
tmp <- "BFIR/BFIR_RCP45.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp45, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "  ")



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

#Rearrange the table
BFIR_rcp85 <- BFIR_rcp85[,-c(1, 2)]
BFIR_rcp85 <- BFIR_rcp85[order(BFIR_rcp85$Age),]
colnames(BFIR_rcp85)[colnames(BFIR_rcp85) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- "*Y NA BFIR ? ? ? ?"
tmp <- "BFIR/BFIR_RCP85.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp85, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "  ")
