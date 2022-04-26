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

#Percentile to use to define mean top Height and mean DBH
Percentile <- 0.90

Var.names <- read.csv("Variable_names.csv")
Var.names = dplyr::rename(Var.names, Names = ï..Names)
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


#MEAN_DIAMETER
#dbh integration to baseline yields
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00 <- XXXX_dbh00[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh00 <- aggregate(XXXX_dbh00, by=list(Age=XXXX_dbh00$Year), FUN=sum)
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00 <- XXXX_dbh00[,-c("Year")]
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
existing <- match(Var.names$Names,names(BFIR_rcp00))
names(BFIR_rcp00)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
colnames(BFIR_rcp00)[colnames(BFIR_rcp00) == 'Age'] <- '_Age'
is.num <- sapply(BFIR_rcp00, is.numeric)
BFIR_rcp00[is.num] <- lapply(BFIR_rcp00[is.num], round, 3)
#Exporting yield curve
txt <- "*Y NA BFIR ? ? Base"
tmp <- "BFIR/BFIR_Base.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp00, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "    ")

#Calculating new variable for RCP45 total volume
XXXX_vol45$VOLtot <- rowSums(XXXX_vol45[,2:ncol(XXXX_vol45)])
XXXX_vol45 = dplyr::rename(XXXX_vol45, Age = Year)
BFIR_rcp45 <- merge (XXXX_vol45, XXXX_hig45, by = "Age")
BFIR_rcp45 <- merge (BFIR_rcp45, XXXX_dbh45, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp45))
names(BFIR_rcp45)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
colnames(BFIR_rcp45)[colnames(BFIR_rcp45) == 'Age'] <- '_Age'
is.num <- sapply(BFIR_rcp00, is.numeric)
BFIR_rcp45[is.num] <- lapply(BFIR_rcp45[is.num], round, 3)
#Exporting yield curve
txt <- "*Y NA BFIR ? ? RCP45"
tmp <- "BFIR/BFIR_RCP45.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp45, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "    ")

#Calculating new variable for RCP85 total volume
XXXX_vol85$VOLtot <- rowSums(XXXX_vol85[,2:ncol(XXXX_vol85)])
XXXX_vol85 = dplyr::rename(XXXX_vol85, Age = Year)
BFIR_rcp85 <- merge (XXXX_vol85, XXXX_hig85, by = "Age")
BFIR_rcp85 <- merge (BFIR_rcp85, XXXX_dbh85, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp85))
names(BFIR_rcp85)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
colnames(BFIR_rcp85)[colnames(BFIR_rcp85) == 'Age'] <- '_Age'
is.num <- sapply(BFIR_rcp00, is.numeric)
BFIR_rcp85[is.num] <- lapply(BFIR_rcp85[is.num], round, 3)
#Exporting yield curve
txt <- "*Y NA BFIR ? ? RCP85"
tmp <- "BFIR/BFIR_RCP85.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp85, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "    ")
