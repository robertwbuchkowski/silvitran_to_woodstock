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

#Forcing R not to use scientific notation
options(scipen = 999)

Var.names <- read.csv("Variable_names.csv")
Var.names = dplyr::rename(Var.names, Names = ï..Names)

P_Ratios <- read.csv("Product_Ratio_V2.csv")
P_Ratios = dplyr::rename(P_Ratios, Specie = ï..Specie)
P_Ratios$Specie <- as.factor(P_Ratios$Specie)
P_Ratios$Product <- as.factor(P_Ratios$Product)
P_Ratios = dplyr::rename(P_Ratios, DBH90_2 = Tree.diameter)
P_Ratios = dplyr::rename(P_Ratios, HGT90_2 = Tree_height)

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

#Subset to keep general conifer ratios
OC_Ratio <- subset(P_Ratios, Specie =="OC")
OC_Ratio <- OC_Ratio[,-c(1, 6)]
OC_log <- subset(OC_Ratio, Product =="Logs")
OC_stud <- subset(OC_Ratio, Product =="Stud")
OC_pulp <- subset(OC_Ratio, Product =="Pulp")
OC_biomass <- subset(OC_Ratio, Product =="Biom")
OC_Ratio <- as.data.table(OC_Ratio)
OC_log <- as.data.table(OC_log)
OC_pulp <- as.data.table(OC_pulp)
OC_biomass <- as.data.table(OC_biomass)

#subset to keep general hardwood ratios
OH_Ratio <- subset(P_Ratios, Specie =="AS")
OH_Ratio <- OH_Ratio[,-c(1, 6)]
OH_log <- subset(OH_Ratio, Product =="Logs")
OH_stud <- subset(OH_Ratio, Product =="Stud")
OH_pulp <- subset(OC_Ratio, Product =="Pulp")
OH_biomass <- subset(OH_Ratio, Product =="Biom")
OH_Ratio <- as.data.table(OH_Ratio)
OH_log <- as.data.table(OH_log)
OH_pulp <- as.data.table(OH_pulp)
OH_biomass <- as.data.table(OH_biomass)

#RCP4.5
XXXX_oth45_2020 <- read.csv("BFIR/BFIR_RCP45_20202049_other.csv")
XXXX_vol45_2020 <- read.csv("BFIR/BFIR_RCP45_20202049_volume.csv")
XXXX_hig45_2020 <- read.csv("BFIR/BFIR_RCP45_20202049_height.csv")
XXXX_dbh45_2020 <- read.csv("BFIR/BFIR_RCP45_20202049_DBH.csv")
XXXX_oth45_2050 <- read.csv("BFIR/BFIR_RCP45_20502079_other.csv")
XXXX_vol45_2050 <- read.csv("BFIR/BFIR_RCP45_20502079_volume.csv")
XXXX_hig45_2050 <- read.csv("BFIR/BFIR_RCP45_20502079_height.csv")
XXXX_dbh45_2050 <- read.csv("BFIR/BFIR_RCP45_20502079_DBH.csv")
XXXX_oth45_2080 <- read.csv("BFIR/BFIR_RCP45_20802110_other.csv")
XXXX_vol45_2080 <- read.csv("BFIR/BFIR_RCP45_20802110_volume.csv")
XXXX_hig45_2080 <- read.csv("BFIR/BFIR_RCP45_20802110_height.csv")
XXXX_dbh45_2080 <- read.csv("BFIR/BFIR_RCP45_20802110_DBH.csv")

#RCP8.5
XXXX_oth85_2020 <- read.csv("BFIR/BFIR_RCP85_20202049_other.csv")
XXXX_vol85_2020 <- read.csv("BFIR/BFIR_RCP85_20202049_volume.csv")
XXXX_hig85_2020 <- read.csv("BFIR/BFIR_RCP85_20202049_height.csv")
XXXX_dbh85_2020 <- read.csv("BFIR/BFIR_RCP85_20202049_DBH.csv")
XXXX_oth85_2050 <- read.csv("BFIR/BFIR_RCP85_20502079_other.csv")
XXXX_vol85_2050 <- read.csv("BFIR/BFIR_RCP85_20502079_volume.csv")
XXXX_hig85_2050 <- read.csv("BFIR/BFIR_RCP85_20502079_height.csv")
XXXX_dbh85_2050 <- read.csv("BFIR/BFIR_RCP85_20502079_DBH.csv")
XXXX_oth85_2080 <- read.csv("BFIR/BFIR_RCP85_20802110_other.csv")
XXXX_vol85_2080 <- read.csv("BFIR/BFIR_RCP85_20802110_volume.csv")
XXXX_hig85_2080 <- read.csv("BFIR/BFIR_RCP85_20802110_height.csv")
XXXX_dbh85_2080 <- read.csv("BFIR/BFIR_RCP85_20802110_DBH.csv")


#MEAN_HEIGHT RCP45
#height integration to 2020-2049
XXXX_hig45_2020 <- as.data.table(XXXX_hig45_2020)
XXXX_hig45_2020 <- XXXX_hig45_2020[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig45_2020 <- aggregate(XXXX_hig45_2020, by=list(Age=XXXX_hig45_2020$Year), FUN=sum)
XXXX_hig45_2020 <- as.data.table(XXXX_hig45_2020)
XXXX_hig45_2020 <- XXXX_hig45_2020[,-c("Year")]
require(reshape2)
XXXX_hig45_2020 <- setDT(XXXX_hig45_2020)
XXXX_hig45_2020 <- melt(XXXX_hig45_2020, id.vars =  c("Age"))
XXXX_hig45_2020$Age <- as.factor(XXXX_hig45_2020$Age)
XXXX_hig45_2020$value <- as.integer(XXXX_hig45_2020$value)
XXXX_hig45_2020$variable <- paste(XXXX_hig45_2020$Age,"_", XXXX_hig45_2020$variable)
XXXX_hig45_2020 <- XXXX_hig45_2020[,-c(1)]
XXXX_hig45_2020 <- uncount(XXXX_hig45_2020, value, .id = "Tree")
XXXX_hig45_2020 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig45_2020$variable), "_", fixed = TRUE)))
XXXX_hig45_2020 = dplyr::rename(XXXX_hig45_2020, Age = X1)
XXXX_hig45_2020 = dplyr::rename(XXXX_hig45_2020, Height = X3)
XXXX_hig45_2020 <- XXXX_hig45_2020[,-c(2)]
XXXX_hig45_2020 <- as.data.table(XXXX_hig45_2020)
XXXX_hig45_2020$Height <- as.integer(XXXX_hig45_2020$Height)
XXXX_hig45_2020$Age <- as.integer(XXXX_hig45_2020$Age)
#To get middle of height class
XXXX_hig45_2020[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig45_2020 <- XXXX_hig45_2020 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))

#height integration to 2050-2079
XXXX_hig45_2050 <- as.data.table(XXXX_hig45_2050)
XXXX_hig45_2050 <- XXXX_hig45_2050[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig45_2050 <- aggregate(XXXX_hig45_2050, by=list(Age=XXXX_hig45_2050$Year), FUN=sum)
XXXX_hig45_2050 <- as.data.table(XXXX_hig45_2050)
XXXX_hig45_2050 <- XXXX_hig45_2050[,-c("Year")]
require(reshape2)
XXXX_hig45_2050 <- setDT(XXXX_hig45_2050)
XXXX_hig45_2050 <- melt(XXXX_hig45_2050, id.vars =  c("Age"))
XXXX_hig45_2050$Age <- as.factor(XXXX_hig45_2050$Age)
XXXX_hig45_2050$value <- as.integer(XXXX_hig45_2050$value)
XXXX_hig45_2050$variable <- paste(XXXX_hig45_2050$Age,"_", XXXX_hig45_2050$variable)
XXXX_hig45_2050 <- XXXX_hig45_2050[,-c(1)]
XXXX_hig45_2050 <- uncount(XXXX_hig45_2050, value, .id = "Tree")
XXXX_hig45_2050 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig45_2050$variable), "_", fixed = TRUE)))
XXXX_hig45_2050 = dplyr::rename(XXXX_hig45_2050, Age = X1)
XXXX_hig45_2050 = dplyr::rename(XXXX_hig45_2050, Height = X3)
XXXX_hig45_2050 <- XXXX_hig45_2050[,-c(2)]
XXXX_hig45_2050 <- as.data.table(XXXX_hig45_2050)
XXXX_hig45_2050$Height <- as.integer(XXXX_hig45_2050$Height)
XXXX_hig45_2050$Age <- as.integer(XXXX_hig45_2050$Age)
#To get middle of height class
XXXX_hig45_2050[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig45_2050 <- XXXX_hig45_2050 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))

#height integration to 2080-2110
XXXX_hig45_2080 <- as.data.table(XXXX_hig45_2080)
XXXX_hig45_2080 <- XXXX_hig45_2080[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig45_2080 <- aggregate(XXXX_hig45_2080, by=list(Age=XXXX_hig45_2080$Year), FUN=sum)
XXXX_hig45_2080 <- as.data.table(XXXX_hig45_2080)
XXXX_hig45_2080 <- XXXX_hig45_2080[,-c("Year")]
require(reshape2)
XXXX_hig45_2080 <- setDT(XXXX_hig45_2080)
XXXX_hig45_2080 <- melt(XXXX_hig45_2080, id.vars =  c("Age"))
XXXX_hig45_2080$Age <- as.factor(XXXX_hig45_2080$Age)
XXXX_hig45_2080$value <- as.integer(XXXX_hig45_2080$value)
XXXX_hig45_2080$variable <- paste(XXXX_hig45_2080$Age,"_", XXXX_hig45_2080$variable)
XXXX_hig45_2080 <- XXXX_hig45_2080[,-c(1)]
XXXX_hig45_2080 <- uncount(XXXX_hig45_2080, value, .id = "Tree")
XXXX_hig45_2080 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig45_2080$variable), "_", fixed = TRUE)))
XXXX_hig45_2080 = dplyr::rename(XXXX_hig45_2080, Age = X1)
XXXX_hig45_2080 = dplyr::rename(XXXX_hig45_2080, Height = X3)
XXXX_hig45_2080 <- XXXX_hig45_2080[,-c(2)]
XXXX_hig45_2080 <- as.data.table(XXXX_hig45_2080)
XXXX_hig45_2080$Height <- as.integer(XXXX_hig45_2080$Height)
XXXX_hig45_2080$Age <- as.integer(XXXX_hig45_2080$Age)
#To get middle of height class
XXXX_hig45_2080[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig45_2080 <- XXXX_hig45_2080 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))


#MEAN_HEIGHT RCP85
#height integration to 2020-2049
XXXX_hig85_2020 <- as.data.table(XXXX_hig85_2020)
XXXX_hig85_2020 <- XXXX_hig85_2020[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig85_2020 <- aggregate(XXXX_hig85_2020, by=list(Age=XXXX_hig85_2020$Year), FUN=sum)
XXXX_hig85_2020 <- as.data.table(XXXX_hig85_2020)
XXXX_hig85_2020 <- XXXX_hig85_2020[,-c("Year")]
require(reshape2)
XXXX_hig85_2020 <- setDT(XXXX_hig85_2020)
XXXX_hig85_2020 <- melt(XXXX_hig85_2020, id.vars =  c("Age"))
XXXX_hig85_2020$Age <- as.factor(XXXX_hig85_2020$Age)
XXXX_hig85_2020$value <- as.integer(XXXX_hig85_2020$value)
XXXX_hig85_2020$variable <- paste(XXXX_hig85_2020$Age,"_", XXXX_hig85_2020$variable)
XXXX_hig85_2020 <- XXXX_hig85_2020[,-c(1)]
XXXX_hig85_2020 <- uncount(XXXX_hig85_2020, value, .id = "Tree")
XXXX_hig85_2020 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig85_2020$variable), "_", fixed = TRUE)))
XXXX_hig85_2020 = dplyr::rename(XXXX_hig85_2020, Age = X1)
XXXX_hig85_2020 = dplyr::rename(XXXX_hig85_2020, Height = X3)
XXXX_hig85_2020 <- XXXX_hig85_2020[,-c(2)]
XXXX_hig85_2020 <- as.data.table(XXXX_hig85_2020)
XXXX_hig85_2020$Height <- as.integer(XXXX_hig85_2020$Height)
XXXX_hig85_2020$Age <- as.integer(XXXX_hig85_2020$Age)
#To get middle of height class
XXXX_hig85_2020[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig85_2020 <- XXXX_hig85_2020 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))

#height integration to 2050-2079
XXXX_hig85_2050 <- as.data.table(XXXX_hig85_2050)
XXXX_hig85_2050 <- XXXX_hig85_2050[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig85_2050 <- aggregate(XXXX_hig85_2050, by=list(Age=XXXX_hig85_2050$Year), FUN=sum)
XXXX_hig85_2050 <- as.data.table(XXXX_hig85_2050)
XXXX_hig85_2050 <- XXXX_hig85_2050[,-c("Year")]
require(reshape2)
XXXX_hig85_2050 <- setDT(XXXX_hig85_2050)
XXXX_hig85_2050 <- melt(XXXX_hig85_2050, id.vars =  c("Age"))
XXXX_hig85_2050$Age <- as.factor(XXXX_hig85_2050$Age)
XXXX_hig85_2050$value <- as.integer(XXXX_hig85_2050$value)
XXXX_hig85_2050$variable <- paste(XXXX_hig85_2050$Age,"_", XXXX_hig85_2050$variable)
XXXX_hig85_2050 <- XXXX_hig85_2050[,-c(1)]
XXXX_hig85_2050 <- uncount(XXXX_hig85_2050, value, .id = "Tree")
XXXX_hig85_2050 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig85_2050$variable), "_", fixed = TRUE)))
XXXX_hig85_2050 = dplyr::rename(XXXX_hig85_2050, Age = X1)
XXXX_hig85_2050 = dplyr::rename(XXXX_hig85_2050, Height = X3)
XXXX_hig85_2050 <- XXXX_hig85_2050[,-c(2)]
XXXX_hig85_2050 <- as.data.table(XXXX_hig85_2050)
XXXX_hig85_2050$Height <- as.integer(XXXX_hig85_2050$Height)
XXXX_hig85_2050$Age <- as.integer(XXXX_hig85_2050$Age)
#To get middle of height class
XXXX_hig85_2050[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig85_2050 <- XXXX_hig85_2050 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))

#height integration to 2080-2110
XXXX_hig85_2080 <- as.data.table(XXXX_hig85_2080)
XXXX_hig85_2080 <- XXXX_hig85_2080[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig85_2080 <- aggregate(XXXX_hig85_2080, by=list(Age=XXXX_hig85_2080$Year), FUN=sum)
XXXX_hig85_2080 <- as.data.table(XXXX_hig85_2080)
XXXX_hig85_2080 <- XXXX_hig85_2080[,-c("Year")]
require(reshape2)
XXXX_hig85_2080 <- setDT(XXXX_hig85_2080)
XXXX_hig85_2080 <- melt(XXXX_hig85_2080, id.vars =  c("Age"))
XXXX_hig85_2080$Age <- as.factor(XXXX_hig85_2080$Age)
XXXX_hig85_2080$value <- as.integer(XXXX_hig85_2080$value)
XXXX_hig85_2080$variable <- paste(XXXX_hig85_2080$Age,"_", XXXX_hig85_2080$variable)
XXXX_hig85_2080 <- XXXX_hig85_2080[,-c(1)]
XXXX_hig85_2080 <- uncount(XXXX_hig85_2080, value, .id = "Tree")
XXXX_hig85_2080 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_hig85_2080$variable), "_", fixed = TRUE)))
XXXX_hig85_2080 = dplyr::rename(XXXX_hig85_2080, Age = X1)
XXXX_hig85_2080 = dplyr::rename(XXXX_hig85_2080, Height = X3)
XXXX_hig85_2080 <- XXXX_hig85_2080[,-c(2)]
XXXX_hig85_2080 <- as.data.table(XXXX_hig85_2080)
XXXX_hig85_2080$Height <- as.integer(XXXX_hig85_2080$Height)
XXXX_hig85_2080$Age <- as.integer(XXXX_hig85_2080$Age)
#To get middle of height class
XXXX_hig85_2080[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_hig85_2080 <- XXXX_hig85_2080 %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))


#MEAN_DIAMETER and DENSITY RCP45
#dbh integration to 2020-2049
XXXX_dbh45_2020 <- as.data.table(XXXX_dbh45_2020)
XXXX_dbh45_2020 <- XXXX_dbh45_2020[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh45_2020 <- aggregate(XXXX_dbh45_2020, by=list(Age=XXXX_dbh45_2020$Year), FUN=sum)
XXXX_dbh45_2020 <- as.data.table(XXXX_dbh45_2020)
XXXX_dbh45_2020 <- XXXX_dbh45_2020[,-c("Year")]
#adding density
XXXX_dty45_2020 <- XXXX_dbh45_2020[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty45_2020 <- XXXX_dbh45_2020[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh45_2020 <- XXXX_dbh45_2020[,-c("DTYtot", "DTY9")]
XXXX_dty45_2020 <- XXXX_dty45_2020[,c("Age", "DTYtot", "DTY9")]
XXXX_dty45_2020[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh45_2020 <- setDT(XXXX_dbh45_2020)
XXXX_dbh45_2020 <- melt(XXXX_dbh45_2020, id.vars =  c("Age"))
XXXX_dbh45_2020$Age <- as.factor(XXXX_dbh45_2020$Age)
XXXX_dbh45_2020$value <- as.integer(XXXX_dbh45_2020$value)
XXXX_dbh45_2020$variable <- paste(XXXX_dbh45_2020$Age,"_", XXXX_dbh45_2020$variable)
XXXX_dbh45_2020 <- XXXX_dbh45_2020[,-c(1)]
XXXX_dbh45_2020 <- uncount(XXXX_dbh45_2020, value, .id = "Tree")
XXXX_dbh45_2020 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh45_2020$variable), "_", fixed = TRUE)))
XXXX_dbh45_2020 = dplyr::rename(XXXX_dbh45_2020, Age = X1)
XXXX_dbh45_2020 = dplyr::rename(XXXX_dbh45_2020, DBH = X3)
XXXX_dbh45_2020 <- XXXX_dbh45_2020[,-c(2)]
XXXX_dbh45_2020 <- as.data.table(XXXX_dbh45_2020)
XXXX_dbh45_2020$DBH <- as.integer(XXXX_dbh45_2020$DBH)
XXXX_dbh45_2020$Age <- as.integer(XXXX_dbh45_2020$Age)
#To get middle of DBH class
XXXX_dbh45_2020[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh45_2020 <- XXXX_dbh45_2020 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))

#dbh integration to 2050-2079
XXXX_dbh45_2050 <- as.data.table(XXXX_dbh45_2050)
XXXX_dbh45_2050 <- XXXX_dbh45_2050[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh45_2050 <- aggregate(XXXX_dbh45_2050, by=list(Age=XXXX_dbh45_2050$Year), FUN=sum)
XXXX_dbh45_2050 <- as.data.table(XXXX_dbh45_2050)
XXXX_dbh45_2050 <- XXXX_dbh45_2050[,-c("Year")]
#adding density
XXXX_dty45_2050 <- XXXX_dbh45_2050[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty45_2050 <- XXXX_dbh45_2050[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh45_2050 <- XXXX_dbh45_2050[,-c("DTYtot", "DTY9")]
XXXX_dty45_2050 <- XXXX_dty45_2050[,c("Age", "DTYtot", "DTY9")]
XXXX_dty45_2050[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh45_2050 <- setDT(XXXX_dbh45_2050)
XXXX_dbh45_2050 <- melt(XXXX_dbh45_2050, id.vars =  c("Age"))
XXXX_dbh45_2050$Age <- as.factor(XXXX_dbh45_2050$Age)
XXXX_dbh45_2050$value <- as.integer(XXXX_dbh45_2050$value)
XXXX_dbh45_2050$variable <- paste(XXXX_dbh45_2050$Age,"_", XXXX_dbh45_2050$variable)
XXXX_dbh45_2050 <- XXXX_dbh45_2050[,-c(1)]
XXXX_dbh45_2050 <- uncount(XXXX_dbh45_2050, value, .id = "Tree")
XXXX_dbh45_2050 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh45_2050$variable), "_", fixed = TRUE)))
XXXX_dbh45_2050 = dplyr::rename(XXXX_dbh45_2050, Age = X1)
XXXX_dbh45_2050 = dplyr::rename(XXXX_dbh45_2050, DBH = X3)
XXXX_dbh45_2050 <- XXXX_dbh45_2050[,-c(2)]
XXXX_dbh45_2050 <- as.data.table(XXXX_dbh45_2050)
XXXX_dbh45_2050$DBH <- as.integer(XXXX_dbh45_2050$DBH)
XXXX_dbh45_2050$Age <- as.integer(XXXX_dbh45_2050$Age)
#To get middle of DBH class
XXXX_dbh45_2050[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh45_2050 <- XXXX_dbh45_2050 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))

#dbh integration to 2080-2110
XXXX_dbh45_2080 <- as.data.table(XXXX_dbh45_2080)
XXXX_dbh45_2080 <- XXXX_dbh45_2080[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh45_2080 <- aggregate(XXXX_dbh45_2080, by=list(Age=XXXX_dbh45_2080$Year), FUN=sum)
XXXX_dbh45_2080 <- as.data.table(XXXX_dbh45_2080)
XXXX_dbh45_2080 <- XXXX_dbh45_2080[,-c("Year")]
#adding density
XXXX_dty45_2080 <- XXXX_dbh45_2080[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty45_2080 <- XXXX_dbh45_2080[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh45_2080 <- XXXX_dbh45_2080[,-c("DTYtot", "DTY9")]
XXXX_dty45_2080 <- XXXX_dty45_2080[,c("Age", "DTYtot", "DTY9")]
XXXX_dty45_2080[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh45_2080 <- setDT(XXXX_dbh45_2080)
XXXX_dbh45_2080 <- melt(XXXX_dbh45_2080, id.vars =  c("Age"))
XXXX_dbh45_2080$Age <- as.factor(XXXX_dbh45_2080$Age)
XXXX_dbh45_2080$value <- as.integer(XXXX_dbh45_2080$value)
XXXX_dbh45_2080$variable <- paste(XXXX_dbh45_2080$Age,"_", XXXX_dbh45_2080$variable)
XXXX_dbh45_2080 <- XXXX_dbh45_2080[,-c(1)]
XXXX_dbh45_2080 <- uncount(XXXX_dbh45_2080, value, .id = "Tree")
XXXX_dbh45_2080 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh45_2080$variable), "_", fixed = TRUE)))
XXXX_dbh45_2080 = dplyr::rename(XXXX_dbh45_2080, Age = X1)
XXXX_dbh45_2080 = dplyr::rename(XXXX_dbh45_2080, DBH = X3)
XXXX_dbh45_2080 <- XXXX_dbh45_2080[,-c(2)]
XXXX_dbh45_2080 <- as.data.table(XXXX_dbh45_2080)
XXXX_dbh45_2080$DBH <- as.integer(XXXX_dbh45_2080$DBH)
XXXX_dbh45_2080$Age <- as.integer(XXXX_dbh45_2080$Age)
#To get middle of DBH class
XXXX_dbh45_2080[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh45_2080 <- XXXX_dbh45_2080 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))


#MEAN_DIAMETER and DENSITY RCP85
#dbh integration to 2020-2049
XXXX_dbh85_2020 <- as.data.table(XXXX_dbh85_2020)
XXXX_dbh85_2020 <- XXXX_dbh85_2020[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh85_2020 <- aggregate(XXXX_dbh85_2020, by=list(Age=XXXX_dbh85_2020$Year), FUN=sum)
XXXX_dbh85_2020 <- as.data.table(XXXX_dbh85_2020)
XXXX_dbh85_2020 <- XXXX_dbh85_2020[,-c("Year")]
#adding density
XXXX_dty85_2020 <- XXXX_dbh85_2020[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty85_2020 <- XXXX_dbh85_2020[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh85_2020 <- XXXX_dbh85_2020[,-c("DTYtot", "DTY9")]
XXXX_dty85_2020 <- XXXX_dty85_2020[,c("Age", "DTYtot", "DTY9")]
XXXX_dty85_2020[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh85_2020 <- setDT(XXXX_dbh85_2020)
XXXX_dbh85_2020 <- melt(XXXX_dbh85_2020, id.vars =  c("Age"))
XXXX_dbh85_2020$Age <- as.factor(XXXX_dbh85_2020$Age)
XXXX_dbh85_2020$value <- as.integer(XXXX_dbh85_2020$value)
XXXX_dbh85_2020$variable <- paste(XXXX_dbh85_2020$Age,"_", XXXX_dbh85_2020$variable)
XXXX_dbh85_2020 <- XXXX_dbh85_2020[,-c(1)]
XXXX_dbh85_2020 <- uncount(XXXX_dbh85_2020, value, .id = "Tree")
XXXX_dbh85_2020 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh85_2020$variable), "_", fixed = TRUE)))
XXXX_dbh85_2020 = dplyr::rename(XXXX_dbh85_2020, Age = X1)
XXXX_dbh85_2020 = dplyr::rename(XXXX_dbh85_2020, DBH = X3)
XXXX_dbh85_2020 <- XXXX_dbh85_2020[,-c(2)]
XXXX_dbh85_2020 <- as.data.table(XXXX_dbh85_2020)
XXXX_dbh85_2020$DBH <- as.integer(XXXX_dbh85_2020$DBH)
XXXX_dbh85_2020$Age <- as.integer(XXXX_dbh85_2020$Age)
#To get middle of DBH class
XXXX_dbh85_2020[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh85_2020 <- XXXX_dbh85_2020 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))

#dbh integration to 2050-2079
XXXX_dbh85_2050 <- as.data.table(XXXX_dbh85_2050)
XXXX_dbh85_2050 <- XXXX_dbh85_2050[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh85_2050 <- aggregate(XXXX_dbh85_2050, by=list(Age=XXXX_dbh85_2050$Year), FUN=sum)
XXXX_dbh85_2050 <- as.data.table(XXXX_dbh85_2050)
XXXX_dbh85_2050 <- XXXX_dbh85_2050[,-c("Year")]
#adding density
XXXX_dty85_2050 <- XXXX_dbh85_2050[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty85_2050 <- XXXX_dbh85_2050[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh85_2050 <- XXXX_dbh85_2050[,-c("DTYtot", "DTY9")]
XXXX_dty85_2050 <- XXXX_dty85_2050[,c("Age", "DTYtot", "DTY9")]
XXXX_dty85_2050[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh85_2050 <- setDT(XXXX_dbh85_2050)
XXXX_dbh85_2050 <- melt(XXXX_dbh85_2050, id.vars =  c("Age"))
XXXX_dbh85_2050$Age <- as.factor(XXXX_dbh85_2050$Age)
XXXX_dbh85_2050$value <- as.integer(XXXX_dbh85_2050$value)
XXXX_dbh85_2050$variable <- paste(XXXX_dbh85_2050$Age,"_", XXXX_dbh85_2050$variable)
XXXX_dbh85_2050 <- XXXX_dbh85_2050[,-c(1)]
XXXX_dbh85_2050 <- uncount(XXXX_dbh85_2050, value, .id = "Tree")
XXXX_dbh85_2050 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh85_2050$variable), "_", fixed = TRUE)))
XXXX_dbh85_2050 = dplyr::rename(XXXX_dbh85_2050, Age = X1)
XXXX_dbh85_2050 = dplyr::rename(XXXX_dbh85_2050, DBH = X3)
XXXX_dbh85_2050 <- XXXX_dbh85_2050[,-c(2)]
XXXX_dbh85_2050 <- as.data.table(XXXX_dbh85_2050)
XXXX_dbh85_2050$DBH <- as.integer(XXXX_dbh85_2050$DBH)
XXXX_dbh85_2050$Age <- as.integer(XXXX_dbh85_2050$Age)
#To get middle of DBH class
XXXX_dbh85_2050[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh85_2050 <- XXXX_dbh85_2050 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))

#dbh integration to 2080-2110
XXXX_dbh85_2080 <- as.data.table(XXXX_dbh85_2080)
XXXX_dbh85_2080 <- XXXX_dbh85_2080[,-c("Species")]
#number of stem per age class and DBH class all species combined
XXXX_dbh85_2080 <- aggregate(XXXX_dbh85_2080, by=list(Age=XXXX_dbh85_2080$Year), FUN=sum)
XXXX_dbh85_2080 <- as.data.table(XXXX_dbh85_2080)
XXXX_dbh85_2080 <- XXXX_dbh85_2080[,-c("Year")]
#adding density
XXXX_dty85_2080 <- XXXX_dbh85_2080[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
XXXX_dty85_2080 <- XXXX_dbh85_2080[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
XXXX_dbh85_2080 <- XXXX_dbh85_2080[,-c("DTYtot", "DTY9")]
XXXX_dty85_2080 <- XXXX_dty85_2080[,c("Age", "DTYtot", "DTY9")]
XXXX_dty85_2080[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
XXXX_dbh85_2080 <- setDT(XXXX_dbh85_2080)
XXXX_dbh85_2080 <- melt(XXXX_dbh85_2080, id.vars =  c("Age"))
XXXX_dbh85_2080$Age <- as.factor(XXXX_dbh85_2080$Age)
XXXX_dbh85_2080$value <- as.integer(XXXX_dbh85_2080$value)
XXXX_dbh85_2080$variable <- paste(XXXX_dbh85_2080$Age,"_", XXXX_dbh85_2080$variable)
XXXX_dbh85_2080 <- XXXX_dbh85_2080[,-c(1)]
XXXX_dbh85_2080 <- uncount(XXXX_dbh85_2080, value, .id = "Tree")
XXXX_dbh85_2080 <- data.frame(do.call("rbind", strsplit(as.character(XXXX_dbh85_2080$variable), "_", fixed = TRUE)))
XXXX_dbh85_2080 = dplyr::rename(XXXX_dbh85_2080, Age = X1)
XXXX_dbh85_2080 = dplyr::rename(XXXX_dbh85_2080, DBH = X3)
XXXX_dbh85_2080 <- XXXX_dbh85_2080[,-c(2)]
XXXX_dbh85_2080 <- as.data.table(XXXX_dbh85_2080)
XXXX_dbh85_2080$DBH <- as.integer(XXXX_dbh85_2080$DBH)
XXXX_dbh85_2080$Age <- as.integer(XXXX_dbh85_2080$Age)
#To get middle of DBH class
XXXX_dbh85_2080[ , DBH:=DBH + 2.5]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
XXXX_dbh85_2080 <- XXXX_dbh85_2080 %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))

#VOLUME RCP45
#Calculating new variable for 2020-2049
XXXX_vol45_2020$VOLtot <- rowSums(XXXX_vol45_2020[,2:ncol(XXXX_vol45_2020)])
XXXX_vol45_2020 = dplyr::rename(XXXX_vol45_2020, Age = Year)
BFIR_rcp45_2020 <- merge (XXXX_vol45_2020, XXXX_hig45_2020, by = "Age")
BFIR_rcp45_2020 <- merge (BFIR_rcp45_2020, XXXX_dbh45_2020, by = "Age")
is.num <- sapply(BFIR_rcp45_2020, is.numeric)
BFIR_rcp45_2020[is.num] <- lapply(BFIR_rcp45_2020[is.num], round, dec)
BFIR_rcp45_2020 <- merge (BFIR_rcp45_2020, XXXX_dty45_2020, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp45_2020))
names(BFIR_rcp45_2020)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]

#Calculating new variable for 2050-2079
XXXX_vol45_2050$VOLtot <- rowSums(XXXX_vol45_2050[,2:ncol(XXXX_vol45_2050)])
XXXX_vol45_2050 = dplyr::rename(XXXX_vol45_2050, Age = Year)
BFIR_rcp45_2050 <- merge (XXXX_vol45_2050, XXXX_hig45_2050, by = "Age")
BFIR_rcp45_2050 <- merge (BFIR_rcp45_2050, XXXX_dbh45_2050, by = "Age")
is.num <- sapply(BFIR_rcp45_2050, is.numeric)
BFIR_rcp45_2050[is.num] <- lapply(BFIR_rcp45_2050[is.num], round, dec)
BFIR_rcp45_2050 <- merge (BFIR_rcp45_2050, XXXX_dty45_2050, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp45_2050))
names(BFIR_rcp45_2050)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]

#Calculating new variable for 2080-2110
XXXX_vol45_2080$VOLtot <- rowSums(XXXX_vol45_2080[,2:ncol(XXXX_vol45_2080)])
XXXX_vol45_2080 = dplyr::rename(XXXX_vol45_2080, Age = Year)
BFIR_rcp45_2080 <- merge (XXXX_vol45_2080, XXXX_hig45_2080, by = "Age")
BFIR_rcp45_2080 <- merge (BFIR_rcp45_2080, XXXX_dbh45_2080, by = "Age")
is.num <- sapply(BFIR_rcp45_2080, is.numeric)
BFIR_rcp45_2080[is.num] <- lapply(BFIR_rcp45_2080[is.num], round, dec)
BFIR_rcp45_2080 <- merge (BFIR_rcp45_2080, XXXX_dty45_2080, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp45_2080))
names(BFIR_rcp45_2080)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]


#VOLUME RCP85
#Calculating new variable for 2020-2049
XXXX_vol85_2020$VOLtot <- rowSums(XXXX_vol85_2020[,2:ncol(XXXX_vol85_2020)])
XXXX_vol85_2020 = dplyr::rename(XXXX_vol85_2020, Age = Year)
BFIR_rcp85_2020 <- merge (XXXX_vol85_2020, XXXX_hig85_2020, by = "Age")
BFIR_rcp85_2020 <- merge (BFIR_rcp85_2020, XXXX_dbh85_2020, by = "Age")
is.num <- sapply(BFIR_rcp85_2020, is.numeric)
BFIR_rcp85_2020[is.num] <- lapply(BFIR_rcp85_2020[is.num], round, dec)
BFIR_rcp85_2020 <- merge (BFIR_rcp85_2020, XXXX_dty85_2020, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp85_2020))
names(BFIR_rcp85_2020)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]

#Calculating new variable for 2050-2079
XXXX_vol85_2050$VOLtot <- rowSums(XXXX_vol85_2050[,2:ncol(XXXX_vol85_2050)])
XXXX_vol85_2050 = dplyr::rename(XXXX_vol85_2050, Age = Year)
BFIR_rcp85_2050 <- merge (XXXX_vol85_2050, XXXX_hig85_2050, by = "Age")
BFIR_rcp85_2050 <- merge (BFIR_rcp85_2050, XXXX_dbh85_2050, by = "Age")
is.num <- sapply(BFIR_rcp85_2050, is.numeric)
BFIR_rcp85_2050[is.num] <- lapply(BFIR_rcp85_2050[is.num], round, dec)
BFIR_rcp85_2050 <- merge (BFIR_rcp85_2050, XXXX_dty85_2050, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp85_2050))
names(BFIR_rcp85_2050)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]

#Calculating new variable for 2080-2110
XXXX_vol85_2080$VOLtot <- rowSums(XXXX_vol85_2080[,2:ncol(XXXX_vol85_2080)])
XXXX_vol85_2080 = dplyr::rename(XXXX_vol85_2080, Age = Year)
BFIR_rcp85_2080 <- merge (XXXX_vol85_2080, XXXX_hig85_2080, by = "Age")
BFIR_rcp85_2080 <- merge (BFIR_rcp85_2080, XXXX_dbh85_2080, by = "Age")
is.num <- sapply(BFIR_rcp85_2080, is.numeric)
BFIR_rcp85_2080[is.num] <- lapply(BFIR_rcp85_2080[is.num], round, dec)
BFIR_rcp85_2080 <- merge (BFIR_rcp85_2080, XXXX_dty85_2080, by = "Age")
existing <- match(Var.names$Names,names(BFIR_rcp85_2080))
names(BFIR_rcp85_2080)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]

#RCP45
#Adding column to help with the merge of wood product ratios (Height are rounded to the closest multiple of 2)
BFIR_rcp45_2020$HGT90_2 <- RoundTo(BFIR_rcp45_2020$HGT90, multiple = 2, FUN = round)
BFIR_rcp45_2020$DBH90_2 <- RoundTo(BFIR_rcp45_2020$DBH90, multiple = 2, FUN = round)
BFIR_rcp45_2050$HGT90_2 <- RoundTo(BFIR_rcp45_2050$HGT90, multiple = 2, FUN = round)
BFIR_rcp45_2050$DBH90_2 <- RoundTo(BFIR_rcp45_2050$DBH90, multiple = 2, FUN = round)
BFIR_rcp45_2080$HGT90_2 <- RoundTo(BFIR_rcp45_2080$HGT90, multiple = 2, FUN = round)
BFIR_rcp45_2080$DBH90_2 <- RoundTo(BFIR_rcp45_2080$DBH90, multiple = 2, FUN = round)
#RCP85
BFIR_rcp85_2020$HGT90_2 <- RoundTo(BFIR_rcp85_2020$HGT90, multiple = 2, FUN = round)
BFIR_rcp85_2020$DBH90_2 <- RoundTo(BFIR_rcp85_2020$DBH90, multiple = 2, FUN = round)
BFIR_rcp85_2050$HGT90_2 <- RoundTo(BFIR_rcp85_2050$HGT90, multiple = 2, FUN = round)
BFIR_rcp85_2050$DBH90_2 <- RoundTo(BFIR_rcp85_2050$DBH90, multiple = 2, FUN = round)
BFIR_rcp85_2080$HGT90_2 <- RoundTo(BFIR_rcp85_2080$HGT90, multiple = 2, FUN = round)
BFIR_rcp85_2080$DBH90_2 <- RoundTo(BFIR_rcp85_2080$DBH90, multiple = 2, FUN = round)

#RCP45
#Period2020-2049
#merging ratios for EASTERN CEDAR
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, EClogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, ECstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, ECplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, SFlogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, SFstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, SFplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, WPlogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, WPstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, WPplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, IHlogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, IHstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, IHplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, THlogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, THstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, THplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, THbioR = Ratio)
#merging ratios for conifers
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OClogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OCstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OCplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OCbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OHlogR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OHstdR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OHplpR = Ratio)
BFIR_rcp45_2020 <- merge(BFIR_rcp45_2020, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2020 = dplyr::rename(BFIR_rcp45_2020, OHbioR = Ratio)

#Period2050-2079
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, EClogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, ECstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, ECplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, SFlogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, SFstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, SFplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, WPlogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, WPstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, WPplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, IHlogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, IHstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, IHplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, THlogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, THstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, THplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, THbioR = Ratio)
#merging ratios for conifers
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OClogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OCstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OCplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OCbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OHlogR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OHstdR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OHplpR = Ratio)
BFIR_rcp45_2050 <- merge(BFIR_rcp45_2050, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2050 = dplyr::rename(BFIR_rcp45_2050, OHbioR = Ratio)

#Period2080-2110
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, EClogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, ECstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, ECplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, SFlogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, SFstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, SFplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, WPlogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, WPstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, WPplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, IHlogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, IHstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, IHplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, THlogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, THstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, THplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, THbioR = Ratio)
#merging ratios for conifers
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OClogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OCstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OCplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OCbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OHlogR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OHstdR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OHplpR = Ratio)
BFIR_rcp45_2080 <- merge(BFIR_rcp45_2080, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp45_2080 = dplyr::rename(BFIR_rcp45_2080, OHbioR = Ratio)

#RCP85
#Period2020-2049
#merging ratios for EASTERN CEDAR
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, EClogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, ECstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, ECplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, SFlogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, SFstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, SFplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, WPlogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, WPstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, WPplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, IHlogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, IHstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, IHplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, THlogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, THstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, THplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, THbioR = Ratio)
#merging ratios for conifers
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OClogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OCstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OCplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OCbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OHlogR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OHstdR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OHplpR = Ratio)
BFIR_rcp85_2020 <- merge(BFIR_rcp85_2020, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2020 = dplyr::rename(BFIR_rcp85_2020, OHbioR = Ratio)

#Period2050-2079
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, EClogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, ECstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, ECplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, SFlogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, SFstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, SFplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, WPlogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, WPstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, WPplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, IHlogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, IHstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, IHplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, THlogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, THstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, THplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, THbioR = Ratio)
#merging ratios for conifers
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OClogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OCstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OCplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OCbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OHlogR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OHstdR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OHplpR = Ratio)
BFIR_rcp85_2050 <- merge(BFIR_rcp85_2050, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2050 = dplyr::rename(BFIR_rcp85_2050, OHbioR = Ratio)

#Period2080-2110
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, EClogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, ECstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, ECplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, ECbioR = Ratio)
#merging ratios for SPRUCE FIR
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, SFlogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, SFstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, SFplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, SFbioR = Ratio)
#merging ratios for WHITE PINE
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, WPlogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, WPstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, WPplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, WPbioR = Ratio)
#merging ratios for INTOLERANT HW
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, IHlogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, IHstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, IHplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, IHbioR = Ratio)
#merging ratios for TOLERANT HW
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, THlogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, THstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, THplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, THbioR = Ratio)
#merging ratios for conifers
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OClogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OCstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OCplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OCbioR = Ratio)
#merging ratios for hardwoods
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OHlogR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OHstdR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OHplpR = Ratio)
BFIR_rcp85_2080 <- merge(BFIR_rcp85_2080, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
BFIR_rcp85_2080 = dplyr::rename(BFIR_rcp85_2080, OHbioR = Ratio)

#RCP45
#Rearrange the tables
#Period 2020-2049
BFIR_rcp45_2020 <- BFIR_rcp45_2020[,-c(1, 2)]
BFIR_rcp45_2020 <- BFIR_rcp45_2020[order(BFIR_rcp45_2020$Age),]
colnames(BFIR_rcp45_2020)[colnames(BFIR_rcp45_2020) == 'Age'] <- '_Age'
#Period 2050-2079
BFIR_rcp45_2050 <- BFIR_rcp45_2050[,-c(1, 2)]
BFIR_rcp45_2050 <- BFIR_rcp45_2050[order(BFIR_rcp45_2050$Age),]
colnames(BFIR_rcp45_2050)[colnames(BFIR_rcp45_2050) == 'Age'] <- '_Age'
#Period 2080-2110
BFIR_rcp45_2080 <- BFIR_rcp45_2080[,-c(1, 2)]
BFIR_rcp45_2080 <- BFIR_rcp45_2080[order(BFIR_rcp45_2080$Age),]
colnames(BFIR_rcp45_2080)[colnames(BFIR_rcp45_2080) == 'Age'] <- '_Age'

#RCP85
#Period 2020-2049
BFIR_rcp85_2020 <- BFIR_rcp85_2020[,-c(1, 2)]
BFIR_rcp85_2020 <- BFIR_rcp85_2020[order(BFIR_rcp85_2020$Age),]
colnames(BFIR_rcp85_2020)[colnames(BFIR_rcp85_2020) == 'Age'] <- '_Age'
#Period 2050-2079
BFIR_rcp85_2050 <- BFIR_rcp85_2050[,-c(1, 2)]
BFIR_rcp85_2050 <- BFIR_rcp85_2050[order(BFIR_rcp85_2050$Age),]
colnames(BFIR_rcp85_2050)[colnames(BFIR_rcp85_2050) == 'Age'] <- '_Age'
#Period 2080-2110
BFIR_rcp85_2080 <- BFIR_rcp85_2080[,-c(1, 2)]
BFIR_rcp85_2080 <- BFIR_rcp85_2080[order(BFIR_rcp85_2080$Age),]
colnames(BFIR_rcp85_2080)[colnames(BFIR_rcp85_2080) == 'Age'] <- '_Age'

#RCP45
#Exporting yield curve
#Period 2020-2049 age group 0
txt <- "*Y NA BFIR ? ? 0 P1 45"
tmp <- "BFIR/BFIR_rcp45_2020_0.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp45_2020, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(BFIR_rcp45_2020,"BFIR/BFIR_rcp45_2020_0.csv", row.names = FALSE)
#Period 2050-2079
txt <- "*Y NA BFIR ? ? X P2 45"
tmp <- "BFIR/BFIR_rcp45_2050.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp45_2050, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(BFIR_rcp45_2050,"BFIR/BFIR_rcp45_2050.csv", row.names = FALSE)
#Period 2080-2110
txt <- "*Y NA BFIR ? ? X P3 45"
tmp <- "BFIR/BFIR_rcp45_2080.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp45_2080, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(BFIR_rcp45_2080,"BFIR/BFIR_rcp45_2080.csv", row.names = FALSE)

#RCP85
#Period 2020-2049 age group 0
txt <- "*Y NA BFIR ? ? 0 P1 85"
tmp <- "BFIR/BFIR_rcp85_2020_0.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp85_2020, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(BFIR_rcp85_2020,"BFIR/BFIR_rcp85_2020_0.csv", row.names = FALSE)
#Period 2050-2079
txt <- "*Y NA BFIR ? ? X P2 85"
tmp <- "BFIR/BFIR_rcp85_2050.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp85_2050, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(BFIR_rcp85_2050,"BFIR/BFIR_rcp85_2050.csv", row.names = FALSE)
#Period 2080-2110
txt <- "*Y NA BFIR ? ? X P3 85"
tmp <- "BFIR/BFIR_rcp85_2080.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp85_2080, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(BFIR_rcp85_2080,"BFIR/BFIR_rcp85_2080.csv", row.names = FALSE)
