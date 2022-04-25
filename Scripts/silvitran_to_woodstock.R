# This is where the script will be.
#Hi ROB!

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

BFIR_vol00 <- read.csv("BFIR/BFIR_Baseline_volume.csv")
BFIR_dbh00 <- read.csv("BFIR/BFIR_Baseline_DBH.csv")
BFIR_hig00 <- read.csv("BFIR/BFIR_Baseline_height.csv")

BFIR_vol45 <- read.csv("BFIR/BFIR_RCP45_volume.csv")
BFIR_dbh45 <- read.csv("BFIR/BFIR_RCP45_DBH.csv")
BFIR_hig45 <- read.csv("BFIR/BFIR_RCP45_height.csv")

BFIR_vol85 <- read.csv("BFIR/BFIR_RCP85_volume.csv")
BFIR_dbh85 <- read.csv("BFIR/BFIR_RCP85_DBH.csv")
BFIR_hig85 <- read.csv("BFIR/BFIR_RCP85_height.csv")

BFIR_hig00 <- as.data.table(BFIR_hig00)
BFIR_hig00 <- BFIR_hig00[,-c("Species")]

#number of stem per age class and height class all species combined
BFIR_hig00 <- aggregate(BFIR_hig00, by=list(Age=BFIR_hig00$Year), FUN=sum)
BFIR_hig00 <- as.data.table(BFIR_hig00)
BFIR_hig00 <- BFIR_hig00[,-c("Year")]
BFIR_hig00 <- BFIR_hig00[,-c("Age")]

#calculate % (in decimals) of the number of trees per height class
BFIR_hig00$Tot <- rowSums( BFIR_hig00[,1:26] )
BFIR_hig00 <- as.data.table(BFIR_hig00)
cols <- c(1:26)
BFIR_hig00[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
BFIR_hig00 <- BFIR_hig00[,-c("Tot")]

BFIR_hig00 = t(apply(BFIR_hig00, 1, cumsum))

BFIR_hig00[BFIR_hig00 > 0.899 & BFIR_hig00 < 1]<-1





BFIR_hig00 <- as.matrix(BFIR_hig00)


few <- apply(BFIR_hig00, 1, function(x) quantile(x[x>=1], probs=.95))



#summing all columns to get the total stems/ha for each species and age class
#BFIR_hig00[, Tot_stems := Reduce(`+`, .SD), .SDcol = 3:28]




#cumputing avg weighted hight
#BFIR_hig00[, avg_hig := (Cls_1*1 + Cls_3*3 + Cls_5*5 + Cls_7*7 + Cls_9*9 + Cls_11*11 + Cls_13*13 +
#             Cls_15*15 + Cls_17*17 +Cls_19*19 + Cls_21*21 + Cls_23*23 + Cls_25*25 + Cls_27*27 + Cls_29*29 +
#             Cls_31*31 + Cls_33*33 + Cls_35*35 + Cls_37*37 + Cls_39*39 + Cls_41*41 + Cls_43*43 + 
#             Cls_45*45 + Cls_47*47 + Cls_49*49 + Cls_gt50*50)/Tot_stems]

