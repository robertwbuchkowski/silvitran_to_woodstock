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
library(gdata)

Var.names <- read.csv("Variable_names.csv")
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

#Fake matrix to calculate height of every class at every ages. Need to be same size as df
Height <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50,
            1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,50)
Height <- matrix(Height,nrow = 21, ncol = 26, byrow = TRUE)

DBH <- c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145,
         1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,145)
DBH <- matrix(DBH,nrow = 21, ncol = 21, byrow = TRUE)


#height integration to baseline yields
XXXX_hig00 <- as.data.table(XXXX_hig00)
XXXX_hig00 <- XXXX_hig00[,-c("Species")]

#number of stem per age class and height class all species combined
XXXX_hig00 <- aggregate(XXXX_hig00, by=list(Age=XXXX_hig00$Year), FUN=sum)
XXXX_hig00 <- as.data.table(XXXX_hig00)
XXXX_hig00 <- XXXX_hig00[,-c("Year")]
XXXX_hig00 <- XXXX_hig00[,-c("Age")]

#calculate % (in decimals) of the number of trees per height class
XXXX_hig00$Tot <- rowSums( XXXX_hig00[,1:26] )
XXXX_hig00 <- as.data.table(XXXX_hig00)
cols <- c(1:26)
XXXX_hig00[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
XXXX_hig00 <- XXXX_hig00[,-c("Tot")]

#cumulative sum over the df
XXXX_hig00 = t(apply(XXXX_hig00, 1, cumsum))

#select the 90th percentile and attribute value 1
XXXX_hig00[XXXX_hig00 > 0.899 & XXXX_hig00 < 1]<-1
#values outside 90th percentile identify as 999 so we know lather that they are outside the range
XXXX_hig00[XXXX_hig00 < 0.899]<-999
XXXX_hig00 <- as.matrix(XXXX_hig00)

#Mutiply fake matrix with original df
XXXX_hig00 = XXXX_hig00 * Height
XXXX_hig00 <- as.data.frame(XXXX_hig00)
#Min values correspond to 90 percentile of height and higher
XXXX_hig00$Height<-apply(XXXX_hig00,1,FUN=min)
#final height dataframe representing average top 90th percentile dbh of the stand over its development 
XXXX_hig00 <- XXXX_hig00[,c("Height")]
XXXX_hig00 <- as.data.frame(XXXX_hig00)


#DBH integration to yields
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00 <- XXXX_dbh00[,-c("Species")]
#number of stem per age class and dbh class all species combined
XXXX_dbh00 <- aggregate(XXXX_dbh00, by=list(Age=XXXX_dbh00$Year), FUN=sum)
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
XXXX_dbh00 <- XXXX_dbh00[,-c("Year")]
XXXX_dbh00 <- XXXX_dbh00[,-c("Age")]

#calculate % (in decimals) of the number of trees per DBH class
XXXX_dbh00$Tot <- rowSums(XXXX_dbh00[,1:21] )
XXXX_dbh00 <- as.data.table(XXXX_dbh00)
cols <- c(1:21)
XXXX_dbh00[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
XXXX_dbh00 <- XXXX_dbh00[,-c("Tot")]

#cumulative sum over the df
XXXX_dbh00 = t(apply(XXXX_dbh00, 1, cumsum))

#select the 90th percentile and attribute value 1
XXXX_dbh00[XXXX_dbh00 > 0.899 & XXXX_dbh00 < 1]<-1
#values outside 90th percentile identify as 999 so we know lather that they are outside the range
XXXX_dbh00[XXXX_dbh00 < 0.899]<-999
XXXX_dbh00 <- as.matrix(XXXX_dbh00)

#Mutiply fake matrix with original df
XXXX_dbh00 = XXXX_dbh00 * DBH
XXXX_dbh00 <- as.data.frame(XXXX_dbh00)
#Min values correspond to 90 percentile of DBH and higher
XXXX_dbh00$DBH<-apply(XXXX_dbh00,1,FUN=min)
#final DBH dataframe representing average top 90th percentile DBH of the stand over its development 
XXXX_dbh00 <- XXXX_dbh00[,c("DBH")]
XXXX_dbh00 <- as.data.frame(XXXX_dbh00)

#Adding a variable for total volume
XXXX_vol00$VOLtot <- rowSums(XXXX_vol00[,2:ncol(XXXX_vol00)])

#Combining and renaming variables in one single yield table
BFIR_base <- cbind(XXXX_vol00, XXXX_hig00, XXXX_dbh00)

#Renaming column in bulk, add new names to .csv if not in already present
existing <- match(Var.names$ï..Names,names(BFIR_base))
names(BFIR_base)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
colnames(BFIR_base)[colnames(BFIR_base) == 'Age'] <- '_Age'
#1 decimal places in yield
is.num <- sapply(BFIR_base, is.numeric)
BFIR_base[is.num] <- lapply(BFIR_base[is.num], round, 3)
colnames(BFIR_base)[colnames(BFIR_base) == 'Age'] <- '_Age'

txt <- "*Y NA BFIR ? ? ? base"
tmp <- "BFIR/BFIR_base.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_base, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "    ")




#height integration to RCP 4.5 yields
XXXX_hig45 <- as.data.table(XXXX_hig45)
XXXX_hig45 <- XXXX_hig45[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig45 <- aggregate(XXXX_hig45, by=list(Age=XXXX_hig45$Year), FUN=sum)
XXXX_hig45 <- as.data.table(XXXX_hig45)
XXXX_hig45 <- XXXX_hig45[,-c("Year")]
XXXX_hig45 <- XXXX_hig45[,-c("Age")]
#calculate % (in decimals) of the number of trees per height class
XXXX_hig45$Tot <- rowSums( XXXX_hig45[,1:26] )
XXXX_hig45 <- as.data.table(XXXX_hig45)
cols <- c(1:26)
XXXX_hig45[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
XXXX_hig45 <- XXXX_hig45[,-c("Tot")]
#cumulative sum over the df
XXXX_hig45 = t(apply(XXXX_hig45, 1, cumsum))
#select the 90th percentile and attribute value 1
XXXX_hig45[XXXX_hig45 > 0.899 & XXXX_hig45 < 1]<-1
#values outside 90th percentile identify as 999 so we know lather that they are outside the range
XXXX_hig45[XXXX_hig45 < 0.899]<-999
XXXX_hig45 <- as.matrix(XXXX_hig45)
#Mutiply fake matrix with original df
XXXX_hig45 = XXXX_hig45 * Height
XXXX_hig45 <- as.data.frame(XXXX_hig45)
XXXX_hig45$Height<-apply(XXXX_hig45,1,FUN=min)
XXXX_hig45 <- XXXX_hig45[,c("Height")]
XXXX_hig45 <- as.data.frame(XXXX_hig45)
XXXX_hig45 = XXXX_hig45 * Height
XXXX_hig45 <- as.data.frame(XXXX_hig45)
XXXX_hig45$Height<-apply(XXXX_hig45,1,FUN=min)
XXXX_hig45 <- XXXX_hig45[,c("Height")]
XXXX_hig45 <- as.data.frame(XXXX_hig45)

#DBH integration to yields
XXXX_dbh45 <- as.data.table(XXXX_dbh45)
XXXX_dbh45 <- XXXX_dbh45[,-c("Species")]
#number of stem per age class and dbh class all species combined
XXXX_dbh45 <- aggregate(XXXX_dbh45, by=list(Age=XXXX_dbh45$Year), FUN=sum)
XXXX_dbh45 <- as.data.table(XXXX_dbh45)
XXXX_dbh45 <- XXXX_dbh45[,-c("Year")]
XXXX_dbh45 <- XXXX_dbh45[,-c("Age")]
#calculate % (in decimals) of the number of trees per DBH class
XXXX_dbh45$Tot <- rowSums(XXXX_dbh45[,1:21] )
XXXX_dbh45 <- as.data.table(XXXX_dbh45)
cols <- c(1:21)
XXXX_dbh45[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
XXXX_dbh45 <- XXXX_dbh45[,-c("Tot")]
XXXX_dbh45 = t(apply(XXXX_dbh45, 1, cumsum))
#select the 90th percentile and attribute value 1
XXXX_dbh45[XXXX_dbh45 > 0.899 & XXXX_dbh45 < 1]<-1
#values outside 90th percentile identify as 999 so we know lather that they are outside the range
XXXX_dbh45[XXXX_dbh45 < 0.899]<-999
XXXX_dbh45 <- as.matrix(XXXX_dbh45)
#Multiply fake matrix with original df
XXXX_dbh45 = XXXX_dbh45 * DBH
XXXX_dbh45 <- as.data.frame(XXXX_dbh45)
XXXX_dbh45$DBH<-apply(XXXX_dbh45,1,FUN=min)
XXXX_dbh45 <- XXXX_dbh45[,c("DBH")]
XXXX_dbh45 <- as.data.frame(XXXX_dbh45)

#Calculating new variable for total volume
XXXX_vol45$VOLtot <- rowSums(XXXX_vol45[,2:ncol(XXXX_vol45)])
BFIR_rcp45 <- cbind(XXXX_vol45, XXXX_hig45, XXXX_dbh45)
existing <- match(Var.names$ï..Names,names(BFIR_rcp45))
names(BFIR_rcp45)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
colnames(BFIR_rcp45)[colnames(BFIR_rcp45) == 'Age'] <- '_Age'
#1 decimal places in yield
is.num <- sapply(BFIR_rcp45, is.numeric)
BFIR_rcp45[is.num] <- lapply(BFIR_rcp45[is.num], round, 3)
colnames(BFIR_rcp45)[colnames(BFIR_rcp45) == 'Age'] <- '_Age'

txt <- "*Y NA BFIR ? ? ? rcp45"
tmp <- "BFIR/BFIR_rcp45.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp45, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "    ")



#height integration to RCP 8.5 yields
XXXX_hig85 <- as.data.table(XXXX_hig85)
XXXX_hig85 <- XXXX_hig85[,-c("Species")]
#number of stem per age class and height class all species combined
XXXX_hig85 <- aggregate(XXXX_hig85, by=list(Age=XXXX_hig85$Year), FUN=sum)
XXXX_hig85 <- as.data.table(XXXX_hig85)
XXXX_hig85 <- XXXX_hig85[,-c("Year")]
XXXX_hig85 <- XXXX_hig85[,-c("Age")]
#calculate % (in decimals) of the number of trees per height class
XXXX_hig85$Tot <- rowSums( XXXX_hig85[,1:26] )
XXXX_hig85 <- as.data.table(XXXX_hig85)
cols <- c(1:26)
XXXX_hig85[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
XXXX_hig85 <- XXXX_hig85[,-c("Tot")]
#cumulative sum over the df
XXXX_hig85 = t(apply(XXXX_hig85, 1, cumsum))
#select the 90th percentile and attribute value 1
XXXX_hig85[XXXX_hig85 > 0.899 & XXXX_hig85 < 1]<-1
#values outside 90th percentile identify as 999 so we know lather that they are outside the range
XXXX_hig85[XXXX_hig85 < 0.899]<-999
XXXX_hig85 <- as.matrix(XXXX_hig85)
#Mutiply fake matrix with original df
XXXX_hig85 = XXXX_hig85 * Height
XXXX_hig85 <- as.data.frame(XXXX_hig85)
XXXX_hig85$Height<-apply(XXXX_hig85,1,FUN=min)
XXXX_hig85 <- XXXX_hig85[,c("Height")]
XXXX_hig85 <- as.data.frame(XXXX_hig85)
XXXX_hig85 = XXXX_hig85 * Height
XXXX_hig85 <- as.data.frame(XXXX_hig85)
XXXX_hig85$Height<-apply(XXXX_hig85,1,FUN=min)
XXXX_hig85 <- XXXX_hig85[,c("Height")]
XXXX_hig85 <- as.data.frame(XXXX_hig85)

#DBH integration to yields
XXXX_dbh85 <- as.data.table(XXXX_dbh85)
XXXX_dbh85 <- XXXX_dbh85[,-c("Species")]
#number of stem per age class and dbh class all species combined
XXXX_dbh85 <- aggregate(XXXX_dbh85, by=list(Age=XXXX_dbh85$Year), FUN=sum)
XXXX_dbh85 <- as.data.table(XXXX_dbh85)
XXXX_dbh85 <- XXXX_dbh85[,-c("Year")]
XXXX_dbh85 <- XXXX_dbh85[,-c("Age")]
#calculate % (in decimals) of the number of trees per DBH class
XXXX_dbh85$Tot <- rowSums(XXXX_dbh85[,1:21] )
XXXX_dbh85 <- as.data.table(XXXX_dbh85)
cols <- c(1:21)
XXXX_dbh85[ , (cols) := lapply(.SD, "/", Tot), .SDcols = cols]
XXXX_dbh85 <- XXXX_dbh85[,-c("Tot")]
XXXX_dbh85 = t(apply(XXXX_dbh85, 1, cumsum))
#select the 90th percentile and attribute value 1
XXXX_dbh85[XXXX_dbh85 > 0.899 & XXXX_dbh85 < 1]<-1
#values outside 90th percentile identify as 999 so we know lather that they are outside the range
XXXX_dbh85[XXXX_dbh85 < 0.899]<-999
XXXX_dbh85 <- as.matrix(XXXX_dbh85)
#Multiply fake matrix with original df
XXXX_dbh85 = XXXX_dbh85 * DBH
XXXX_dbh85 <- as.data.frame(XXXX_dbh85)
XXXX_dbh85$DBH<-apply(XXXX_dbh85,1,FUN=min)
XXXX_dbh85 <- XXXX_dbh85[,c("DBH")]
XXXX_dbh85 <- as.data.frame(XXXX_dbh85)
#Calculating new variable for total volume
XXXX_vol85$VOLtot <- rowSums(XXXX_vol85[,2:ncol(XXXX_vol85)])
BFIR_rcp85 <- cbind(XXXX_vol85, XXXX_hig85, XXXX_dbh85)
existing <- match(Var.names$ï..Names,names(BFIR_rcp85))
names(BFIR_rcp85)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]
#1 decimal places in yield
is.num <- sapply(BFIR_rcp85, is.numeric)
BFIR_rcp85[is.num] <- lapply(BFIR_rcp85[is.num], round, 3)
colnames(BFIR_rcp85)[colnames(BFIR_rcp85) == 'Age'] <- '_Age'

txt <- "*Y NA BFIR ? ? ? rcp85"
tmp <- "BFIR/BFIR_rcp85.txt"
cat(txt, "\n", file = tmp)
write.fwf(BFIR_rcp85, file = tmp, append = TRUE, rownames = FALSE, quote = FALSE, sep = "    " )




#For data visualization 
#BFIR_base.v <- subset(BFIR_base, select = -c(VOLtot, HGT3cls, DBH5cls))
#BFIR_base.vm <- melt(BFIR_base.v ,  id.vars = 'Age', variable.name = 'series')
#ggplot(BFIR_base.vm, aes(Age, value, fill = series))+
#  geom_area()


