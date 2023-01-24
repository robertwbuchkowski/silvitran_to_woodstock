#set R packages
library(data.table)
library(spatialEco)
library(dplyr)
library(tidyr)
require(ggplot2)
library(ggthemes)
library(rgdal)
library(gdata)
library(reshape2)
library(DescTools)
#Baseline because they have different file names for some reason
#Enter VOLUME file to use
#dbh.file <- read.csv("TOHW/TOHW_Baseline_20002300_DBH.csv")
#Enter tree HIGHT file to use
#height.file <- read.csv("TOHW/TOHW_Baseline_20002300_height.csv")
#Enter tree DBH file to use
#volume.file <- read.csv("TOHW/TOHW_Baseline_20002300_volume.csv")

#Enter VOLUME file to use
dbh.file <- read.csv("TOHW/TOHW_RCP85_T3_Age0-25years_DBH.csv")
#Enter tree HIGHT file to use
height.file <- read.csv("TOHW/TOHW_RCP85_T3_Age0-25years_height.csv")
#Enter tree DBH file to use
volume.file <- read.csv("TOHW/TOHW_RCP85_T3_Age0-25years_volume.csv")

#DEFINE VALUES FOR EACH THEMES
##Z is used to replace ? because ? is not accepted in windows file names##
#THEME1
TH1 <- "NA"
#THEME2
TH2 <- "TOHW"
#THEME3
TH3 <- "Z"
#THEME4
TH4 <- "Z"
#THEME5
TH5 <- "X"
#THEME6
TH6 <- "P3"
#THEME7
TH7 <- "85"

#For file name
Y <- "*Y"

#Enter percentile to use to define mean top Height and mean DBH
Percentile <- 0.90

#Enter Number of decimal points to use in yield tables
dec <- 3

#Forcing R not to use scientific notation
options(scipen = 999)

#Rounding inital tables to no decimals
dbh.file <- dbh.file %>% mutate(across(where(is.numeric), round, 0))
#Rounding inital tables to no decimals
height.file <- height.file %>% mutate(across(where(is.numeric), round, 0))



#Open variable names file to attribute to full lenght names
Var.names <- read.csv("Variable_names.csv")
Var.names = dplyr::rename(Var.names, Names = ï..Names)

#Get ratio for each products from database
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


#MEAN_HEIGHT
#HEIGHT integration to compatible table with WK
height.file <- as.data.table(height.file)
height.file <- height.file[,-c("Species")]
#number of stem per age class and height class all species combined
height.file <- aggregate(height.file, by=list(Age=height.file$Year), FUN=sum)
height.file <- as.data.table(height.file)
height.file <- height.file[,-c("Year")]
require(reshape2)
height.file <- setDT(height.file)
height.file <- melt(height.file, id.vars =  c("Age"))
height.file$Age <- as.factor(height.file$Age)
height.file$value <- as.integer(height.file$value)
height.file$variable <- paste(height.file$Age,"_", height.file$variable)
height.file <- height.file[,-c(1)]
height.file <- uncount(height.file, value, .id = "Tree")
height.file <- data.frame(do.call("rbind", strsplit(as.character(height.file$variable), "_", fixed = TRUE)))
height.file = dplyr::rename(height.file, Age = X1)
height.file = dplyr::rename(height.file, Height = X3)
height.file <- height.file[,-c(2)]
height.file <- as.data.table(height.file)
height.file$Height <- as.integer(height.file$Height)
height.file$Age <- as.integer(height.file$Age)
height.d <- height.file
#To get middle of height class
height.file[ , Height:=Height + 1]
#Defines what is the 90 percentile values and average the top 10% of the dataframe
height.file <- height.file %>%
  group_by(Age) %>%
  summarise(HGT90 = mean(Height[Height>=quantile(Height, Percentile)]))


#MEAN_DIAMETER and DENSITY RCP45
#DBH and DENSITY integration to compatible table with WK
dbh.file <- as.data.table(dbh.file)
dbh.file <- dbh.file[,-c("Species")]
#number of stem per age class and DBH class all species combined
dbh.file <- aggregate(dbh.file, by=list(Age=dbh.file$Year), FUN=sum)
dbh.file <- as.data.table(dbh.file)
dbh.file <- dbh.file[,-c("Year")]
#adding density
density.data <- dbh.file[, DTYtot := rowSums(.SD), .SDcols = 2:22][]
density.data <- dbh.file[, DTY9 := rowSums(.SD), .SDcols = 4:22][]
dbh.file <- dbh.file[,-c("DTYtot", "DTY9")]
density.data <- density.data[,c("Age", "DTYtot", "DTY9")]
density.data[,c(2:3) := round(.SD,0), .SDcols=c(2:3)]
require(reshape2)
dbh.file <- setDT(dbh.file)
dbh.file <- melt(dbh.file, id.vars =  c("Age"))
dbh.file$Age <- as.factor(dbh.file$Age)
dbh.file$value <- as.integer(dbh.file$value)
dbh.file$variable <- paste(dbh.file$Age,"_", dbh.file$variable)
dbh.file <- dbh.file[,-c(1)]
dbh.file <- uncount(dbh.file, value, .id = "Tree")
dbh.file <- data.frame(do.call("rbind", strsplit(as.character(dbh.file$variable), "_", fixed = TRUE)))
dbh.file = dplyr::rename(dbh.file, Age = X1)
dbh.file = dplyr::rename(dbh.file, DBH = X3)
dbh.file <- dbh.file[,-c(2)]
dbh.file <- as.data.table(dbh.file)
dbh.file$DBH <- as.integer(dbh.file$DBH)
dbh.file$Age <- as.integer(dbh.file$Age)
#To get middle of DBH class
dbh.file[ , DBH:=DBH + 2.5]
dbh.h <- dbh.file
#merchantable DBH
dbhmerch.file <- dbh.file %>%
  group_by(Age) %>%
  summarise(DBHmerch = mean(DBH[DBH>=9.1]))
#replacing NaN (where there is no merchantable volume) by 0 even if it doesn't make sense for Woodstock to recognize
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
dbhmerch.file[is.nan(dbhmerch.file)] <- 0
#Defines what is the 90 percentile values and average the top 10% of the dataframe
dbhpercent.file <- dbh.file %>%
  group_by(Age) %>%
  summarise(DBH90 = mean(DBH[DBH>=quantile(DBH, Percentile)]))

#Average height of merchantable trees
height.dbh <- cbind(dbh.h, height.d)
height.dbh <- height.dbh[,-c(3)]
height.dbh <- height.dbh %>%
  group_by(Age) %>%
  summarise(HGTmerch = mean(Height[DBH>=9.1]))
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
height.dbh[is.nan(height.dbh)] <- 0

#VOLUME
#Volume integration to compatible table with WK
volume.file$VOLtot <- rowSums(volume.file[,2:ncol(volume.file)])
volume.file = dplyr::rename(volume.file, Age = Year)
yield.WK <- merge (volume.file, height.file, by = "Age")
yield.WK <- merge (yield.WK, height.dbh, by = "Age")
yield.WK <- merge (yield.WK, dbhpercent.file, by = "Age")
yield.WK <- merge (yield.WK, dbhmerch.file, by = "Age")
is.num <- sapply(yield.WK, is.numeric)
yield.WK[is.num] <- lapply(yield.WK[is.num], round, dec)
yield.WK <- merge (yield.WK, density.data, by = "Age")
existing <- match(Var.names$Names,names(yield.WK))
names(yield.WK)[na.omit(existing)] <- Var.names$Code[which(!is.na(existing))]

#Adding column to help with the merge of wood product ratios (Height are rounded to the closest multiple of 2)
#Products ratios based on 90th percentile:
#yield.WK$HGT90_2 <- RoundTo(yield.WK$HGT90, multiple = 2, FUN = round)
#yield.WK$DBH90_2 <- RoundTo(yield.WK$DBH90, multiple = 2, FUN = round)

#Products ratios based on merch dbh and height
yield.WK$HGT90_2 <- RoundTo(yield.WK$HGTmerch, multiple = 2, FUN = round)
yield.WK$DBH90_2 <- RoundTo(yield.WK$DBHmerch, multiple = 2, FUN = round)

#merging ratios for EASTERN CEDAR
yield.WK <- merge(yield.WK, EC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, ECl = Ratio)
yield.WK <- merge(yield.WK, EC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, ECs = Ratio)
yield.WK <- merge(yield.WK, EC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, ECp = Ratio)
yield.WK <- merge(yield.WK, EC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, ECb = Ratio)
#merging ratios for SPRUCE FIR
yield.WK <- merge(yield.WK, SF_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, SFl = Ratio)
yield.WK <- merge(yield.WK, SF_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, SFs = Ratio)
yield.WK <- merge(yield.WK, SF_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, SFp = Ratio)
yield.WK <- merge(yield.WK, SF_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, SFb = Ratio)
#merging ratios for WHITE PINE
yield.WK <- merge(yield.WK, WP_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, WPl = Ratio)
yield.WK <- merge(yield.WK, WP_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, WPs = Ratio)
yield.WK <- merge(yield.WK, WP_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, WPp = Ratio)
yield.WK <- merge(yield.WK, WP_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, WPb = Ratio)
#merging ratios for INTOLERANT HW
yield.WK <- merge(yield.WK, IH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, IHl = Ratio)
yield.WK <- merge(yield.WK, IH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, IHs = Ratio)
yield.WK <- merge(yield.WK, IH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, IHp = Ratio)
yield.WK <- merge(yield.WK, IH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, IHb = Ratio)
#merging ratios for TOLERANT HW
yield.WK <- merge(yield.WK, TH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, THl = Ratio)
yield.WK <- merge(yield.WK, TH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, THs = Ratio)
yield.WK <- merge(yield.WK, TH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, THp = Ratio)
yield.WK <- merge(yield.WK, TH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, THb = Ratio)
#merging ratios for conifers
yield.WK <- merge(yield.WK, OC_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OCl = Ratio)
yield.WK <- merge(yield.WK, OC_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OCs = Ratio)
yield.WK <- merge(yield.WK, OC_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OCp = Ratio)
yield.WK <- merge(yield.WK, OC_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OCb = Ratio)
#merging ratios for hardwoods
yield.WK <- merge(yield.WK, OH_log[ , c("HGT90_2", "DBH90_2", "Ratio")], by.x = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OHl = Ratio)
yield.WK <- merge(yield.WK, OH_stud[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OHs = Ratio)
yield.WK <- merge(yield.WK, OH_pulp[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OHp = Ratio)
yield.WK <- merge(yield.WK, OH_biomass[ , c("HGT90_2", "DBH90_2", "Ratio")], by = c("HGT90_2", "DBH90_2"))
yield.WK = dplyr::rename(yield.WK, OHb = Ratio)

#Rearrange the table
yield.WK <- yield.WK[,-c(1, 2)]
yield.WK <- yield.WK[order(yield.WK$Age),]
colnames(yield.WK)[colnames(yield.WK) == 'Age'] <- '_Age'

#Exporting yield curve
txt <- capture.output(cat(Y, " ", TH1, " ", TH2, " ", TH3, " ", TH4, " ", TH5, " ", TH6, " ", TH7, sep="")) 
tmp <- capture.output(cat(TH2, "/", "Y", "_", TH1, "_", TH2, "_", TH3, "_", TH4, "_", TH5, "_", TH6, "_", TH7, ".txt", sep=""))
cs <- capture.output(cat(TH2, "/", "Y", "_", TH1, "_", TH2, "_", TH3, "_", TH4, "_", TH5, "_", TH6, "_", TH7, ".csv", sep=""))
cat(txt, "\n", file = tmp)
write.fwf(yield.WK, file = tmp, append = TRUE, rownames = FALSE, colnames = TRUE, quote = FALSE, sep = "  ")
write.csv(yield.WK,cs, row.names = FALSE)
