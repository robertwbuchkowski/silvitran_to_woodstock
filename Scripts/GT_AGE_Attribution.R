#set working directory
tmp <- "D:/OneDrive - University of New Brunswick/R/GT_AGE_Attribution"
setwd(tmp)
start_time <- Sys.time()

#set R packages
library(data.table)
library(dplyr)
library(tidyr)
require(ggplot2)
library(ggthemes)
library(sf)#Load shapefiles faster than "rgdal"
library(gstat)
library(paletteer)
library(ggpubr)
library(MatchIt)
library(Hmisc)

#General Landbase code Block
#Open the most recent landbase file with EFI and THEMES
WK_LAND <- read_sf("GT_WK_ready_052022/WK_LANDBASE_052022.shp")
WK_LAND <- WK_LAND[,-c(1)]
WK_LAND <- as.data.table(WK_LAND)
#Addin new ID column so each polygon have a unique ID
WK_LAND <- tibble::rowid_to_column(WK_LAND, "ID")

#correlation plot between volume and density from EFI
r <- round(cor(WK_LAND$GMV9, WK_LAND$TPH9), 2)
p <- cor.test(WK_LAND$GMV9, WK_LAND$TPH9)$p.value
ggplot(WK_LAND, aes(y=GMV9, x=TPH9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 

#Only keep matching variables to reduce file size
MT_LAND <- WK_LAND[,c("ID","GMV9","TPH9","AGE","Area_Ha","THEME2")]
MT_LAND$THEME2 <- as.factor(MT_LAND$THEME2)
#Loop subset each THEME2 factors into its own dataframe
list2env(split(MT_LAND, MT_LAND$THEME2), envir = .GlobalEnv)


#Age attribution for BFIR
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
BFIRy <- read.csv("Yield_Curves/BFIR/Y_NA_BFIR_Z_Z_Z_P1_BL.csv")
BFIRy = dplyr::rename(BFIRy, Age = 1)
BFIRy$Age <- as.integer(BFIRy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(BFIRy$VOLtot, BFIRy$DTY9), 2)
p <- cor.test(BFIRy$VOLtot, BFIRy$DTY9)$p.value
ggplot(BFIRy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR BFIR### based on  VOLUME and DENSITY
#Variables to link in both dataframe should have the same names
BFIR = dplyr::rename(BFIR, VOLtot = GMV9)
BFIR = dplyr::rename(BFIR, DTY9 = TPH9)
#Keeping only the link variables
BFIR <- BFIR[,c("ID", "VOLtot", "DTY9")]
BFIRy <- BFIRy[,c("Age", "VOLtot", "DTY9")]
#Convert data to long format
BFIRy_M <- reshape2::melt(BFIRy, id.vars="Age")
BFIR_M <- reshape2::melt(BFIR, id.vars="ID")
BFIRy_M <- as.data.table(BFIRy_M)
BFIR_M <- as.data.table(BFIR_M)
#Get the nearest age for each: TOTAL VOLUME and DENSITY >9cm dbh
BFIRa <- BFIRy_M[ BFIR_M , on=c("variable","value"), roll="nearest" ]
#Get the mean age based on totvol and density of ycurves by ID
BFIRa <- BFIRa[ , .(yAge = mean(Age)), by = ID]


#Age attribution for POHW
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
POHWy <- read.csv("Yield_Curves/POHW/Y_NA_POHW_Z_Z_Z_P1_BL.csv")
POHWy = dplyr::rename(POHWy, Age = 1)
POHWy$Age <- as.integer(POHWy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(POHWy$VOLtot, POHWy$DTY9), 2)
p <- cor.test(POHWy$VOLtot, POHWy$DTY9)$p.value
ggplot(POHWy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR POHW### based on  VOLUME and DENSITY
POHW = dplyr::rename(POHW, VOLtot = GMV9)
POHW = dplyr::rename(POHW, DTY9 = TPH9)
POHW <- POHW[,c("ID", "VOLtot", "DTY9")]
POHWy <- POHWy[,c("Age", "VOLtot", "DTY9")]
POHWy_M <- reshape2::melt(POHWy, id.vars="Age")
POHW_M <- reshape2::melt(POHW, id.vars="ID")
POHWy_M <- as.data.table(POHWy_M)
POHW_M <- as.data.table(POHW_M)
POHWa <- POHWy_M[ POHW_M , on=c("variable","value"), roll="nearest" ]
POHWa <- POHWa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for MXSP
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
MXSPy <- read.csv("Yield_Curves/MXSP/Y_NA_MXSP_Z_Z_Z_P1_BL.csv")
MXSPy = dplyr::rename(MXSPy, Age = 1)
MXSPy$Age <- as.integer(MXSPy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(MXSPy$VOLtot, MXSPy$DTY9), 2)
p <- cor.test(MXSPy$VOLtot, MXSPy$DTY9)$p.value
ggplot(MXSPy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR MXSP### based on  VOLUME and DENSITY
MXSP = dplyr::rename(MXSP, VOLtot = GMV9)
MXSP = dplyr::rename(MXSP, DTY9 = TPH9)
MXSP <- MXSP[,c("ID", "VOLtot", "DTY9")]
MXSPy <- MXSPy[,c("Age", "VOLtot", "DTY9")]
MXSPy_M <- reshape2::melt(MXSPy, id.vars="Age")
MXSP_M <- reshape2::melt(MXSP, id.vars="ID")
MXSPy_M <- as.data.table(MXSPy_M)
MXSP_M <- as.data.table(MXSP_M)
MXSPa <- MXSPy_M[ MXSP_M , on=c("variable","value"), roll="nearest" ]
MXSPa <- MXSPa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for BIHW
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
BIHWy <- read.csv("Yield_Curves/BIHW/Y_NA_BIHW_Z_Z_Z_P1_BL.csv")
BIHWy = dplyr::rename(BIHWy, Age = 1)
BIHWy$Age <- as.integer(BIHWy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(BIHWy$VOLtot, BIHWy$DTY9), 2)
p <- cor.test(BIHWy$VOLtot, BIHWy$DTY9)$p.value
ggplot(BIHWy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR BIHW### based on  VOLUME and DENSITY
BIHW = dplyr::rename(BIHW, VOLtot = GMV9)
BIHW = dplyr::rename(BIHW, DTY9 = TPH9)
BIHW <- BIHW[,c("ID", "VOLtot", "DTY9")]
BIHWy <- BIHWy[,c("Age", "VOLtot", "DTY9")]
BIHWy_M <- reshape2::melt(BIHWy, id.vars="Age")
BIHW_M <- reshape2::melt(BIHW, id.vars="ID")
BIHWy_M <- as.data.table(BIHWy_M)
BIHW_M <- as.data.table(BIHW_M)
BIHWa <- BIHWy_M[ BIHW_M , on=c("variable","value"), roll="nearest" ]
BIHWa <- BIHWa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for BSPR
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
BSPRy <- read.csv("Yield_Curves/BSPR/Y_NA_BSPR_Z_Z_Z_P1_BL.csv")
BSPRy = dplyr::rename(BSPRy, Age = 1)
BSPRy$Age <- as.integer(BSPRy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(BSPRy$VOLtot, BSPRy$DTY9), 2)
p <- cor.test(BSPRy$VOLtot, BSPRy$DTY9)$p.value
ggplot(BSPRy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR BSPR### based on  VOLUME and DENSITY
BSPR = dplyr::rename(BSPR, VOLtot = GMV9)
BSPR = dplyr::rename(BSPR, DTY9 = TPH9)
BSPR <- BSPR[,c("ID", "VOLtot", "DTY9")]
BSPRy <- BSPRy[,c("Age", "VOLtot", "DTY9")]
BSPRy_M <- reshape2::melt(BSPRy, id.vars="Age")
BSPR_M <- reshape2::melt(BSPR, id.vars="ID")
BSPRy_M <- as.data.table(BSPRy_M)
BSPR_M <- as.data.table(BSPR_M)
BSPRa <- BSPRy_M[ BSPR_M , on=c("variable","value"), roll="nearest" ]
BSPRa <- BSPRa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for SPMX
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
SPMXy <- read.csv("Yield_Curves/SPMX/Y_NA_SPMX_Z_Z_Z_P1_BL.csv")
SPMXy = dplyr::rename(SPMXy, Age = 1)
SPMXy$Age <- as.integer(SPMXy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(SPMXy$VOLtot, SPMXy$DTY9), 2)
p <- cor.test(SPMXy$VOLtot, SPMXy$DTY9)$p.value
ggplot(SPMXy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR SPMX### based on  VOLUME and DENSITY
SPMX = dplyr::rename(SPMX, VOLtot = GMV9)
SPMX = dplyr::rename(SPMX, DTY9 = TPH9)
SPMX <- SPMX[,c("ID", "VOLtot", "DTY9")]
SPMXy <- SPMXy[,c("Age", "VOLtot", "DTY9")]
SPMXy_M <- reshape2::melt(SPMXy, id.vars="Age")
SPMX_M <- reshape2::melt(SPMX, id.vars="ID")
SPMXy_M <- as.data.table(SPMXy_M)
SPMX_M <- as.data.table(SPMX_M)
SPMXa <- SPMXy_M[ SPMX_M , on=c("variable","value"), roll="nearest" ]
SPMXa <- SPMXa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for BFMX
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
BFMXy <- read.csv("Yield_Curves/BFMX/Y_NA_BFMX_Z_Z_Z_P1_BL.csv")
BFMXy = dplyr::rename(BFMXy, Age = 1)
BFMXy$Age <- as.integer(BFMXy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(BFMXy$VOLtot, BFMXy$DTY9), 2)
p <- cor.test(BFMXy$VOLtot, BFMXy$DTY9)$p.value
ggplot(BFMXy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR BFMX### based on  VOLUME and DENSITY
BFMX = dplyr::rename(BFMX, VOLtot = GMV9)
BFMX = dplyr::rename(BFMX, DTY9 = TPH9)
BFMX <- BFMX[,c("ID", "VOLtot", "DTY9")]
BFMXy <- BFMXy[,c("Age", "VOLtot", "DTY9")]
BFMXy_M <- reshape2::melt(BFMXy, id.vars="Age")
BFMX_M <- reshape2::melt(BFMX, id.vars="ID")
BFMXy_M <- as.data.table(BFMXy_M)
BFMX_M <- as.data.table(BFMX_M)
BFMXa <- BFMXy_M[ BFMX_M , on=c("variable","value"), roll="nearest" ]
BFMXa <- BFMXa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for BIMX
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
BIMXy <- read.csv("Yield_Curves/BIMX/Y_NA_BIMX_Z_Z_Z_P1_BL.csv")
BIMXy = dplyr::rename(BIMXy, Age = 1)
BIMXy$Age <- as.integer(BIMXy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(BIMXy$VOLtot, BIMXy$DTY9), 2)
p <- cor.test(BIMXy$VOLtot, BIMXy$DTY9)$p.value
ggplot(BIMXy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR BIMX### based on  VOLUME and DENSITY
BIMX = dplyr::rename(BIMX, VOLtot = GMV9)
BIMX = dplyr::rename(BIMX, DTY9 = TPH9)
BIMX <- BIMX[,c("ID", "VOLtot", "DTY9")]
BIMXy <- BIMXy[,c("Age", "VOLtot", "DTY9")]
BIMXy_M <- reshape2::melt(BIMXy, id.vars="Age")
BIMX_M <- reshape2::melt(BIMX, id.vars="ID")
BIMXy_M <- as.data.table(BIMXy_M)
BIMX_M <- as.data.table(BIMX_M)
BIMXa <- BIMXy_M[ BIMX_M , on=c("variable","value"), roll="nearest" ]
BIMXa <- BIMXa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for WPSW
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
WPSWy <- read.csv("Yield_Curves/WPSW/Y_NA_WPSW_Z_Z_Z_P1_BL.csv")
WPSWy = dplyr::rename(WPSWy, Age = 1)
WPSWy$Age <- as.integer(WPSWy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(WPSWy$VOLtot, WPSWy$DTY9), 2)
p <- cor.test(WPSWy$VOLtot, WPSWy$DTY9)$p.value
ggplot(WPSWy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR WPSW### based on  VOLUME and DENSITY
WPSW = dplyr::rename(WPSW, VOLtot = GMV9)
WPSW = dplyr::rename(WPSW, DTY9 = TPH9)
WPSW <- WPSW[,c("ID", "VOLtot", "DTY9")]
WPSWy <- WPSWy[,c("Age", "VOLtot", "DTY9")]
WPSWy_M <- reshape2::melt(WPSWy, id.vars="Age")
WPSW_M <- reshape2::melt(WPSW, id.vars="ID")
WPSWy_M <- as.data.table(WPSWy_M)
WPSW_M <- as.data.table(WPSW_M)
WPSWa <- WPSWy_M[ WPSW_M , on=c("variable","value"), roll="nearest" ]
WPSWa <- WPSWa[ , .(yAge = mean(Age)), by = ID]

#Age attribution for TOHW
#Open yield curves from baseline ONLY Age starting 0-25 for each stand types
TOHWy <- read.csv("Yield_Curves/TOHW/Y_NA_TOHW_Z_Z_Z_P1_BL.csv")
TOHWy = dplyr::rename(TOHWy, Age = 1)
TOHWy$Age <- as.integer(TOHWy$Age)
#correlation plot between volume and density from yield curves
r <- round(cor(TOHWy$VOLtot, TOHWy$DTY9), 2)
p <- cor.test(TOHWy$VOLtot, TOHWy$DTY9)$p.value
ggplot(TOHWy, aes(y=VOLtot, x=DTY9)) + 
  geom_point() + 
  geom_smooth(method="lm", col="Red") + 
  annotate("text", x=20, y=250, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=20, y=235, label=paste0("p = ", round(p, 3)), hjust=0) +
  theme_classic() 
###CUMPUTING AGE FOR TOHW### based on  VOLUME and DENSITY
TOHW = dplyr::rename(TOHW, VOLtot = GMV9)
TOHW = dplyr::rename(TOHW, DTY9 = TPH9)
TOHW <- TOHW[,c("ID", "VOLtot", "DTY9")]
TOHWy <- TOHWy[,c("Age", "VOLtot", "DTY9")]
TOHWy_M <- reshape2::melt(TOHWy, id.vars="Age")
TOHW_M <- reshape2::melt(TOHW, id.vars="ID")
TOHWy_M <- as.data.table(TOHWy_M)
TOHW_M <- as.data.table(TOHW_M)
TOHWa <- TOHWy_M[ TOHW_M , on=c("variable","value"), roll="nearest" ]
TOHWa <- TOHWa[ , .(yAge = mean(Age)), by = ID]





#Binding all age with their ID
Rbinding <- bind_rows(BFIRa, BIHWa, MXSPa, POHWa, SPMXa, BFMXa, BIMXa, BSPRa, WPSWa, TOHWa)
#merge yAge to original Woodstock landbase
WK_LAND <- merge(Rbinding, WK_LAND, by = "ID", all.y = TRUE)

#Conserving the age of stands that have been harvested in the past
WK_LAND <- as.data.table(WK_LAND)
#Age 5 was attributed to "unknown" so everything above 5 is the real age
WK_LAND <- WK_LAND[AGE > 5,yAge:= AGE]

WK_LAND$LIDARage = WK_LAND$yAge

#Building Theme5
WK_LAND <- WK_LAND[WK_LAND$yAge >= 0 & WK_LAND$yAge <= 25, THEME5 := 0 ]
WK_LAND <- WK_LAND[WK_LAND$yAge > 25 & WK_LAND$yAge <= 50, THEME5 := 25 ]
WK_LAND <- WK_LAND[WK_LAND$yAge > 50 & WK_LAND$yAge <= 75, THEME5 := 50 ]
WK_LAND <- WK_LAND[WK_LAND$yAge > 75, THEME5 := 75 ]

WK_LAND <- WK_LAND[WK_LAND$yAge >= 0 & WK_LAND$yAge <= 25, yAge := 5 ]
WK_LAND <- WK_LAND[WK_LAND$yAge > 25 & WK_LAND$yAge <= 50, yAge := 25 ]
WK_LAND <- WK_LAND[WK_LAND$yAge > 50 & WK_LAND$yAge <= 75, yAge := 50 ]
WK_LAND <- WK_LAND[WK_LAND$yAge > 75, yAge := 75 ]
WK_LAND$yAge[is.na(WK_LAND$yAge)] <- 5

#Building Theme6
WK_LAND <- WK_LAND[ , THEME6 := "P1" ]

#Building Theme7
WK_LAND <- WK_LAND[ , THEME7 := "BL" ]

#Back to SF to go in Woodstock
WK_LAND <- st_as_sf(WK_LAND)
st_write(WK_LAND, "WK_TH_Patial_BL", driver="ESRI Shapefile", delete_layer = TRUE)


