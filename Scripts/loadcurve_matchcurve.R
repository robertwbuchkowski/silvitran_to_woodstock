# Code the load and match the Yield Curves to the current forest:

# Library
library(sf)
library(tidyverse)
library(parallel)

# Load in the yield curves:
LF = list.files("YieldCurves", recursive = T)

# Keep only the correct yield curve files:
LF = LF[grepl("Z_Z_Z_P1_BL.xlsx", LF)]

# Load in the yield curves:
yc = vector("list", length = length(LF))

for(i in 1:length(LF)){
  yc[[i]] = readxl::read_xlsx(paste0("YieldCurves/",LF[i])) %>%
    mutate(FUNA = str_split(LF[i], "/")[[1]][1]) %>%
    pivot_longer(!`_Age` & !FUNA)
}

yc = do.call("rbind", yc) %>%
  pivot_wider(values_fill = 0) # This is OK because we are filling in missing species with zero volume


# Calculate the locations on the yield curves where there are between 25% and 75% softwood trees:

if(F){
  yc %>%
    select(`_Age`, FUNA, contains("v") & !VOLtot) %>%
    pivot_longer(!`_Age`&!FUNA) %>%
    separate(name, into = c("name", NA), sep = -1) %>%
    left_join(
      read_csv("C:/Users/rober/Documents/GitHubRepos/occupancy_exploration/Data/SPcodeHWSW.csv") %>% select(-Name), by = c("name" = "SpeciesCode")) %>%
    group_by(`_Age`, FUNA, Type) %>%
    summarize(value = sum(value)) %>%
    mutate(Prop = replace_na(value/sum(value), 0)) %>%
    select(-value) %>%
    filter(Type == "SW" & Prop > 0.25 & Prop < 0.75) %>%
    select(-Type) %>% rename(`Proportion softwood by volume` = Prop) %>%
    write_csv("ResultsData/FUNA_prop_softwood.csv")
}

# Get the landscape data ----

PIobjects = read_rds("Data/LiDAR_extracts_PI.rds")

# Here we are using the original map, because the divided polygons are too small for accurate LiDAR data!
newPI = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp")

if(max(table(newPI$OBJECTID)) != 1) print("WARNING: Unique indentified is duplicated!") else print("YAY: Unique indentifier is unique!")

# Check the FUNA that Cedric calculated:
Cedric_FUNA = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromColleagues/Cedric_gagetown_landscape/WK_LANDBASE_052022.shp") %>% 
  st_drop_geometry() %>%
  select(OBJECTID, THEME2) %>%
  arrange(OBJECTID) %>%
  distinct()

Cedric_FUNA = Cedric_FUNA %>%
  group_by(OBJECTID) %>%
  slice_sample(n = 1) %>%
  ungroup()

#Keep only what we need for the match:
newPI = newPI %>%  
  st_drop_geometry() %>%
  select(OBJECTID, contains("L1S") & !L1SC, contains("L1PR"), contains("L2S"), contains("L2PR"))

# Add in L2 data if L1 is absent:
newPI = newPI %>%
  mutate(L1PR1 = ifelse(is.na(L1S1) & !is.na(L2S1), L2PR1, L1PR1),
         L1PR2 = ifelse(is.na(L1S2) & !is.na(L2S2), L2PR2, L1PR2),
         L1PR3 = ifelse(is.na(L1S3) & !is.na(L2S3), L2PR3, L1PR3),
         L1PR4 = ifelse(is.na(L1S4) & !is.na(L2S4), L2PR4, L1PR4),
         L1PR5 = ifelse(is.na(L1S5) & !is.na(L2S5), L2PR5, L1PR5),

         L1S1 = ifelse(is.na(L1S1) & !is.na(L2S1), L2S1, L1S1),
         L1S2 = ifelse(is.na(L1S2) & !is.na(L2S2), L2S2, L1S2),
         L1S3 = ifelse(is.na(L1S3) & !is.na(L2S3), L2S3, L1S3),
         L1S4 = ifelse(is.na(L1S4) & !is.na(L2S4), L2S4, L1S4),
         L1S5 = ifelse(is.na(L1S5) & !is.na(L2S5), L2S5, L1S5)) %>%
  select(OBJECTID, contains("L1S"), contains("L1PR"))


# Build the datasets for matching:

matching_data = vector("list", 4)
names(matching_data) = c("PIempirical", "PIspecies", "YCempirical", "YCspecies")


# Calculate the proportion of softwood by OBJECTID:
matching_data$PIempirical = newPI %>%
  select(OBJECTID, contains("L1S"))%>%
  pivot_longer(-OBJECTID, values_to = "Species") %>%
  separate(name, into = c(NA, "name"), sep = -1) %>%
  # Add in softwood and hardwood identifiers:
  left_join(
    read_csv("Data/SPcodeHWSW.csv") %>% select(-Name), by = c("Species" = "SpeciesCode")
  ) %>%
  select(-Species) %>%
  rename(Species = Type) %>%
  full_join(
    newPI %>%
      select(OBJECTID, contains("L1PR"))%>%
      pivot_longer(-OBJECTID) %>%
      separate(name, into = c(NA, "name"), sep = -1)%>%
      mutate(value = value/10) %>%
      group_by(OBJECTID) %>%
      mutate(Total = sum(value)) %>%
      mutate(value = value/Total) %>% # Rescale so it sums to 1
      select(-Total), by = c("OBJECTID", "name")
  )  %>%
  filter(!is.na(Species)) %>%
  select(-name) %>%
  group_by(OBJECTID, Species) %>% # Sometimes the same species is listed twice...combine those proportions
  summarise(value = sum(value))

matching_data$PIempirical %>% group_by(OBJECTID) %>% summarize(value = sum(value)) %>% pull(value) %>% unique()

# Check if all the OBJECTIDs are in both data sets:
all(unique(matching_data$PIempirical$OBJECTID) %in% Cedric_FUNA$OBJECTID)

matching_data$PIempirical = matching_data$PIempirical %>%
  rename(totvalue = value)

# Calculate the percent softwood for each OBJECTID:

matching_data$PIempirical = PIobjects %>%
  select(OBJECTID, GMV9_mean, TPH9_mean, AHT9_mean) %>%
  left_join(
    matching_data$PIempirical %>% filter(Species == "SW") %>% select(-Species), by = c("OBJECTID")
  ) %>% 
  mutate(Prop_SW_PI = replace_na(totvalue, 0)) %>%
  select(-totvalue)


# Calculate the proportion of species cover by OBJECTID:
matching_data$PIspecies = newPI %>%
  select(OBJECTID, contains("L1S"))%>%
  pivot_longer(-OBJECTID, values_to = "Species") %>%
  separate(name, into = c(NA, "name"), sep = -1) %>%
  full_join(
    newPI %>%
      select(OBJECTID, contains("L1PR"))%>%
      pivot_longer(-OBJECTID) %>%
      separate(name, into = c(NA, "name"), sep = -1)%>%
      mutate(value = value/10) %>%
      group_by(OBJECTID) %>%
      mutate(Total = sum(value)) %>%
      mutate(value = value/Total) %>% # Rescale so it sums to 1
      select(-Total), by = c("OBJECTID", "name")
  )  %>%
  filter(!is.na(Species)) %>%
  select(-name) %>%
  group_by(OBJECTID, Species) %>% # Sometimes the same species is listed twice...combine those proportions
  summarise(value = sum(value))

matching_data$PIspecies %>% group_by(OBJECTID) %>% summarize(value = sum(value)) %>% pull(value) %>% unique()

# Check if all the OBJECTIDs are in both data sets:
all(unique(newPI_species$OBJECTID) %in% Cedric_FUNA$OBJECTID)

# Convert the group or unmatched species that are at high frequency to species in Anthony's model:
matching_data$PIspecies = matching_data$PIspecies %>%
  left_join(
    readxl::read_xlsx("Data/groupcode_to_species.xlsx") %>%
      select(-Allocation1, - Allocation) %>%
      pivot_wider(names_from = `Species Code`, values_from = Allocation_Mike, values_fill = 0), by = c("Species" = "Group Code")
  ) 

matching_data$PIspecies %>% filter(is.na(TA)) %>% pull(Species) %>% unique() # All these are codes we are OK getting rid of

matching_data$PIspecies = matching_data$PIspecies %>%
  rename(totvalue = value) %>%
  pivot_longer(!OBJECTID & !Species & !totvalue) %>%
  filter(value > 0) %>%
  mutate(value = totvalue*value/100) %>% # Calculate the new percentage
  select(-Species, -totvalue) %>%
  rename(Species = name) %>%
  rename(Prop_species_PI = value)

# Calculate the data for the yield curves:
matching_data$YCempirical = yc %>% 
  select(`_Age`, FUNA, contains("v"), -VOLtot) %>%
  pivot_longer(!`_Age` & !FUNA) %>%
  separate(name, into = c("Species", NA), sep = -1) %>%
  left_join(
    read_csv("Data/SPcodeHWSW.csv") %>% select(-Name), by = c("Species" = "SpeciesCode")
  ) %>% 
  group_by(`_Age`, FUNA, Type) %>%
  summarize(value = sum(value)) %>%
  group_by(`_Age`, FUNA) %>%
  mutate(Prop_SW = value/sum(value)) %>%
  filter(Type == "SW") %>%
  ungroup()

matching_data$YCempirical = yc %>%
  select(`_Age`, FUNA, DTY9, HGTmerch, VOLtot) %>%
  left_join(
    matching_data$YCempirical,by = join_by(`_Age`, FUNA)
  ) %>%
  select(-Type, -value) %>%
  mutate(Prop_SW = replace_na(Prop_SW, 0))

matching_data$YCspecies = yc %>% 
  select(`_Age`, FUNA, contains("v"), -VOLtot) %>%
  pivot_longer(!`_Age` & !FUNA) %>%
  separate(name, into = c("Species", NA), sep = -1) %>%
  group_by(`_Age`, FUNA) %>%
  mutate(Prop_species = value/sum(value)) %>%
  ungroup() %>%
  mutate(Prop_species_yc = replace_na(Prop_species, 0)) %>%
  select(-Prop_species, -value)


IDS = unique(matching_data$PIempirical$OBJECTID)

matchfunction <- function(IDSf,mdata = matching_data){
  
  rescale01 <- function(X){
    (X - min(X, na.rm = T))/(max(X, na.rm = T) - min(X, na.rm = T))
  }
  
  empdata2 = matching_data$PIempirical %>%
    filter(OBJECTID == IDSf)
  
  if(dim(empdata2)[1] >1) stop("Repeated OBJECTID")
  
  temp1 = matching_data$YCempirical %>%
    mutate(dvol = rescale01((VOLtot - empdata2$GMV9_mean)^2),
           dsw = rescale01((Prop_SW - empdata2$Prop_SW_PI)^2),
           dheight = rescale01((HGTmerch - empdata2$AHT9_mean)^2),
           ddensity = rescale01((DTY9 - empdata2$TPH9_mean)^2))
  
  temp1 = temp1 %>%
    left_join(
      matching_data$YCspecies %>%
        right_join(
          matching_data$PIspecies %>%
            filter(OBJECTID == 1), relationship = "many-to-many",by = join_by(Species)
        ) %>%
        mutate(dSpecies = (Prop_species_yc - Prop_species_PI)^2) %>%
        group_by(`_Age`, FUNA) %>%
        summarise(dSpecies = sum(dSpecies),.groups = "drop"), by = c("_Age", "FUNA")
    ) %>% mutate(dSpecies = replace_na(dSpecies,0))
  
  temp1 %>% 
    mutate(diffall = dvol + dsw + ddensity) %>%
    slice_min(diffall) %>%
    mutate(OBJECTID = IDSf)
  
}

matchfunction(IDSf = 5)

cl = makeCluster(15)
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(tidyr))
clusterExport(cl=cl, varlist=c("matching_data"))
cors = parLapply(cl, IDS, matchfunction,mdata = matching_data)
stopCluster(cl)

do.call("rbind", cors) %>% write_rds("ResultsData/matches/matching_SW_vol_denisty.rds")

# Load back in the final matches and plot them:

finalmatch = read_rds("ResultsData/matches/matching_SW_vol_density.rds")


read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp") %>%
  select(OBJECTID) %>%
  left_join(
    finalmatch
  ) %>%
  select(OBJECTID, VOLtot, Prop_SW, HGTmerch, DTY9) %>%
  left_join(
    matching_data$PIempirical
  ) %>%
  mutate(Area = as.numeric(st_area(.))/10000) %>%
  st_drop_geometry() %>%
  pivot_longer(!OBJECTID & !Area) %>%
  mutate(value = value*Area) %>%
  group_by(name) %>%
  summarize(value = mean(value))

# Prepare a file output for Mike:

selmatch = finalmatch %>%
  select(OBJECTID, `_Age`, FUNA)

mapforarea = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp")

selmatch = mapforarea %>%
  left_join(
    selmatch
  ) %>%
  select(OBJECTID, CAT, `_Age`, FUNA, L1FUNA, L2FUNA) %>%
  mutate(Area = st_area(.))

selmatch2 = selmatch %>% st_drop_geometry()


selmatch2 %>%
  group_by(CAT) %>%
  summarize(sum(Area))

selmatch2 %>%
  filter(CAT == "FO" & is.na(FUNA))

selmatch2 = selmatch2 %>%
  rename(L1FUNA_photointerp = L1FUNA,
         L2FUNA_photointerp = L2FUNA) %>%
  left_join(
    yc, by = c("FUNA", "_Age")
  ) 

hist(selmatch2$`_Age`)


selmatch2 %>% select(OBJECTID, Area, contains("v") & !VOLtot) %>%
  pivot_longer(!OBJECTID & !Area) %>%
  group_by(name) %>%
  mutate(value = value/Area) %>%
  summarize(tot = sum(value, na.rm = T)) %>%
  mutate(tot = tot/sum(tot))

selmatch2 %>%
  filter(!is.na(FUNA)) %>%
  mutate(Periods = ifelse(`_Age`*5 < 25, "0-25",
                          ifelse(`_Age`*5 > 75, "75+", 
                                 ifelse(`_Age`*5 > 25 & `_Age`*5 < 50, "25-50", "50-75")))) %>%
  group_by(FUNA, Periods) %>%
  mutate(Area = as.numeric(Area)/10000) %>%
  summarize(Area = sum(Area)) %>%
  pivot_wider(names_from = FUNA, values_from = Area, values_fill = 0)
  

selmatch2 %>% 
  write_csv("Data/FUNAmatching_yieldcurve_SW_vol_species.csv")

  
pdf("Plots/density.pdf")
finalmatch %>%
  mutate(Run = "Density") %>%
  bind_rows(
    finalmatch_old %>%
      mutate(Run = "Height")
  ) %>%
  filter(Type == "Full") %>%
  ggplot(aes(x = `_Age`, fill = Run)) + geom_density(alpha = 0.5)
dev.off()

finalmatch = finalmatch %>%
  mutate(Run = "Density") %>%
  bind_rows(
    finalmatch_old %>%
      mutate(Run = "Height")
  )

area_objects = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020_Cedric/Gagetown_Landbase_07_24_2020b.shp") %>% 
  select(OBJECTID) %>%
  mutate(Shape_Area = st_area(.)) %>%
  mutate(Shape_Area = as.numeric(Shape_Area/10000))


read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020_Cedric/Gagetown_Landbase_07_24_2020b.shp") %>% 
  select(OBJECTID) %>%
  mutate(Shape_Area = st_area(.)) %>%
  mutate(Shape_Area = as.numeric(Shape_Area/10000)) %>%
  pull(Shape_Area) %>% sum()

area_objects %>%
  st_drop_geometry() %>%
  left_join(
    finalmatch %>%
      mutate(Run = "Density") %>%
      bind_rows(
        finalmatch_old %>%
          mutate(Run = "Height")
      ) %>%
      filter(Type == "Full")
  ) %>%
  group_by(`_Age`, Run) %>%
  summarize(Shape_Area = sum(Shape_Area)) %>%
  filter(!is.na(Run)) %>%
  ggplot(aes(x = `_Age`, y = Shape_Area, color = Run)) + geom_line() + ylab("Area (ha)") + xlab("Age (periods)")


# Here we are using Cedric's map to assign the FUNA and Age matches to the divided polygons.
finalmap = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020_Cedric/Gagetown_Landbase_07_24_2020b.shp") %>% 
  select(OBJECTID, L1FUNA,Shape_Area,TRT,THEME5) %>%
  left_join(
    finalmatch, by = "OBJECTID"
  ) %>%
  mutate(Shape_Area = st_area(.)) %>%
  mutate(Shape_Area = as.numeric(Shape_Area/10000)) # Convert m2 to hectares


#Add Woodstock themes ----
library(data.table)
map = finalmap

Zones = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020_Cedric/mgmt.shp")

NAD83 <- st_crs("+proj=sterea +lat_0=46.5 +lon_0=-66.5 +k=0.999912 +x_0=2500000 +y_0=7500000 +ellps=GRS80 +units=m +no_defs")
# Zones <- st_transform(Zones,st_crs(NAD83))
map <- st_transform(map,st_crs(Zones))

map <- st_intersection(map, left = TRUE, 
                       Zones [ , c("Zone")],left = TRUE) #left = FALSE retain only overlap
map = map %>% 
  rename("_Age" = "X_Age", "THEME3" = "Zone", "THEME4" = "THEME5")

#Woodstock Age
map <- as.data.table(map)
map[ , WKAge := `_Age`/5 ]
map[ WKAge < 1 , WKAge := 1 ] #no bad _age in Woodstock

#THEME4
map$FUNA = map$FUNA %>% replace_na("XXNA")
map[FUNA %in% c("XXNA"), THEME4:= "D1"]
map$THEME4 = map$THEME4 %>% replace_na("B1")

#THEME2
map = map %>% 
  rename("THEME2" = "FUNA")

#THEME1
#commercial thinning & pre-commercial thinning
map[TRT %in% c("TI", "CT"),THEME1:= "TI"]
#partial harvest
map[TRT %in% c("PC", "ST"),THEME1:= "PC"]
#plantations
map[TRT %in% c("PL"),THEME1:= "PL"]
#natural
map$THEME1 = map$THEME1 %>% replace_na("NA")
#non-forested
map[THEME2 %in% c("XXNF"),THEME1:= "XX"]

#THEME5
map[ WKAge %between% c(0,3) , THEME5 := 0 ]
map[ WKAge %between% c(2,9) , THEME5 := 25 ]
map[ WKAge %between% c(8,14) , THEME5 := 50 ]
map[ WKAge > 13 , THEME5 := 75 ]
#age col based on THEME5
map[, c("THEME5", "WKAge")] <- lapply(map[, c("THEME5", "WKAge")], as.integer)
map[ , TH5_WKage := THEME5/5 ]
map[ THEME5 < 1 , TH5_WKage := 1 ]

#THEME6
map[,THEME6:= "P1"] #all stand now exist in 2020

#THEME7
map[,THEME7:= "BL"] #first run = all stand in baseline

#reorder column
map <- map[, c("OBJECTID", "Shape_Area", "TRT", "_Age", "WKAge", "TH5_WKage", "THEME1", "THEME2", "THEME3", "THEME4", "THEME5", "THEME6", "THEME7", "geometry")]

#Recalculate Area in ha
map <- st_as_sf(map)
map$Shape_Area <- st_area(map)/10000

map <- map[!duplicated(map$geometry),]

map %>%
  write_sf("Data/Cedricmap/Cedricmap.shp")

st_write(map, "GT_Match_02202023", driver="ESRI Shapefile", delete_layer = TRUE)