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



# Calculate the proportion of species cover by OBJECTID:
newPI = newPI %>%
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

newPI %>% group_by(OBJECTID) %>% summarize(value = sum(value)) %>% pull(value) %>% unique()

# Check if all the OBJECTIDs are in both data sets:
all(unique(newPI$OBJECTID) %in% Cedric_FUNA$OBJECTID)

# Calculate the proportion of species by cover:

coverprops = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp") %>%
  st_drop_geometry() %>%
  select(OBJECTID, Shape_Area) %>%
  full_join(
    newPI, by = "OBJECTID"
  ) %>%
  mutate(value = value*Shape_Area) %>%
  group_by(Species) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(value = value/sum(value, na.rm = T)) %>%
  arrange(value) %>% print(n = Inf)

# Break up the species codes into matches for the yield curve:

# Yield curve species:
c((yc %>% select(contains("v"), -VOLtot) %>% colnames() %>% str_split("v", simplify = T))[,1])

# PO becomes TA
# BI becomes WB?
# IH becomes TA, WB?
# FS becomes 75% BF and 25% RS/BS
# SW becomes ???
# HW becomes ???
# PI becomes WP, RP, JP?
# SF becomes 75% RS/BS and 25% BF
# NC is removed
# OH becomes RO, YB
# OS becomes EC, EH
# AL is removed
# BE is removed
# DS is removed
# DF is removed
# GB is removed
# IR is removed
# BC is removed
# SP becomes RS/WS
# AS is removed
# BH is removed

# Next need to code the matching! The code needs to add up all the relevant species unless they are recorded elsewhere in the stand. Need to figure out how to deal with that part...

unmatchedspecies = unique(newPI$Species)[!(unique(newPI$Species) %in% c((yc %>% select(contains("v"), -VOLtot) %>% colnames() %>% str_split("v", simplify = T))[,1]))]

table(newPI$Species) %>%
  data.frame() %>%
  filter(Var1 %in% unmatchedspecies) %>%
  arrange(-Freq)

rm(unmatchedspecies)

# Convert the group or unmatched species that are at high frequency to species in Anthony's model:
newPI = newPI %>%
  left_join(
    readxl::read_xlsx("Data/groupcode_to_species.xlsx") %>%
      select(-Allocation1, - Allocation) %>%
      pivot_wider(names_from = `Species Code`, values_from = Allocation_Mike, values_fill = 0), by = c("Species" = "Group Code")
  ) 

readxl::read_xlsx("Data/groupcode_to_species.xlsx") %>%
  group_by(`Group Code`) %>%
  summarise(Allocation = sum(Allocation_Mike)) %>% pull(Allocation) %>% unique()

newPI %>% filter(is.na(TA)) %>% pull(Species) %>% unique() # All these are codes we are OK getting rid of

newPI = newPI %>%
  rename(totvalue = value) %>%
  pivot_longer(!OBJECTID & !Species & !totvalue) %>%
  filter(value > 0) %>%
  mutate(value = totvalue*value/100) %>% # Calculate the new percentage
  select(-Species, -totvalue) %>%
  rename(Species = name)

# GMV9 matches with volume in Anthony curves
# TPH0 matches with DTY9 in Antony curves

# Use the mean GMV9 for the fit by species:
PImatchyc = newPI %>%
  rename(Prop_species_PI = value)

yc2 = yc %>% 
  select(`_Age`, FUNA, contains("v"), -VOLtot) %>%
  pivot_longer(!`_Age` & !FUNA) %>%
  separate(name, into = c("Species", NA), sep = -1) %>%
  group_by(`_Age`, FUNA) %>%
  mutate(Prop_species = value/sum(value)) %>%
  ungroup() %>%
  mutate(Prop_species_yc = replace_na(Prop_species, 0))

IDS = unique(PImatchyc$OBJECTID)


# Compare matching by proportion cover and by volume comparison
PImatchyc %>%
  filter(OBJECTID == 2) %>%
  full_join(
    yc2, by = join_by(Species), relationship =
      "many-to-many"
  ) %>%
  filter(!is.na(FUNA)) %>% filter(!is.na(OBJECTID))%>%
  mutate(diff = (Prop_species_PI - Prop_species_yc)^2) %>%
  group_by(OBJECTID, `_Age`, FUNA) %>%
  summarise(diff = sum(diff), .groups = NULL) %>%
  left_join(
    newPI %>%
      left_join(
        PIobjects
      ) %>%
      mutate(GMV9 = GMV9_mean*value) %>%
      select(OBJECTID, Species, GMV9) %>%
      filter(OBJECTID == 2) %>%
      full_join(
        yc %>% 
          select(`_Age`, FUNA, contains("v"), -VOLtot) %>%
          pivot_longer(!`_Age` & !FUNA) %>%
          separate(name, into = c("Species", NA), sep = -1), by = join_by(Species), relationship =
          "many-to-many"
      ) %>%
      filter(!is.na(FUNA)) %>% filter(!is.na(OBJECTID))%>%
      mutate(diff = (GMV9 - value)^2) %>%
      group_by(OBJECTID, `_Age`, FUNA) %>%
      summarise(diffvol = sum(diff), .groups = NULL) 
  ) %>% 
  filter(diff < 0.07) %>%
  ggplot(aes(x = diff, y = diffvol)) + geom_label(aes(label = FUNA, color = `_Age`))


matchfunction <- function(IDSf, PImatchycf = PImatchyc, yc2f = yc2, ycf = yc, PIobjectsf = PIobjects, Cedric_FUNAf = Cedric_FUNA){
  tmp1 = PImatchycf %>%
    filter(OBJECTID == IDSf) %>%
    full_join(
      yc2f, by = join_by(Species), relationship =
        "many-to-many"
    ) %>%
    filter(!is.na(FUNA)) %>% filter(!is.na(OBJECTID))
  
  rescale01 <- function(X){
    (X - min(X, na.rm = T))/(max(X, na.rm = T) - min(X, na.rm = T))
  }
  
  if(dim(tmp1)[1] > 0){ # If there is a species match, go forward with it
    tmp1 = tmp1 %>%
      mutate(dSpecies = (Prop_species_PI - Prop_species_yc)^2) %>%
      group_by(OBJECTID, `_Age`, FUNA) %>%
      summarise(dSpecies = sum(dSpecies), .groups = NULL) %>%
      left_join(
        ycf %>%
          select(`_Age`, FUNA, DTY9) %>%
          ungroup() %>%
          mutate(dTHP9 = (DTY9 - pull(PIobjectsf[PIobjectsf$OBJECTID == IDSf, "TPH9_mean"]))^2) %>%
          select(-DTY9), by = c("_Age", "FUNA")
      ) %>%
      left_join(
        ycf %>%
          select(`_Age`, FUNA, HGTmerch) %>%
          ungroup() %>%
          mutate(dAHT = (HGTmerch - pull(PIobjectsf[PIobjectsf$OBJECTID == IDSf, "AHT9_mean"]))^2) %>%
          select(-HGTmerch), by = c("_Age", "FUNA")
      ) %>%
      left_join(
        ycf %>%
          select(`_Age`, FUNA, VOLtot) %>%
          ungroup() %>%
          mutate(dGMV9 = (VOLtot - pull(PIobjectsf[PIobjectsf$OBJECTID == IDSf, "GMV9_mean"]))^2) %>%
          select(-VOLtot), by = c("_Age", "FUNA")
      ) %>% 
      ungroup() %>%
      mutate(dSpecies = rescale01(dSpecies),
             dTHP9 = rescale01(dTHP9),
             dAHT = rescale01(dAHT),
             dGMV9 = rescale01(dGMV9)) %>%
      mutate(diff_all = dSpecies*2 + dGMV9 + dTHP9) %>% # Excluding density match here
      arrange(diff_all)
  }else{ # If there are no species matches, only match volume and density
    tmp1 = yc2f %>%
      select(`_Age`, FUNA, DTY9) %>%
      ungroup() %>%
      mutate(dTHP9 = (DTY9 - pull(PIobjectsf[PIobjectsf$OBJECTID == IDSf, "TPH9_mean"]))^2) %>%
      select(-DTY9)%>%
      full_join(
        ycf %>%
          select(`_Age`, FUNA, VOLtot) %>%
          ungroup() %>%
          mutate(dGMV9 = (VOLtot - pull(PIobjectsf[PIobjectsf$OBJECTID == IDSf, "GMV9_mean"]))^2) %>%
          select(-VOLtot), by = c("_Age", "FUNA")
      )  %>%
      full_join(
        ycf %>%
          select(`_Age`, FUNA, HGTmerch) %>%
          ungroup() %>%
          mutate(dAHT = (HGTmerch - pull(PIobjectsf[PIobjectsf$OBJECTID == IDSf, "AHT9_mean"]))^2) %>%
          select(-HGTmerch), by = c("_Age", "FUNA")
      ) %>%
      mutate(dTHP9 = rescale01(dTHP9),
             dGMV9 = rescale01(dGMV9),
             dAHT = rescale01(diff4))%>%
      mutate(diff_all = dGMV9 + dTHP9) %>%
      mutate(dSpecies = NA) %>%
      mutate(OBJECTID = IDS) %>%
      select(OBJECTID, `_Age`, FUNA,   dSpecies,dTHP9,dGMV9,dAHT, diff_all) %>%
      arrange(diff_all)
    
  }
  # tmp2 = tmp1 %>% filter(FUNA == (Cedric_FUNAf %>% filter(OBJECTID == IDSf) %>% pull(THEME2))) %>% arrange(diff_all)
  # 
  # return(rbind(tmp1[1,], tmp2[1,]) %>% mutate(Type = c("Full", "FUNA")))
  
  return(tmp1[1,])
}

matchfunction(IDSf = 3)


cl = makeCluster(15)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl=cl, varlist=c("PImatchyc","yc2","yc", "PIobjects", "Cedric_FUNA"))
cors = parLapply(cl, IDS, matchfunction, PImatchycf = PImatchyc, yc2f = yc2, ycf = yc, PIobjectsf = PIobjects, Cedric_FUNAf = Cedric_FUNA)
stopCluster(cl)

do.call("rbind", cors) %>% write_rds("ResultsData/matches/matchingFUNAage_spx2_vol_density.rds")

# Load back in the final matches and plot them:

finalmatch = read_rds("ResultsData/matches/matchingFUNAage_spx2_vol_density.rds")


# Compare fits for the different versions:

LF = list.files("ResultsData/matches/")

do.call("rbind",lapply(1:length(LF), function(x) read_rds(paste0("ResultsData/matches/",LF[x])) %>% mutate(fit = LF[x]))) %>%
  group_by(fit) %>%
  summarise(dSpecies = sum(dSpecies),
            dTHP9     = sum(dTHP9),
            dAHT     = sum(dAHT),
            dGMV9 = sum(dGMV9))

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
  write_csv("Data/FUNAmatching_yieldcurve_height_withL2.csv")

  
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

finalmap %>%
  filter(Type == "Full") %>%
  select(OBJECTID, `_Age`, FUNA) %>%
  write_sf("Data/height_match/height_match.shp")

# Calculate the total species composition
finalmap %>%
  st_drop_geometry() %>%
  select(OBJECTID, Shape_Area, `_Age`, FUNA, Type, Run) %>%
  left_join(
    yc %>% select(`_Age`, FUNA, contains("v"), -VOLtot), by = c("_Age", "FUNA")
  ) %>%
  pivot_longer(contains('v')) %>%
  mutate(GMV9area = value*Shape_Area) %>% # Multiple GMV m2/ha by ha of the stand
  group_by(name, Type, Run) %>%
  summarize(GMV9area = sum(GMV9area, na.rm = T)) %>%
  group_by(Type, Run) %>%
  separate(name, into = c("Species", NA), sep = -1) %>%
  mutate(GMV9area = 100*GMV9area/sum(GMV9area)) %>%
  filter(!is.na(Type))  %>% 
  filter(Type == "Full") %>%
  arrange(GMV9area) %>% pivot_wider(names_from = Run, values_from = GMV9area) %>%
  left_join(
    coverprops %>% rename(`PI Cover` = value), by = "Species") %>% write_csv("matching_percents_mike.csv")

finalmap %>%
  st_drop_geometry() %>%
  select(OBJECTID, Shape_Area, `_Age`, FUNA, Type, Run) %>%
  filter(Type == 'Full') %>%
  group_by(Run, FUNA) %>%
  summarize(Age = paste0(signif(mean(`_Age`), 2), " (", signif(sd(`_Age`), 2), ") %")) %>%
  pivot_wider(names_from = Run, values_from = Age)%>% write_csv("avg_age_FUNA_mike.csv")

finalmap %>%
  st_drop_geometry() %>%
  select(OBJECTID, Shape_Area, `_Age`, FUNA, Type, Run) %>%
  filter(Type == 'Full') %>%
  group_by(Run, FUNA) %>%
  summarize(Age = sum(Shape_Area)) %>%
  pivot_wider(names_from = Run, values_from = Age)%>% write_csv("FUNA_area_mike.csv")


finalmap_Full = finalmap %>% filter(Type == "Full")

table(finalmap_Full$L1FUNA, finalmap_Full$FUNA) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  rename(`Interpreted FUNA` = Var1) %>%
  write_csv("matching_FUNAs.csv")

pdf("matching_maps_height.pdf", width = 8, height = 8)
finalmap_Full %>%
  ggplot(aes(fill = FUNA, color = FUNA)) + geom_sf()

finalmap_Full %>%
  ggplot(aes(fill = `_Age`, color = `_Age`)) + geom_sf()
dev.off()


#Add Woodstock themes ----
library(data.table)
map = finalmap_Full

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