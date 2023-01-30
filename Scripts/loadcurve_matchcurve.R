# Code the load and match the Yield Curves to the current forest:

# Library
library(sf)
library(tidyverse)

# Load in the yield curves:
LF = list.files("YieldCurves", recursive = T)

# Keep only the correct yield curve files:
LF = LF[grepl("Z_Z_Z_P1_BL.csv", LF)]

# Load in the yield curves:
yc = vector("list", length = length(LF))

for(i in 1:length(LF)){
  yc[[i]] = read_csv(paste0("YieldCurves/",LF[i])) %>%
    mutate(FUNA = str_split(LF[i], "/")[[1]][1]) %>%
    pivot_longer(!`_Age` & !FUNA)
}

yc = do.call("rbind", yc) %>%
  pivot_wider(values_fill = 0) # This is OK because we are filling in missing species with zero volume

# Get the landscape data ----

# This is Cedric's landscape---does not contain the information that we need and no longer necessary to group stands--the matching algorithm will do that for us.
if(F){ 
  # Load in Cedric's landscape file:
  WK_Land = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromColleagues/Cedric_gagetown_landscape/WK_LANDBASE_052022.shp")
  
  rm(WK_Land); gc()
}


# Load in photo-interpreted data since there is no species composition in the other one:

newPI = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp")%>% 
  # Keep only UID:
  select(OBJECTID)

# Load in the full EFI data and join to the photo-interpreted data set: 

temp1 = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromColleagues/EFI_Gagetown_bounded/NB_EFI_cfbg_lassoclipN.shp") %>%
  bind_rows(
    read_sf("C:/Users/rober/Documents/AFC/Data/DataFromColleagues/EFI_Gagetown_bounded/NB_EFI_cfbg_lassoclipS.shp")
  )

# Keep only useful columns:
temp1 = temp1[,c("UID","GMV9","TPH9","geometry")]; gc()

PIobjects = newPI %>%
  st_join(
    temp1
  ) %>%
  tibble() %>%
  group_by(OBJECTID) %>%
  summarise(GMV9_mean = mean(GMV9, na.rm = T),
            GMV9_sd = sd(GMV9, na.rm = T),
            GMV9_min = min(GMV9, na.rm = T),
            GMV9_max = max(GMV9, na.rm = T),
            TPH9_mean = mean(TPH9, na.rm = T),
            TPH9_sd = sd(TPH9, na.rm = T),
            TPH9_min = min(TPH9, na.rm = T),
            TPH9_max = max(TPH9, na.rm = T))

rm(temp1); gc() # Get rid of large LiDAR data set.

PIobjects %>% write_rds("Data/LiDAR_extracts_PI.rds")

rm(PIobjects); gc()

# Combine the LiDAR data and the PI data:

PIobjects = read_rds("Data/LiDAR_extracts_PI.rds")

newPI = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp")%>% 
  
  #Keep only what we need for the match:
  tibble() %>%
  select(OBJECTID, contains("L1S") & !L1SC, contains("L1PR"))

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

# Check that they all sum to 1:
newPI %>% summarize(t = sum(value)) %>% pull(t) %>% unique()

# GMV9 matches with volume in Anthony curves
# TPH0 matches with DTY9 in Antony curves

# Use the mean GMV9 for the fit:
PImatchyc = newPI %>%
  left_join(
    PIobjects
  ) %>%
  mutate(GMV9 = GMV9_mean*value) %>%
  select(OBJECTID, Species, GMV9) 

yc2 = yc %>% 
  select(`_Age`, FUNA, contains("v"), -VOLtot) %>%
  pivot_longer(!`_Age` & !FUNA) %>%
  separate(name, into = c("Species", NA), sep = -1)

# Anthony only has some of the species...all not included in the model are first treated as mismatches:
sort(unique(PImatchyc$Species))
sort(unique(yc2$Species))

rescale01 <- function(X){
  (X - min(X, na.rm = T))/(max(X, na.rm = T) - min(X, na.rm = T))
}

IDS = unique(PImatchyc$OBJECTID)

result = vector('list', length = length(IDS))

for(i in 1:length(IDS)){
  tmp1 = PImatchyc %>%
    filter(OBJECTID == IDS[i]) %>%
    left_join(
      yc2
    ) %>%
    filter(!is.na(FUNA))
  
  if(dim(tmp1)[1] > 0){ # If there is a species match, go forward with it
    tmp1 = tmp1 %>%
      mutate(diff = (GMV9 - value)^2) %>%
      group_by(OBJECTID, `_Age`, FUNA) %>%
      summarise(diff = sum(diff), .groups = NULL) %>% # Something is not working here.
      mutate(diff = diff) %>%
      full_join(
        yc %>%
          select(`_Age`, FUNA, DTY9) %>%
          ungroup() %>%
          mutate(diff2 = (DTY9 - pull(PIobjects[PIobjects$OBJECTID == IDS[i], "TPH9_mean"]))^2) %>%
          select(-DTY9), by = c("_Age", "FUNA")
      ) %>%
      full_join(
        yc %>%
          select(`_Age`, FUNA, VOLtot) %>%
          ungroup() %>%
          mutate(diff3 = (VOLtot - pull(PIobjects[PIobjects$OBJECTID == IDS[i], "GMV9_mean"]))^2) %>%
          select(-VOLtot), by = c("_Age", "FUNA")
      ) %>% 
      ungroup() %>%
      mutate(diff = rescale01(diff),
             diff2 = rescale01(diff2),
             diff3 = rescale01(diff3)) %>%
      mutate(diff_all = diff + diff2 + diff3) %>%
      arrange(diff_all) %>%
      rename(dSpecies = diff,
             dTHP9 = diff2,
             dGMV9 = diff3)
  }else{ # If there are no species matches, only match volume and density
    tmp1 = yc %>%
      select(`_Age`, FUNA, DTY9) %>%
      ungroup() %>%
      mutate(diff2 = (DTY9 - pull(PIobjects[PIobjects$OBJECTID == IDS[i], "TPH9_mean"]))^2) %>%
      select(-DTY9)%>%
      full_join(
        yc %>%
          select(`_Age`, FUNA, VOLtot) %>%
          ungroup() %>%
          mutate(diff3 = (VOLtot - pull(PIobjects[PIobjects$OBJECTID == IDS[i], "GMV9_mean"]))^2) %>%
          select(-VOLtot), by = c("_Age", "FUNA")
      ) %>%
      mutate(diff2 = rescale01(diff2),
             diff3 = rescale01(diff3))%>%
      mutate(diff_all = diff2 + diff3) %>%
      rename(dTHP9 = diff2,
             dGMV9 = diff3) %>%
      mutate(dSpecies = NA) %>%
      mutate(OBJECTID = IDS[i]) %>%
      select(OBJECTID, `_Age`, FUNA,   dSpecies,dTHP9,dGMV9, diff_all) %>%
      arrange(diff_all)
      
  }
  
  result[[i]] = tmp1[1,] 
  print(i)
}

do.call("rbind", result) %>% write_rds("Data/matchingFUNAage_stand.rds") 

# Load back in the final matches and plot them:

finalmatch = read_rds("Data/matchingFUNAage_stand.rds")


finalmap = read_sf("C:/Users/rober/Documents/AFC/Data/DataFromAFC/Gagetown_Landbase_07_24_2020/Gagetown_Landbase_07_24_2020.shp") %>% 
  select(OBJECTID, L1FUNA,Shape_Area) %>%
  left_join(
    finalmatch
  )

# Calcualte the total species composition
finalmap %>%
  tibble() %>%
  select(OBJECTID, Shape_Area, `_Age`, FUNA) %>%
  left_join(
    yc %>% select(`_Age`, FUNA, contains("v"), -VOLtot), by = c("_Age", "FUNA")
  ) %>%
  pivot_longer(contains('v')) %>%
  mutate(GMV9area = value/Shape_Area) %>%
  group_by(name) %>%
  summarize(GMV9area = sum(GMV9area, na.rm = T)) %>%
  separate(name, into = c("Species", NA), sep = -1) %>%
  mutate(GMV9area = 100*GMV9area/sum(GMV9area)) %>%
  arrange(GMV9area) %>% write_csv("Data/matching_percents.csv")

table(finalmap$L1FUNA, finalmap$FUNA) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  rename(`Interpreted FUNA` = Var1) %>%
  write_csv("Data/matching_FUNAs.csv")

pdf("Plots/matching_maps.pdf", width = 8, height = 8)
finalmap %>%
  ggplot(aes(fill = FUNA, color = FUNA)) + geom_sf()

finalmap %>%
  ggplot(aes(fill = `_Age`, color = `_Age`)) + geom_sf()
dev.off()

# Read in species type if needed:
read_csv("Data/SPcodeHWSW.csv") %>% select(-Name)
