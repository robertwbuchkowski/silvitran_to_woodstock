# Code the load and match the Yield Curves to the current forest:

# Library
library(sf)
library(tidyverse)

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

# Get the landscape data ----

newPI = read_sf("C:/Users/rober/Documents/GitHubRepos/occupancy_exploration/Data/inventory_poly_outside_base/inventory_poly_outside_base.shp") %>%
  mutate(OBJECTID = 1:10)

PLOTID_OBJECTID = newPI %>% st_drop_geometry() %>% select(OBJECTID, PLOTID)

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

# Clean out LiDAR data:
rm(temp1); gc()

if(max(table(newPI$OBJECTID)) != 1) print("WARNING: Unique indentified is duplicated!") else print("YAY: Unique indentifier is unique!")

#Keep only what we need for the match:
newPI = newPI %>%  
  st_drop_geometry() %>%
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

newPI %>% group_by(OBJECTID) %>% summarize(value = sum(value)) %>% pull(value) %>% unique()

# Convert the group or unmatched species that are at high frequency to species in Anthony's model:
newPI = newPI %>%
  left_join(
    readxl::read_xlsx("Data/groupcode_to_species.xlsx") %>%
      select(-Allocation1, - Allocation) %>%
      pivot_wider(names_from = `Species Code`, values_from = Allocation_Mike, values_fill = 0), by = c("Species" = "Group Code")
  ) 

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
  left_join(
    PIobjects
  ) %>%
  mutate(GMV9 = GMV9_mean*value) %>%
  select(OBJECTID, Species, GMV9)

yc2 = yc %>% 
  select(`_Age`, FUNA, contains("v"), -VOLtot) %>%
  pivot_longer(!`_Age` & !FUNA) %>%
  separate(name, into = c("Species", NA), sep = -1)

rescale01 <- function(X){
  (X - min(X, na.rm = T))/(max(X, na.rm = T) - min(X, na.rm = T))
}

IDS = unique(PImatchyc$OBJECTID)

result = vector('list', length = length(IDS))

for(i in 1:length(IDS)){
  tmp1 = PImatchyc %>%
    filter(OBJECTID == IDS[i]) %>%
    full_join(
      yc2
    ) %>%
    filter(!is.na(FUNA)) %>% filter(!is.na(OBJECTID))
  
  if(dim(tmp1)[1] > 0){ # If there is a species match, go forward with it
    tmp1 = tmp1 %>%
      mutate(diff = (GMV9 - value)^2) %>%
      group_by(OBJECTID, `_Age`, FUNA) %>%
      summarise(diff = sum(diff), .groups = NULL) %>%
      left_join(
        yc %>%
          select(`_Age`, FUNA, DTY9) %>%
          ungroup() %>%
          mutate(diff2 = (DTY9 - pull(PIobjects[PIobjects$OBJECTID == IDS[i], "TPH9_mean"]))^2) %>%
          select(-DTY9), by = c("_Age", "FUNA")
      ) %>%
      left_join(
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

do.call("rbind", result) %>% select(OBJECTID, `_Age`, FUNA) %>% 
  left_join(
    PLOTID_OBJECTID
  ) %>%
  select(PLOTID, `_Age`, FUNA) %>%
  write_rds("C:/Users/rober/Documents/GitHubRepos/occupancy_exploration/Data/inventory_poly_outside_base/age_FUNA_outside_base.rds")