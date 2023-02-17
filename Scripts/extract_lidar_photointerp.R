# Extract the LiDAR data by data if needed ----
# (Once you have the file "LiDAR_extracts_PI.rds" this is not necessary)

# Library
library(sf)
library(tidyverse)

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