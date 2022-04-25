# Explore the possible yield curves
# A: Robert W. Buchkowski
# D: April 25/2022

# Load in the libraries:
library(pacman)
p_load(tidyverse)

# Load in the baseline scenario data:

read_csv("BFIR/BFIR_Baseline_volume.csv") %>%
  pivot_longer(!Year) %>%
  ggplot(aes(x = Year, y = value)) + geom_line()  + facet_wrap(.~name, scales = "free")

read_csv("BFIR/BFIR_Baseline_DBH.csv") %>%
  pivot_longer(!Year & !Species) %>%
  ggplot(aes(x = Year, y = value, color = name)) + geom_line() + facet_wrap(.~Species, scales = "free")

read_csv("BFIR/BFIR_Baseline_height.csv") %>%
  pivot_longer(!Year & !Species) %>%
  ggplot(aes(x = Year, y = value, color = name)) + geom_line() + facet_wrap(.~Species, scales = "free")


read_csv("BFIR/BFIR_Baseline_other.csv") %>%
  pivot_longer(!Year & !Species) %>%
  ggplot(aes(x = Year, y = value, color = Species)) + geom_line() + facet_wrap(.~name, scales = "free")
