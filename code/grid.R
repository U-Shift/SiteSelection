

# create a grid with 500m in Lisbon
library(tidyverse)
library(sf)



MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/SiteSelection/releases/download/0.1/MUNICIPIOSgeo.Rds")) # not working ?
MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"))

CITY = "Lisboa"
CITYlimit = MUNICIPIOSgeo %>% filter(Concelho == CITY)

