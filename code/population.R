# aim: get population for each cell and then include information in the grid

library(tidyverse)
library(sf)

# get census data ---------------------------------------------------------
download.file("https://mapas.ine.pt/download/filesGPG/2021/BGRI21_CONT.zip", "database/BGRI_CONT.zip")
unzip("database/BGRI_CONT.zip", "BGRI21_CONT.gpkg")
file.copy(from = "BGRI21_CONT.gpkg", to = "database/BGRI21_CONT.gpkg")
file.remove("BGRI21_CONT.gpkg", "database/BGRI_CONT.zip")

CENSUSraw = st_read("database/BGRI21_CONT.gpkg")

#make centroids and select only resident population and buildings (total and exclusively residential)
CENSUSpoint = CENSUSraw |> 
  select(BGRI2021, N_INDIVIDUOS, N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_EXCLUSIV_RESID) |> 
  st_centroid(of_largest_polygon = TRUE) 
# # Error in scan(text = lst[[length(lst)]], quiet = TRUE) : 
# scan() expected 'a real', got 'ParseException:'
# In addition: Warning message:
#   st_centroid assumes attributes are constant over geometries 
# Error in (function (msg)  : ParseException: Unknown WKB type 12
# Problema com multipolygon? 

st_write(CENSUSpoint, "database/CENSUSpoint.gpkg")

# density -----------------------------------------------------------------
population
Filtrar tabela só com população residente e lat/long (pontos)
  centroids pop  
  
Ter também os edificios total, e edificios exclus residenciais

# back to grid -------------------------------------------------------------
    