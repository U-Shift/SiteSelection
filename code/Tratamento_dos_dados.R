#Tratamento dos dados

library(sf)
library(tidyverse)

grid <- st_read("C:/Users/gabri/OneDrive - Universidade de Lisboa/Drive/Thesis/Artigo Site selection/R and QGIS/gtfs-live/Grid_Final_EPB.shp")

data <- read.csv("Carris_paragens.csv")

gtfs_23h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "23:00:00" & arrival_time < "24:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_23h = n())


grid <- grid %>% 
  left_join(gtfs_23h)

plot(grid["gtfs_23h"])

library(writexl)
write_xlsx(grid,"Grid_Temporaria_GTFS_2.xlsx")


# df<- data.frame(data)
# 
# df$arrival_time <- as.numeric(df$arrival_time)

#6h -7h
gtfs_6h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "06:00:00" & arrival_time < "07:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_6h = n())


grid <- grid %>% 
  left_join(gtfs_6h)

plot(grid["gtfs_6h"])

#7h - 8h
gtfs_7h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "07:00:00" & arrival_time < "08:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_7h = n())

grid <- grid %>% 
  left_join(gtfs_7h)

plot(grid["gtfs_7h"])

#8h - 9h
gtfs_8h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "08:00:00" & arrival_time < "09:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_8h = n()) 

grid <- grid %>% 
  left_join(gtfs_8h)

plot(grid["gtfs_8h"])

#9h - 10h
gtfs_9h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "09:00:00" & arrival_time < "10:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_9h = n()) 

grid <- grid %>% 
  left_join(gtfs_9h)

plot(grid["gtfs_9h"])

#10h - 11h
gtfs_10h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "10:00:00" & arrival_time < "11:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_10h = n()) 

grid <- grid %>% 
  left_join(gtfs_10h)

plot(grid["gtfs_10h"])

#11h - 12h
gtfs_11h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "11:00:00" & arrival_time < "12:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_11h = n()) 

grid <- grid %>% 
  left_join(gtfs_11h)

plot(grid["gtfs_11h"])

#12h - 13h
gtfs_12h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "12:00:00" & arrival_time < "13:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_12h = n())

grid <- grid %>% 
  left_join(gtfs_12h)

plot(grid["gtfs_12h"])

#13h - 14h
gtfs_13h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "13:00:00" & arrival_time < "14:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_13h = n())

grid <- grid %>% 
  left_join(gtfs_13h)

plot(grid["gtfs_13h"])

#14h - 15h
gtfs_14h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "14:00:00" & arrival_time < "15:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_14h = n())

grid <- grid %>% 
  left_join(gtfs_14h)

plot(grid["gtfs_14h"])

#15h - 16h
gtfs_15h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "15:00:00" & arrival_time < "16:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_15h = n())

grid <- grid %>% 
  left_join(gtfs_15h)

plot(grid["gtfs_15h"])

#16h - 17h
gtfs_16h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "16:00:00" & arrival_time < "17:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_16h = n())

grid <- grid %>% 
  left_join(gtfs_16h)

plot(grid["gtfs_16h"])

#17h - 18h
gtfs_17h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "17:00:00" & arrival_time < "18:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_17h = n())

grid <- grid %>% 
  left_join(gtfs_17h)

plot(grid["gtfs_17h"])

#18h - 19h
gtfs_18h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "18:00:00" & arrival_time < "19:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_18h = n())

grid <- grid %>% 
  left_join(gtfs_18h)

plot(grid["gtfs_18h"])

#19h - 20h
gtfs_19h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "19:00:00" & arrival_time < "20:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_19h = n())

grid <- grid %>% 
  left_join(gtfs_19h)

plot(grid["gtfs_19h"])

#20h - 21h
gtfs_20h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "20:00:00" & arrival_time < "21:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_20h = n())

grid <- grid %>% 
  left_join(gtfs_20h)

plot(grid["gtfs_20h"])

##21h - 22h
gtfs_21h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "21:00:00" & arrival_time < "22:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_21h = n())

grid <- grid %>% 
  left_join(gtfs_21h)

plot(grid["gtfs_21h"])

#22h - 23h
gtfs_22h <- data %>% 
  as.data.frame() %>% 
  select(arrival_time, stop_lat,stop_lon) %>% 
  filter(arrival_time >= "22:00:00" & arrival_time < "23:00:00") %>% 
  st_as_sf(coords=c("x"="stop_lon","y"="stop_lat"),crs=4326) %>%
  st_join(grid %>% select(id)) %>% 
  st_drop_geometry() %>%  #tirar a geometria
  filter(!is.na(id)) %>%   #tirar os pontos que não intersectam as células
  group_by(id) %>% 
  summarize(gtfs_22h = n())

grid <- grid %>% 
  left_join(gtfs_22h)

plot(grid["gtfs_22h"])

#library(writexl)

#write_xlsx(grid,"Grid_Temporaria_GTFS.xlsx")

#Foi feito o cálculo da LS para o TP

library(readxl)
table_temporaria <- read_excel("C:/Users/gabri/OneDrive - Universidade de Lisboa/Drive/Thesis/Artigo Site selection/Grid_Final_SiteSelection/Grid_Final_SiteSelection.xlsx")  
table_temporaria <- table_temporaria[,-c(1:4,6:57)]

grid <- grid[,-c(58,59)]

grid <- grid %>%
  left_join(table_temporaria)




st_write(grid, "C:/Users/gabri/OneDrive - Universidade de Lisboa/Drive/Thesis/Artigo Site selection/Grid_Final_SiteSelection.shp")



