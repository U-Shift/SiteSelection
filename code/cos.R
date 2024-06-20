# COS ofr city agglomrtes

library(sf)
library(dplyr)

COS_zip = "https://geo2.dgterritorio.gov.pt/cos/COS2018/COS2018v2-gpkg.zip" # 572 MB
download.file(COS_zip, destfile = "database/cos.zip")
unzip(zipfile = "database/cos.zip", exdir = "database/")
file.remove("database/cos.zip")

COSoficial = st_read(list.files("database", pattern = "COS", full.names = TRUE)[1])

table(COSoficial$COS18n3_L)
table(COSoficial$COS18n2_L)

COSurbano = COSoficial |> 
  filter(COSoficial$COS18n2_C == "1.1") |> # tecido edificado
  select(ID, Area_ha, geom)

mapview::mapview(COSurbano)
