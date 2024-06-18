#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_manifest()
targets::tar_visnetwork()
targets::tar_visnetwork(targets_only = TRUE)

library(dplyr)
# CENSUSpoint = sf::st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CENSUSpoint.gpkg", quiet = TRUE)
# CENSUSpop = CENSUSpoint |> 
#   sf::st_drop_geometry() |>
#   select(Concelho, N_INDIVIDUOS) |>
#   group_by(Concelho) |>
#   summarise(population = sum(N_INDIVIDUOS))
# 
# CAOPcidades = CAOP_municipios |> 
#   sf::st_drop_geometry() |>
#   mutate(Cidadona = toupper(Concelho)) |>
#   left_join(CENSUSpop, by = c("Cidadona" = "Concelho"))
# saveRDS(CAOPcidades, "inputdata/CAOPcidades.Rds")

CAOPcidades = readRDS("inputdata/CAOPcidades.Rds")
cidades = CAOPcidades |> 
  filter(population >= 25000) |> # set here desired population min
  select(Concelho) |> 
  arrange(Concelho)
cidades = cidades$Concelho

sample = c("Almada", "Viseu", "Tavira")

# clean and fresh analysis table
analysis_table_loop = readRDS("analysis/analysis_table_loop.Rds")
analysis_table_loop = analysis_table_loop[-c(1:nrow(analysis_table_loop)),]
saveRDS(analysis_table_loop, "analysis/analysis_table_loop.Rds")


# CONTINUE FROM HERE IF BREAKS --------------------------------------------

# run only remaining
already_done = readRDS("analysis/analysis_table_loop.Rds")
cidades_fwd = cidadesT |> filter(!Concelho %in% already_done$CITY)
cidades_fwd = cidades_fwd$Concelho

# run loop for all cities
#### FIRST SET THE DESIRED SETTINGS AT _targets.R ####
for (cidade in cidades_fwd) { # change for sample for testing with more than one, or cities for all
  Sys.setenv(SELECTED_CITY = cidade)
  # Sys.setenv(SELECTED_CITY = "Tavira") #test with only one case
  targets::tar_make()
  print("wait a bit")
  Sys.sleep(100) # wait 1 min before next, so we can get overpass api limits
}

analysis_table_loop = readRDS("analysis/analysis_table_loop.Rds")



targets::tar_load(grid_all)
mapview::mapview(grid_all, zcol="all_candidate")
mapview::mapview(grid_all, zcol="score") #ranked

targets::tar_load(site_selection)
mapview::mapview(site_selection, zcol="complexity")

