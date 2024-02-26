# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint


# Set defaults HERE ######################
CITY_input = "Almada"
cellsize_input = c(200, 200)
square_input = TRUE #TRUE = squares, FALSE = hexagons
build_osm = FALSE #clean osm road network again?

#########################################



# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "sf", "sfheaders", "stplanr", "osmdata", "sfnetworks",
               "tidygraph", "scales", "qgisprocess"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)



# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("R/functions.R")
# source("other_functions.R") # Source other scripts as needed. # nolint


# Create outuput data
if(!file.exists("outputdata")){
  dir.create("outputdata")
}


# Replace the target list below with your own:
list(
  tar_target(
    name = CITY,
    command = select_city(CITY = CITY_input)),
  tar_target(
    name = CITYlimit,
    command = get_citylimit(CITY)),
  tar_target(
    name = grid,
    command = make_grid(CITYlimit, cellsize = cellsize_input, square = square_input)),
  tar_target(
    name = road_network,
    command = get_osm(CITYlimit, CITY)),
  tar_target(
    name = road_network_clean,
    command = clean_osm(road_network, CITY, build_osm)),
  tar_target(
    name = centrality_nodes,
    command = get_centrality(road_network_clean, CITY)),
  tar_target(
    name = centrality_grid,
    command = get_centrality_grid(centrality_nodes, grid)),
  tar_target(
    name = CENSUScity,
    command = get_census(CITY)),
  tar_target(
    name = density_grid,
    command = get_density_grid(grid, CENSUScity)),
  tar_target(
    name = candidates_all,
    command = find_candidates(grid, centrality_grid, density_grid, CITY)
  )
)
  
