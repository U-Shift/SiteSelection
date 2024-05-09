# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint


# Set defaults HERE ######################
CITY_input = "Lisboa"
cellsize_input = c(400, 400)
square_input = TRUE #TRUE = squares, FALSE = hexagons
build_osm = FALSE #clean osm road network again?

# Thresholds
population_min = mean # mean or median? default: mean
degree_min = mean # mean or median? default: mean
betweeness_range = 0.4 # percentile to exclude (upper and lower) default: 0.25
closeness_range = 0.25 # value to exclude (upper and lower) default: 0.25
entropy_min = 0.35 # value to exclude (lower) default: 0.5

#########################################



# Load packages required to define the pipeline:
library(targets)
# library(crew)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "sf", "sfheaders", "stplanr", "osmdata", "sfnetworks",
               "tidygraph", "scales", "qgisprocess"), # packages that your targets need to run
  format = "rds", # default storage format
  storage = "worker",
  retrieval = "worker",
  # controller = crew_controller_local(workers = 3) #change here number of paralell process
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
    command = make_grid(CITYlimit, CITY, cellsize = cellsize_input, square = square_input)),
  tar_target(
    name = road_network,
    command = get_osm(CITYlimit, CITY)),
  tar_target(
    name = road_network_clean,
    command = clean_osm(road_network, CITY, build_osm),
    # deployment = "worker", #paralell processing
  ),
  tar_target(
    name = centrality_nodes,
    command = get_centrality(road_network_clean, CITY)),
  tar_target(
    name = centrality_grid,
    command = get_centrality_grid(centrality_nodes, grid)),
  tar_target(
    name = CITYcensus,
    command = get_census(CITY)),
  tar_target(
    name = points_transit,
    command = get_transit(CITYlimit)),
  tar_target(
    name = transit_grid,
    command = get_transit_grid(grid, points_transit)),
  tar_target(
    name = density_grid,
    command = get_density_grid(grid, CITYcensus)),
  tar_target(
    name = landuse_entropy,
    command = get_landuse(grid, CITYcensus)),
  tar_target(
    name = candidates_all,
    command = find_candidates(grid, CITY,
                              centrality_grid, density_grid, landuse_entropy, transit_grid,
                              population_min, degree_min, betweeness_range, closeness_range,
                              entropy_min)
  )
)
  
