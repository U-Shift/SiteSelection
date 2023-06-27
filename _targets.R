# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse", "sf", "stplanr", "osmdata", "sfnetworks",
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

# Replace the target list below with your own:
list(
  tar_target(
    name = CITY,
    command = select_city(CITY = "Lisboa")),
  tar_target(
    name = CITYlimit,
    command = get_citylimit(CITY)),
  tar_target(
    name = grid,
    command = make_grid(CITYlimit)),
  tar_target(
    name = road_network,
    command = get_osm(CITYlimit, CITY)),
  tar_target(
    name = road_network_clean,
    command = clean_osm(road_network, CITY)),
  tar_target(
    name = centrality_nodes,
    command = get_centrality(road_network_clean, CITY)),
  tar_target(
    name = centrality_grid,
    command = get_centrality_grid(centrality_nodes, grid)),
  tar_target(
    name = candidates_centrality,
    command = find_candidates(grid, centrality_grid, CITY)
  )
)
  
