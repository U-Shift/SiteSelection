#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# Check objects
targets::tar_manifest()
targets::tar_visnetwork()
targets::tar_visnetwork(targets_only = TRUE)

# Run the pipeline
targets::tar_meta_delete() # delete previous runs (clear cache)
targets::tar_make() # run the pipeline!
# targets::tar_crew() # parallel processing stats


# Load the output data
targets::tar_load(grid_all)
mapview::mapview(grid_all, zcol="all_candidate")
mapview::mapview(grid_all, zcol="score") # ranked

targets::tar_load(site_selection)
mapview::mapview(site_selection, zcol="complexity")

