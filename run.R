#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_manifest()
targets::tar_visnetwork()
targets::tar_visnetwork(targets_only = TRUE)

targets::tar_make()
targets::tar_crew() # parallel processing stats

# targets::tar_meta(fields = error, complete_only = TRUE) # debugging

targets::tar_load(candidates_all)
mapview::mapview(candidates_all, zcol="entropy")

