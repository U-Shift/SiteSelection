SiteSelection
================

Script for a site selection - [Streets4All](https://streets4all.pt/)
Project.

### About

SiteSelection aims to find areas for a dynamic street realocation
experiment.

It selects the cell locations of a giver city or neighborhood where the
street space is more disputed by different transport modes and street
activities. SiteSelection is a full script that uses a dynamic pipeline,
and gathers and processes information on:

- Population density
- Road network centrality measures
- Traffic levels
- Public Transit demand
- POI and activities

This work is part of [Streets4All
Project](https://streets4all.tecnico.ulisboa.pt), developed at the
University of Lisbon and at the University of Coimbra, and funded by
Fundação para a Ciência e Tecnologia (PT).

## Sources

The SiteSelection package is based in Portuguese open datasets, such as
census and GTFS data.

Although it is easy to run for any location in Portugal, you may adapt
the code to run at other locations (considering you have the similar
data needed).

Data needed for other locations:

- Census data (population and buildings)
- POIs
- GTFS data
- Administrative boundaries

## Setup

### Requirements

- QGis and [`qgis_process`](https://r-spatial.github.io/qgisprocess/)
  installed and working
- [`targets`](https://books.ropensci.org/targets/) R package.
- [`siteselection`]() R package \[*under development*\].

### Change defaults

Open the `_targets.R` file and change the defaults to your needs (don’t
forget to save the file before run!):

``` r
# Set defaults HERE ######################
CITY_input = "Almada"
cellsize_input = c(200, 200)
square_input = TRUE #TRUE = squares, FALSE = hexagons
build_osm = FALSE #clean osm road network again?

# Thresholds
population_min = mean # mean or median? default: mean
degree_min = mean # mean or median? default: mean
betweeness_range = 0.4 # percentile to exclude (upper and lower) default: 0.25
closeness_range = 0.25 # value to exclude (upper and lower) default: 0.25
entropy_min = 0.35 # value to exclude (lower) default: 0.5
```

### Run the pipeline

``` r
library(targets)
tar_visnetwork(targets_only = FALSE) # or true, to hide objects
```

And you should have something like this

<img src="img/Rplot.png" width="1053" />

``` r
tar_make()
# let it run

tar_load(candidates_centrality)
mapview::mapview(candidates_centrality, zcol = "degree")
```

<img src="img/tar_result_centrality_almada.png" width="1058" />

You can also load other objects, such as **`candidates_all`**,
`candidates_centrality`, `candidates_entropy`, `candidates_traffic`,
`candidates_transit`, `candidates_density`.

When the process is not complete, you may have an error like this

``` r
tar_visnetwork()
```

<img src="img/tar_viz_werror.png" width="689" />

*Work in Progress*
