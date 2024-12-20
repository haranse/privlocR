
<!-- README.md is generated from README.Rmd. Please edit that file -->

# privlocR

<!-- badges: start -->

[![R-CMD-check](https://github.com/haranse/privlocR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haranse/privlocR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/haranse/privlocR/graph/badge.svg)](https://app.codecov.io/gh/haranse/privlocR)
<!-- badges: end -->

The goal of privlocR is to obtain meaningful tags for location data
obtained in research, translating a list of longitudes and latitudes to
a list of meaningful tags describing what is found near the various
locations (e.g., shop, coastline, restaurant). Researchers fitting
participants with wearables or collecting location data from smartphones
can obtain coordinates of participant location over time[^1]. However,
given the coordinates of a participant’s location at a specific time, it
would be interesting to know whether the participant is at a residential
building, a shop, in a forest, or next to the sea. privlocR uses freely
available, openly licensed data from the
[OpenStreetMap](https://www.openstreetmap.org) project[^2] to obtain
that information easily, freely, privately, and reproducibly:
**Easily**, as the package is hopefully easy to use. **Freely**, as data
from OpenStreetMap is publicly available for free. **Privately**, as no
data is not sent to any online service. **Reproducibly**, as running
privlocR on the same downloaded map files will always return the same
results, even as online map data changes. Additionally, privlocR code is
available to understand how location data was analysed.

This is all in contrast to using commercial services which provide APIs
that can obtain information about specific locations (see, for example,
the function `google_places` from the `googleway` package). These APIs
usually require setting up paid accounts with the various services,
which adds complexity and cost; data is sent to the services, which
could create privacy risks; analysis is not reproducible as online
services update their maps, and their code is not available.

## Installation

You can install the development version of privlocR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("haranse/privlocR")
```

## Example

Generating tags from a set of locations is as simple as placing an
.osm.pbf map data file, which can be downloaded from
[Geofabrik](https://download.geofabrik.de/), in a directory; selecting
the distance around each location for which to search for tags; and
running the get_close_tags function. The following example gets all
location tags 100 meters or less from the two specified locations:

``` r
library(privlocR)

# Directory that contains pbf files (the R temporary dir in this case)
mydir <- tempdir()

# Populate the directory with map file(s) downloaded from OpenStreetMap
file.copy(system.file("extdata", "tokelau.osm.pbf", package = "privlocR"), mydir)
#> [1] TRUE

# The distance around each location in which we want to search for tags
mydst = units::set_units(100, m)

# Example longitude and latitude values
long = c(-9.1979860, -9.192079)
lat = c(-171.8501176, -171.856883)

get_close_tags(mydir, long, lat, dst = mydst)
#> Re-reading with feature count reset from 52 to 29
#> Re-reading with feature count reset from 4 to 2
#> [[1]]
#> [1] "landuse_residential" "amenity_restaurant"  "natural_reef"       
#> [4] "natural_coastline"   "natural_scrub"       "tourism_hotel"      
#> [7] "leisure_park"        "building_yes"       
#> 
#> [[2]]
#> [1] "natural_reef"      "natural_coastline" "natural_water"    
#> [4] "natural_wood"      "building_yes"
```

As we can see, the data tells us that while both locations are close to
the ocean, the first is a more commercial area, around a hotel and a
restauarant, while the second is only next to some unrecognized
building.

Additional information including other ways to download map data, as
well as advice regarding performance see the full tag workflow vignette.

## Roadmap

There are additional ways to obtain meaning from such data, including
clustering (i.e., identifying that participants were at the same “place”
at time A and time B) or assessing mobility (e.g., by estimating the
distance traveled by the participant every day). Future versions are
planned to include additional analyses, providing a standard way to
extract a variety of features from location data.

## Funding

This project has received funding from the European Union’s Horizon 2020
research and innovation programme under the Marie Sklodowska-Curie grant
agreement No 101023860.

[^1]:  Of course, the project can be used with location data of any
    origin (e.g., weather patterns, animal movement, etc.)

[^2]: Other map data can be used, as long as it can be converted to
    osm.pbf.
