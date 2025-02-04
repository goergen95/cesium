cesium
================

[![R-CMD-check](https://github.com/goergen95/cesium/workflows/R-CMD-check/badge.svg)](https://github.com/goergen95/cesium/actions)
[![License](https://img.shields.io/badge/License-GPL%20(%3E=3)-brightgreen.svg?style=flat)](https://choosealicense.com/licenses/gpl-3.0/)

## Installation

You can install the development version of `cesium` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("goergen95/cesium")
```

## Usecases

Visit the
[usecases](https://github.com/goergen95/cesium/tree/main/usecases)
directory on GitHub to explore applications of the `cesium` package.

### NOAA - Arctic Sea Ice Extent 2000 - 2022

<figure>
<img src="./usecases/assets/arctic-sea-ice.gif"
alt="Animation of arctic sea ice extent" />
<figcaption aria-hidden="true">Animation of arctic sea ice
extent</figcaption>
</figure>

### Global Forest Watch - Deforestation in Rondonia, Brazil 2000 - 2020

<figure>
<img src="./usecases/assets/rondonia-deforestation.gif"
alt="Animation of forest loss in Rondonia, Brazil" />
<figcaption aria-hidden="true">Animation of forest loss in Rondonia,
Brazil</figcaption>
</figure>

### NASA FIRMS - Fires in Serengeti National Park, Tanzania 2010 - 2020

<figure>
<img src="./usecases/assets/serengeti-fires.gif"
alt="Animation of fire detections in the Serengeti National Park, Tanzania" />
<figcaption aria-hidden="true">Animation of fire detections in the
Serengeti National Park, Tanzania</figcaption>
</figure>

### NOAA - Atlantic hurricanes 2004 - 2021 timelapse

<figure>
<img src="./usecases/assets/hurricanes.gif"
alt="Animation of atlantic hurricanes from the NOAA database 2004 - 2021" />
<figcaption aria-hidden="true">Animation of atlantic hurricanes from the
NOAA database 2004 - 2021</figcaption>
</figure>

## Disclaimer

This package is highly experimental, very far from being feature-equal
compared with other mapping tools available to the R community and very
likely to include breaking-changes in the foreseeable future. In other
words, it needs your help! If you would like to be able to use
[`CesiumJS`](https://cesium.com/platform/cesiumjs/) capabilities from
within R, please consider contributing to the development of this
package. You can report bugs, open feature-requests or contribute
pull-requests to improve the documentation or the code base. We have a
[Code-of-Conduct](https://github.com/goergen95/cesium/blob/main/CODE_OF_CONDUCT.md)
governing the process of contributing to this repository. By
participating in this project you agree to abide by its terms.

## Getting started

If you are familiar with the
[`leaflet`](http://rstudio.github.io/leaflet/) package, you will quickly
be able to use `cesium` as well. In fact, behind the scenes, `cesium`
uses the very same design patterns that the authors of leaflet did. One
distinctive feature of `cesium`, which probably represents the most
difficult part in using it, is the inclusion of time as another
dimension for mapping spatial data. It uses
[`CesiumJS`](https://cesium.com/platform/cesiumjs/), developed by AGI, a
Javascript library for mapping spatio-temporal data on an interactive 3D
globe in the browser. While it also can be used to map data which is
constant in time, it shows its real strength when the properties of your
data change over time.

To achieve this, the authors of `CesiumJS` came up with a `JSON` based
file format called
[`CZML`](https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/CZML-Guide).
The cesium R package, in its essence, is a translator between sf objects
to `CZML`. This already takes us a long way, because we can use `CZML`
to describe very different types of geometries in a way that `CesiumJS`
can render these geometries and their properties dynamically.

To enable you to use the package efficiently, let’s briefly discuss some
of the basic building blocks.

### Basic concepts

The most basic concept in using `cesium` is that of an entity. An entity
is a digital representation of an object located in space and time. When
your data does not change over time, you could think of an entity as a
single row in your `sf` object, where certain measurements, or
properties in the `CesiumJS` lingua, are associated with a specific
location in space. This becomes a little bit more complicated when your
data has a temporal component. Now, either the entity might change its
location in space over time, or it might change certain properties over
time, or both. To complicate it even further, if you have a collection
of entities that vary over time, more often than not they are not
sampled at the exact same time steps. You end up with a time series
where potentially both the location and properties of the entities vary
irregularly over time.

``` r
library(sf)
```

    ## Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE

``` r
data <- data.frame(
  ID = c(1,1,2,2),
  M = runif(4),
  time = c(Sys.time() - 0:1 * 30,  Sys.time() - 1:2 * 15),
  geom = st_as_sfc(apply(cbind(runif(4, -180, 180), runif(4, -90, 90)), 1, st_point, simplify = F))
)

(data <- st_as_sf(data, crs = st_crs("EPSG:4326")))
```

    ## Simple feature collection with 4 features and 3 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -89.42126 ymin: -55.35812 xmax: 31.43983 ymax: 72.71347
    ## Geodetic CRS:  WGS 84
    ##   ID         M                time                    geometry
    ## 1  1 0.8582765 2023-10-11 19:28:49   POINT (19.19777 39.66742)
    ## 2  1 0.7900004 2023-10-11 19:28:19   POINT (25.57935 72.71347)
    ## 3  2 0.1460654 2023-10-11 19:28:34   POINT (31.43983 5.546985)
    ## 4  2 0.5349906 2023-10-11 19:28:19 POINT (-89.42126 -55.35812)

To accommodate such irregular time series as the one above, we use `sf`
as the digital representation for such objects. If your data varies over
time, `cesium` expects you to supply a variable which identifies all
observations that belong to a given entity. In the case of the data
shown above, you would be required to set `id_var = "ID"`. Additionally,
you need to supply the variable that identifies the time of each
observation. In the case above, that would translate to
`time_var = "time"`.

As you can see by inspecting the `geometry` column, the two entities
change their location over time. `cesium` does not check if an entity
changes its location, but instead offers an argument indicating whether
or not to treat space as constant or dynamic. In case of constant space,
the default, the first location is used for all subsequent observations
of an entity. For the data above, we thus would have to set
`constant_space = FALSE`, explicitly.

In case of time-dynamic entities, the question arises how certain
properties should be treated *between* the sampled time steps. `cesium`
let’s you decide between two different possibilities:

- intervals: The default behavior in `cesium`. In this case, a property
  is only valid for an interval between the actual observation and the
  following one. This means that the value of a property changes
  abruptly once the respective interval is no longer valid. You do not
  have to do anything special to enable intervals for time-tagged
  properties, since this is the default behavior.

- interpolation: In this case, properties are interpolated for the time
  between observations. **Note**, that interpolation can only be done
  for numerical properties. It **cannot** be sensually done for things
  like a text label or an image. You can use `interpolation_options()`
  in most of the `czml_*()` functions to fine-control the interpolation
  behavior.

### Implementation & Limitations

Currently, `cesium` provides basic support to plot the following items:

- point geometries enabled by `add_points()` and `point_options()`
- markers enabled by `add_markers()` and `marker_options()`
- labels enabled by `add_labels()` and `label_options()`
- line geometries enabled by `add_lines()` and `line_options()`
- polygon geometries enabled by `add_polygons()` and `polygon_options()`
- raster image time series enabled by `add_raster()`

Most arguments should be set using the respective `czml_*()` function.
Please consider the help page if in doubt. If your data has no temporal
dimension to interpolate over or you wish to hold certain properties
constant, you might supply single values that are then applied for all
time steps.

In case you want to vary a property over time, you can use the formula
notation and additionally supply the column where the time steps are
found. **Note**, that different from `leaflet`, formulas are resolved on
the entity level. If you wish to set a property based on a global
statistic, you will need to pre-compute it.

Following is a list of features that most probably are required for a
productive usage of `cesium` but currently are not implemented:

- conversion methods for `terra` and other spatial classes
- support for `sf` geometries of type `MULTI*`
- mechanism to supply base maps
- layer manager enabling toggling visibility of layers
- legends for colors, sizes, etc.
- support for custom TMS/WMS servers
- support for custom terrain providers
- R Markdown and shiny support

Please contact the maintainers of the package if you would like to
support the development of these or other items.

### Acknowledgments

On the R-side, `cesium` borrows heavily from the `leaflet` package for
the internal workings of the code and the color mechanisms. Without the
people driving the development of `CesiumJS` and the `CZML` format, such
a thin wrapper as `cesium` would have no chance to go that far.

## Licence

This package is licensed to you under the terms of the GNU General
Public License version 3 or later.

Copyright 2023 Darius A. Görgen
