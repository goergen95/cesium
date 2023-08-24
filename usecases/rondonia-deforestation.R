library(sf)
library(stars)
library(cesium)
library(rnaturalearth)
library(mapme.biodiversity)
require("rnaturalearthhires")

data_loc <- "./usecases/gfw-data"
dir.create(data_loc, showWarnings = FALSE)

aoi <- ne_states("Brazil", returnclass = "sf")[ ,"name"]
aoi <- aoi[aoi$name == "Rondônia", ] |> st_cast("POLYGON")
aoi$value <- 1

aoi <- init_portfolio(aoi, years = 2000:2020, outdir = data_loc, add_resources = F) |>
  get_resources(resources = c("gfw_lossyear", "gfw_treecover"))

# pre-process GFW data to a coarser resolution suitable for plotting
resolution <- 0.005 # ~ 500 meter at the equator
tifs <- list.files(data_loc, "*.tif$", recursive = T, full.names = T)
lossyear <- grep("lossyear", tifs, value = TRUE)
treecover <- grep("treecover", tifs, value = TRUE)

treecover <- lapply(treecover, read_stars)
treecover <- do.call(st_mosaic, treecover)
treecover <- st_crop(treecover, aoi)

treecover <- st_warp(
  treecover,
  crs = st_crs(4326),
  cellsize = resolution,
  use_gdal = TRUE,
  method = "bilinear",
  no_data_value = 255)

lossyear <- lapply(lossyear, read_stars)
lossyear <- do.call(st_mosaic, lossyear)
lossyear <- st_crop(lossyear, aoi)

lossyear <- st_warp(
  lossyear,
  crs = st_crs(4326),
  cellsize = resolution,
  use_gdal = TRUE,
  method = "med",
  no_data_value = 255)

# mask rasters to the extent of aoi
dummy <- treecover
dummy[[1]] <- NA
aoi_stars <- st_rasterize(aoi[, "value"], template = dummy)
treecover[is.na(aoi_stars)] <- NA
lossyear[is.na(aoi_stars)] <- NA

# create yearly forest cover maps
treecover[treecover < 1] <- 1
names(treecover) <-  paste0("treecover")

treecover_timeseries <- lapply(2000:2020, function(year) {
  if (year == 2000) {
    return(treecover)
  } else {
    losses <- !((lossyear < (year - 2000)) & lossyear != 0)
    return(treecover * losses)
  }
})

treecover_timeseries$along <- "time"
treecover_timeseries <- do.call(c, treecover_timeseries)
treecover_timeseries <- st_set_dimensions(
  treecover_timeseries, "time",
  seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"))

# forest loss since 2000 in red, forest cover in greens
pal_greens <- color_numeric("Greens", domain = 1:100)
greens <- pal_greens(1:100)
pal <- color_numeric(
  palette = c("#ef090d", greens),
  domain = c(0:100),
  na_color = NA)

globe <- cesium(options = cesium_options(animation = TRUE, base_layer_picker = T)) |>
  add_raster(
    data = treecover_timeseries,
    layer_id = "gfw-treecover",
    what = "treecover",
    colors = pal)

globe
