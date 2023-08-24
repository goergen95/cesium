library(sf)
library(dplyr)
library(cesium)


data_loc <- "./usecases/fire-data"
dir.create(data_loc, recursive = TRUE, showWarnings = FALSE)

base_url <- "https://firms.modaps.eosdis.nasa.gov/data/country/zips/modis_%s_all_countries.zip"
years <- 2010:2020

for (year in years) {
  url <- sprintf(base_url, year)
  filename <- file.path(data_loc, basename(url))
  if (!file.exists(filename))
    download.file(url, filename)
}

serengeti <- read_sf("/vsicurl/https://github.com/mapme-initiative/mapme.biodiversity/raw/main/vignettes/assets/data/serengeti.gpkg")

zips <- list.files(data_loc, pattern = ".zip$", full.names = TRUE)

data <- lapply(zips, function(zip) {

  year <- regmatches(zip, regexpr("[0-9]{4}", zip))
  path <- file.path("modis", year, paste0("modis_", year, "_Tanzania.csv"))
  path <- file.path("/vsizip", zip, path)
  layer <- st_layers(path)$name

  tmp <- read_sf(path,
                 query = paste0("select longitude, latitude, brightness, ",
                                "acq_date, confidence from ", layer)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) |>
    st_crop(serengeti)

  index <- as.numeric(unlist(st_contains(serengeti, tmp)))

  tmp[index, ] |>
    mutate(brightness = as.numeric(brightness),
           acq_date = as.Date(acq_date, "%Y-%m-%d"),
           confidence = as.numeric(confidence)) |>
    filter(confidence > 80)
})

data <- do.call(rbind, data)
data$id <- 1:nrow(data)

# repeat points for a temporal dimension
data2 <- data
# keeps points for 3 months after detection
data2$acq_date <- data2$acq_date + 90

data <- rbind(data, data2)
rm(data2); gc()

serengeti <- st_cast(serengeti, "LINESTRING")
serengeti_p  <- st_centroid(serengeti)

pal <- color_numeric("Reds", data$brightness)

globe <- cesium(options = cesium_options(animation = T, base_layer_picker = T)) |>

  add_labels(
    layer_id = "serengeti-label",
    id_var = "NAME",
    data = serengeti_p,
    text = "Serengeti National Park",
    height = 25000) |>

  add_markers(
    layer_id = "serengeti-fires",
    data = data,
    id_var = "id",
    time_var = "acq_date",
    image_url = czml_url(
      fa_icon("fire", fill = "white")
    ),
    color = ~czml_color(pal(brightness), .8, acq_date),
    scale = 0.05,
    popup = ~paste("Date: ", acq_date, "<br>",
                   "Brightness: ", brightness),
    options = marker_options(height_reference = czml_height_reference("CLAMP_TO_GROUND"))) |>

  add_lines(
    layer_id = "serengeti-boundary",
    id_var = "NAME",
    data = serengeti,
    material = czml_dash_material(
      color = czml_color("red"),
      gap_color = czml_color("#FFFFFF", alpha = 0),
      dash_length = 8
    ),
    width = 4) |>

  fly_to(x = st_coordinates(serengeti_p)[1], y = st_coordinates(serengeti_p)[2], z = 500000, duration = 10)

globe
