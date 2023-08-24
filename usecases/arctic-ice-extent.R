library(sf)
library(httr)
library(stars)
library(dplyr)
library(cesium)

data_loc <- "./usecases/ice-data/"
dir.create(data_loc, recursive = TRUE, showWarnings = FALSE)
base_url <- "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/shapefiles/shp_extent/"
folders <- paste(sprintf("%02d", 1:12), "_", c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), sep = "")
layers <- "extent_N_%s_polygon_v3.0.zip"

for (i in 1:12) {
  print(i)
  tmp_step <- "%s%02d"
  tmp_step <- sprintf(tmp_step, 2000:2022, i)
  urls <- paste0(base_url, folders[i], "/", sprintf(layers, tmp_step))
  filenames <- file.path(data_loc, basename(urls))
  for (j in 1:length(urls)) {
    if (!file.exists(filenames[j]))
      if (httr::HEAD(urls[j])$status_code == 200)
        download.file(urls[j], filenames[j])
  }
}

zips <- list.files(data_loc, pattern = "*.zip$", full.names = T)
data <- lapply(zips, function(zip) {
  date <- regmatches(basename(zip), regexpr("\\d{6}", basename(zip)))
  read_sf(paste0("/vsizip/", zip)) |>
    mutate(date = as.Date(paste(date, "01"), "%Y%m%d"),
           year = format(date, "%Y"),
           month = format(date, "%m"),
           id = paste0(year, "-", FID),
           area = as.numeric(st_area(geometry)) / 1e6) |>
    filter(area == max(area)) |> # only largest polygon
    st_make_valid() |>
    st_transform(st_crs(4326))
})

data <- do.call(rbind, data)

stats <- data |>
  st_drop_geometry(data) |>
  mutate(area_avg = mean(area))

data <- stats |>
  summarise(area_avg_month = mean(area), .by = month) |>
  left_join(stats, by = "month") |>
  left_join(data) |>
  select(id, FID, date, year, month, area, area_avg, area_avg_month, geometry) |>
  st_as_sf()

pal_year <- color_factor("Greens", domain = data$year)

data$id <- "Arctic Sea Ice Extent (Largest Polygon)"

globe <- cesium(
  data = data,
  options = cesium_options(animation = T, base_layer_picker = T)) |>
  add_polygons(
    layer_id = "northern-ice-extent",
    id_var = "id",
    time_var = "date",
    constant_space = F,
    material = czml_image_material(
      image_url = czml_url("https://images.pexels.com/photos/6610027/pexels-photo-6610027.jpeg")
    ),
    popup = ~czml_string(paste0(
      "Current Date: ", format(date, "%Y-%m-%d"), " <br>",
      "Current Extent: ", round(area / 1000), " 1000/km2 <br>",
      "Monthly Average Extent: ", round(area_avg_month / 1000), " 1000/km2 <br>",
      "Percent of monthly Extent: ", round(area / area_avg_month * 100), "% <br>",
      "Long-term average: ", round(area_avg / 1000), " 1000/km2 <br>",
      "Percent of long-term average: ", round(area / area_avg * 100), "%"),
      date),
    options = polygon_options(debug = F)) |>
  fly_to(x = 0, y = 90, z = 2e7, duration = 15)

globe
