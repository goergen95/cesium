library(sf)
library(cesium)
library(dplyr)
library(tidyr)

data <- storms  %>%
  filter(year >= 2004) %>%
  filter(month >=8 & month <=10) %>%
  mutate(
    id = paste0(name, "-", year),
    year = 2004,
    month = ifelse(month > 9, as.character(month), paste0("0", month)),
    day =  ifelse(day > 9, as.character(day), paste0("0", day)),
    hour =  ifelse(hour > 9, as.character(hour), paste0("0", hour)),
    time = as.POSIXct(paste0(year,"-",month,"-",day,"T",hour,":00:00Z"), format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) %>%
  filter(!is.na(tropicalstorm_force_diameter), !is.na(hurricane_force_diameter))

pal <- color_numeric("Blues", data$wind)

globe <- data %>%
  cesium(options = cesium_options(base_layer_picker = T, animation = T, info_box = T)) %>%
  add_points(
    layer_id = "storms",
    id_var = "id",
    time_var = "time",
    constant_space = F,
    color = ~czml_color(pal(wind), timesteps = time, interpolation = interpolation_options(algorithm = "LINEAR")),
    outline_color = czml_color("black"),
    outline_width = czml_double(2),
    size = ~czml_double(as.numeric(wind)/2, time),
    height = 1000,
    popup = ~czml_string(paste0("Speed: ", wind), time),
    interpolation = interpolation_options(algorithm = "LINEAR"))
