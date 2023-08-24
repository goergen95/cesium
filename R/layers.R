# common sanity checks for input
health_checks <- function(data, layer_id, id_var, constant_space, time_var, height) {
  if (is.null(layer_id))
    rlang::abort("cannot add a layer without layer_id specified")
  if (is.null(id_var))
    rlang::abort("id_var needs to be set")
  if (!is.logical(constant_space))
    rlang::abort("constant_space must be a logical")
  if (!constant_space && is.null(time_var))
    rlang::abort("constant_space was set to FALSE, but time_var is NULL")
  if (!inherits(data, "sf"))
    rlang::abort("data needs to be an sf object")
  if (!sf::st_is_longlat(data))
    rlang::abort("data object is not in geographic coordinates.")
  if (length(unique(sf::st_geometry_type(data))) != 1)
    rlang::abort("data object has mixed geometry types")
  if (is.null(height))
    height <- 0
  if (!is.null(time_var)) {
    time <- try(
      data[[time_var]] <- as.POSIXct(
        data[[time_var]], format = .pkgenv$time_format, origin = "1970-01-01"),
      silent = TRUE)
    if (inherits(time, "try-error") || any(is.na(time))) {
      rlang::abort("time_var is not convertible to POSIXct")
    } else {
      data[[time_var]] <- time
    }
  }
  data <- tidyr::nest(data, entities = tidyr::everything(), .by = id_var)
  invisible(data)
}


#' Options for geometries and layers
#'
#' Use these functions to set options to fine control the generation of the
#' globe and the added entities
#'
#' @param algorithm The interpolation algorithm to use.
#' @param degree The degree of interpolation as an integer value.
#' @param forward_type The type of extrapolation to perform when a value is
#'   requested at a time after any available samples
#' @param forward_duration The amount of time to extrapolate forward before the
#'   property becomes undefined.
#' @param backward_type The type of extrapolation to perform when a value is
#'   requested at a time before any available samples.
#' @param backward_duration The amount of time to extrapolate backward before
#'   the property becomes undefined.
#'
#' @return A list of options
#' @export
#' @source \url{https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/}
#' @describeIn globe-options Options for interpolateable properties
interpolation_options <- function(
    algorithm = c("LINEAR", "LAGRANGE", "HERMITE"),
    degree = NULL,
    forward_type = c("NONE", "HOLD", "EXTRAPOLATE"),
    forward_duration = NULL,
    backward_type = c("NONE", "HOLD", "EXTRAPOLATE"),
    backward_duration = NULL
) {

  opts <- filterNULL(list(
    algorithm = match.arg(algorithm),
    degree = degree,
    forward_type = match.arg(forward_type),
    forward_duration = forward_duration,
    backward_type = match.arg(backward_type),
    backward_duration = backward_duration
  ))

  if (length(opts) == 0) return(NULL)
  opts
}


#' @param lead_time time ahead of the animation time to draw the path, in seconds
#' @param trail_time time behind of the animation time to draw the path, in seconds
#' @param width the width of the path line
#' @param resolution maximum step size to sample the path, in seconds
#' @param material material to draw the path line
#' @export
#' @describeIn globe-options Options for plotting paths
path_options <- function(
    lead_time = czml_double(),
    trail_time = czml_double(),
    width = czml_double(),
    resolution = czml_double(),
    material = czml_solid_material(NULL),
    distance_display_condition = czml_dist_display_cond()) {

  args <- filterNULL(
    list(
      leadTime = lead_time,
      trailTime = trail_time,
      width = width,
      resolution = resolution,
      material = material,
      distanceDisplayCondition = distance_display_condition))
  if(length(args) == 0) return(NULL)
  args
}


czml_point <- function(
    show = TRUE,
    size = NULL,
    height_reference = NULL,
    color = NULL,
    outline_color = NULL,
    outline_width = NULL,
    scale_by_distance = NULL,
    translucency_by_distance = NULL,
    distance_display_condition = NULL,
    disable_depth_test_distance = NULL,
    ...
) {

  filterNULL(
    list(
      show = show,
      pixelSize = size,
      heightReference = height_reference,
      color = color,
      outlineColor = outline_color,
      outlineWidth = outline_width,
      scaleByDistance = scale_by_distance,
      translucencyByDistance = translucency_by_distance,
      distanceDisplayCondition = distance_display_condition,
      disableDepthTestDistance = disable_depth_test_distance
    )
  )
}

#' @param show Logical, show the entities of this layer on the globe
#' @param height_reference height reference to interpret height values, not-interpolateable
#' @param scale_by_distance scale an entity's size based on the viewing distance, interpolateable
#' @param translucency_by_distance scale an entity's opacity based on the viewing distance, interpolateable
#' @param distance_display_condition toggles the visibility of an entity based
#'   on the viewing distance, interpolatebale
#' @param disable_depth_test_distance Logical, enables depth test for the
#'   visibility of entities below the ground.
#' @param path optional output of \code{path_options()} to draw a path for
#'   an entity
#' @param progress Logical, if TRUE progressr is used to send progress ticks.
#'   To show a progress bar, user-side code must be wrapped with
#'   \code{progressr::with_progress()}.
#' @param debug Logical, if TRUE the layer functions will output the list
#'   of entities instead of the globe widget. used for debugging during
#'   development and to resolve issues.
#' @export
#' @describeIn globe-options Options for point geometries
point_options <- function(
    show = TRUE,
    height_reference = czml_height_reference(),
    scale_by_distance = czml_near_far_scalar(),
    translucency_by_distance = czml_near_far_scalar(),
    distance_display_condition = czml_dist_display_cond(),
    disable_depth_test_distance = czml_double(),
    path = path_options(),
    progress = FALSE,
    debug = FALSE) {

  filterNULL(
    list(
      show = show,
      height_reference = height_reference,
      scale_by_distance = scale_by_distance,
      translucency_by_distance = translucency_by_distance,
      distance_display_condition = distance_display_condition,
      disable_depth_test_distance = disable_depth_test_distance,
      path = path,
      progress = progress,
      debug = debug)
  )
}


#' Layers and geometries
#'
#' Add layers to a globe accepting sf objects as input. A unique layer_id
#' has to be specified. Entities are identified by a column in the sf object
#' and can optionally vary over time. Properties are set with the respective
#' \code{czml_*()} functions and can be interpolated over time by supplying
#' formulas. Note, if a formula is supplied, the values will be resolved
#' based on the entity level if a time_var was specified. If you want
#' to set the property of an entitiy based on a data-set wide statistic you
#' will need to pre-compute it.
#'
#' @param globe the globe to modify
#' @param layer_id character used to name the layer, mandatory.
#' @param id_var character identifying a column in data which identifies
#'   entities, mandatory.
#' @param time_var character vector identifying a column with timestemps
#'   Must be convertible to POSIXct. Optional.
#' @param constant_space Logical, assume space to be constant over time, defaults
#'   to TRUE.
#' @param color a color value
#' @param size a value to set the size of an entity
#' @param outline_color the color to draw the outline of an entity
#' @param outline_width the width of the outline of an entity
#' @param height A numeric expressing the height of entities in meters. If a
#'   single value is supplied, it will be used for all entities. Can be a
#'   vector with the same length as the number of rows in the data object.
#'   If time_var is present, supply a formula determining the height of an
#'   entity over its validity
#' @param popup A character string to be used in the info box of an entity. Can
#'   be any valid HTML. Use \code{czml_string()} if you want it to vary over time.
#' @param options List of options supplied by the respective options function.
#' @param interpolation List of options for the interpolation of the spatial
#'   position supplied by \code{interpolation_options()}.
#' @param data An sf object, either inherited from the globe object or
#'   supplied to the respective function
#' @param ... Additional arguments passed on to the entity packets as a list
#' @describeIn globe-layers Adds point geometries to a globe
#' @return A modified globe widget
#' @export
add_points <- function(globe,
                       layer_id = NULL,
                       id_var = NULL,
                       time_var = NULL,
                       constant_space = TRUE,
                       color = czml_color("white"),
                       size = czml_double(1),
                       outline_color = czml_color("black"),
                       outline_width = czml_double(1),
                       height = 0,
                       popup = czml_string(NULL),
                       options = point_options(),
                       interpolation = interpolation_options(),
                       data = getGlobeData(globe),
                       ...) {

  data <- health_checks(data, layer_id, id_var, constant_space, time_var, height)
  n_entities <- nrow(data)

  add_args <- list(...)

  args <- filterNULL(
    c(list(
      color = color,
      size = size,
      outline_color = outline_color,
      outline_width = outline_width,
      height = height,
      popup = popup
    ),
    options,
    list(interpolation = interpolation),
    list(add_args = add_args)))

  packets <- prep_packets(
    data,
    args,
    layer_id,
    id_var,
    time_var,
    constant_space,
    packet_fun = czml_point,
    "point",
    progress = options$progress)

  document <- czml_document(layer_id = layer_id, packets = packets)
  invoke_czml(globe, data, document, layer_id, to_disk = FALSE, debug = options$debug)
}

#' @param pixel_offset offset in pixels from the original position
#' @param eye_offset offset in pixels expressed from the viewers perspective
#'   (x is right, y is up, z is in the direction of the screen)
#' @param horizontal_origin sets the origin in relation to the anchor position
#' @param vertical_origin sets the origin in relation to the anchor position
#' @param size_in_meters logical, interpret sizes in meters or pixels
#' @param pixel_offset_scale_by_distance scale the pixel offset based on the viewing distance
#' @param image_sub_region A rectangle measured in pixels from the bottom left
#'   to crop an image
#' @export
#' @describeIn globe-options Options for markers
marker_options = function(
    show = TRUE,
    pixel_offset = czml_cartesian2(0,0),
    eye_offset = czml_cartesian3(0,0,0),
    horizontal_origin = czml_horizontal_origin(),
    vertical_origin = czml_vertical_origin(),
    height_reference = czml_height_reference(),
    size_in_meters = FALSE,
    path = path_options(),
    scale_by_distance = czml_near_far_scalar(),
    translucency_by_distance = czml_near_far_scalar(),
    pixel_offset_scale_by_distance = czml_near_far_scalar(),
    image_sub_region = czml_bounding_rectangle(),
    distance_display_condition = czml_dist_display_cond(),
    disable_depth_test_distance = czml_double(),
    progress = FALSE,
    debug = FALSE) {

  filterNULL(
    list(
      show = show,
      pixel_offset = pixel_offset,
      eye_offset = eye_offset,
      horizontal_origin = horizontal_origin,
      vertical_origin = vertical_origin,
      height_reference = height_reference,
      size_in_meters = size_in_meters,
      path = path,
      scale_by_distance = scale_by_distance,
      translucency_by_distance = translucency_by_distance,
      pixel_offset_scale_by_distance = pixel_offset_scale_by_distance,
      image_sub_region = image_sub_region,
      distance_display_condition = distance_display_condition,
      disable_depth_test_distance = disable_depth_test_distance,
      progress = progress,
      debug = debug))
}



#' @param image_url character, either the URL to a web resource, a local
#'   file path, or a base64-encoded data URI
#' @param scale scales the size of an entity, values greater 1 enlarge it,
#'   values smaller than 1 shrink it
#' @param rotation rotation of the entity in radians
#' @param image_width image width in pixels
#' @param image_height image height in pixels
#' @export
#' @describeIn globe-layers Adds markers to a globe
add_markers <- function(globe,
                        layer_id = NULL,
                        id_var = NULL,
                        time_var = NULL,
                        constant_space = TRUE,
                        image_url = czml_url(),
                        scale = czml_double(1),
                        color = czml_color("white"),
                        rotation = czml_double(0),
                        height = 0,
                        image_width = czml_double(),
                        image_height = czml_double(),
                        popup = czml_string(),
                        options = marker_options(),
                        interpolation = interpolation_options(),
                        data = getGlobeData(globe),
                        ...) {


  data <- health_checks(data, layer_id, id_var, constant_space, time_var, height)
  n_entities <- nrow(data)

  add_args <- list(...)

  args <- filterNULL(
    c(list(
      image_url = image_url,
      scale = scale,
      color = color,
      rotation = rotation,
      image_width = image_width,
      image_height = image_height,
      height = height,
      popup = popup
    ),
    options,
    list(interpolation = interpolation),
    list(add_args = add_args)))

  packets <- prep_packets(
    data,
    args,
    layer_id,
    id_var,
    time_var,
    constant_space,
    packet_fun = czml_billboard,
    "billboard",
    progress = options$progress)

  document <- czml_document(layer_id = layer_id, packets = packets)
  invoke_czml(globe, data, document, layer_id, to_disk = FALSE, debug = options$debug)

}


#' @param font character, sets the font for labels, expects valid CSS font
#'   definitions
#' @param background_padding horizontal and vertical padding in pixels
#' @param outline_color czml_color value to draw the outline
#' @param outline_width czml_double for the outline width
#' @export
#' @describeIn globe-options Options for labels
label_options <- function(
    show = TRUE,
    font = "30px sans-serif",
    background_padding = czml_cartesian2(7, 5),
    pixel_offset = czml_cartesian2(0,0),
    eye_offset = czml_cartesian3(0,0,0),
    horizontal_origin = czml_horizontal_origin(),
    vertical_origin = czml_vertical_origin(),
    height_reference = czml_height_reference(),
    outline_color = czml_color("black"),
    outline_width = czml_double(1),
    path = path_options(),
    translucency_by_distance = czml_near_far_scalar(),
    pixel_offset_scale_by_distance = czml_near_far_scalar(),
    scale_by_distance = czml_near_far_scalar(),
    disable_depth_test_distance = czml_double(),
    distance_display_condition = czml_dist_display_cond(),
    progress = FALSE,
    debug = FALSE) {

  filterNULL(
    list(
      show = show,
      font = font,
      background_padding = background_padding,
      pixel_offset = pixel_offset,
      eye_offset = eye_offset,
      horizontal_origin = horizontal_origin,
      vertical_origin = vertical_origin,
      height_reference = height_reference,
      outline_color = outline_color,
      outline_width = outline_width,
      path = path,
      translucency_by_distance = translucency_by_distance,
      pixel_offset_scale_by_distance = pixel_offset_scale_by_distance,
      scale_by_distance = scale_by_distance,
      disable_depth_test_distance = disable_depth_test_distance,
      distance_display_condition = distance_display_condition,
      progress = progress,
      debug = debug))
}


#' @param text character, used as labels
#' @param label_style the style to fill the background of a label
#' @param background_color a color value for the background
#' @param show_background logical, if the background is filled
#' @param fill_color a color value for the label
#' @export
#' @describeIn globe-layers Adds labels to a globe
add_labels <- function(
    globe,
    layer_id = NULL,
    id_var = NULL,
    time_var = NULL,
    constant_space = TRUE,
    text = czml_string(),
    scale = czml_double(1),
    label_style = czml_label_style(),
    background_color = czml_color("#2a2a2aCC"),
    show_background = FALSE,
    fill_color = czml_color("white"),
    height = 0,
    popup = czml_string(),
    options = label_options(),
    interpolation = interpolation_options(),
    data = getGlobeData(globe),
    ...) {


  data <- health_checks(data, layer_id, id_var, constant_space, time_var, height)
  n_entities <- nrow(data)

  add_args <- list(...)

  args <- filterNULL(
    c(list(
      text = text,
      scale = scale,
      label_style = label_style,
      background_color = background_color,
      show_background = show_background,
      fill_color = fill_color,
      height = height,
      popup = popup
    ),
    options,
    list(interpolation = interpolation),
    list(add_args = add_args)))

  packets <- prep_packets(
    data,
    args,
    layer_id,
    id_var,
    time_var,
    constant_space,
    packet_fun = czml_label,
    "label",
    progress = options$progress)

  document <- czml_document(layer_id = layer_id, packets = packets)
  invoke_czml(globe, data, document, layer_id, to_disk = FALSE, debug = options$debug)

}



#' @param fail_material material to use when an entitiy is below the surface
#' @param shadows sets if an entity recieves or casts shadows
#' @param clamp_to_ground logical, should the entity be clamped to the ground
#' @param classification_type sets if a classification affects the terrain and/or 3D tiles
#' @param granularity sampling distance, in radians
#' @param z_index integer, used to order geometries positioned on the ground
#' @export
#' @describeIn globe-options Options for line geometries
line_options <- function(
    fail_material = NULL,
    shadows = czml_shadow_mode(),
    distance_display_condition = czml_dist_display_cond(),
    clamp_to_ground = FALSE,
    classification_type =  czml_classification_type(),
    granularity = czml_double(pi / 180),
    z_index = czml_integer(),
    progress = FALSE,
    debug = FALSE) {

  filterNULL(
    list(
      fail_material = fail_material,
      shadows = shadows,
      distance_display_condition = distance_display_condition,
      clamp_to_ground = clamp_to_ground,
      classification_type = classification_type,
      granularity = granularity,
      z_index = z_index,
      progress = progress,
      debug = debug))
}


#' @param width a czml_double determining the width
#' @param material a czml_material used to render an entity
#' @param arc_type the type of arc used to draw lines
#' @export
#' @describeIn globe-layers Adds lines to a globe
add_lines <- function(
    globe,
    layer_id = NULL,
    id_var = NULL,
    time_var = NULL,
    constant_space = TRUE,
    material = czml_solid_material(),
    arc_type = czml_arc_type(),
    width = czml_double(1),
    height = 0,
    popup = NULL,
    options = line_options(),
    interpolation = interpolation_options(),
    data = getGlobeData(globe),
    ...) {


  data <- health_checks(data, layer_id, id_var, constant_space, time_var, height)
  n_entities <- nrow(data)

  add_args <- list(...)

  args <- filterNULL(
    c(list(
      material = material,
      arc_type = arc_type,
      width = width,
      height = height,
      popup = popup
    ),
    options,
    list(interpolation = interpolation),
    list(add_args = add_args)))

  packets <- prep_packets(
    data,
    args,
    layer_id,
    id_var,
    time_var,
    constant_space,
    packet_fun = czml_polyline,
    "polyline",
    progress = options$progress)

  document <- czml_document(layer_id = layer_id, packets = packets)
  invoke_czml(globe, data, document, layer_id, to_disk = FALSE, debug = options$debug)

}

#' @param extruded_height_reference sets the reference for the extruded height
#' @param close_top logical if the top of an entity should be closed
#' @param close_bottom logical, if the bottom of an entity should be closed
#' @param per_position_height logical, if height is to be interpreted
#'   for each vertex or for the entire entity
#' @param st_rotation rotation of any applied texture
#' @export
#' @describeIn globe-options Options for polygon geometries
polygon_options <- function(
    show = TRUE,
    height_reference = czml_height_reference(),
    extruded_height_reference = czml_height_reference(),
    close_top = TRUE,
    close_bottom = TRUE,
    per_position_height = FALSE,
    shadows = czml_shadow_mode(),
    st_rotation = czml_double(),
    granularity = czml_double(pi / 180.0),
    distance_display_condition = czml_dist_display_cond(),
    classification_type = czml_classification_type(),
    z_index = czml_integer(),
    progress = FALSE,
    debug = FALSE) {

  list(
    show = show,
    height_reference = height_reference,
    extruded_height_reference = extruded_height_reference,
    close_top = close_top,
    close_bottom = close_bottom,
    per_position_height = per_position_height,
    shadows = shadows,
    st_rotation = st_rotation,
    granularity = granularity,
    distance_display_condition = distance_display_condition,
    classification_type = classification_type,
    z_index = z_index,
    progress = progress,
    debug = debug)
}

#' @param fill logical, if the entity should be filled
#' @param outline logical, if the outline should be drawn
#' @param extruded_height similar to height, indicating the extruded
#'   height of an entity based on its lower bound or in relation to the
#'   ground, depending on the value of extruded_height_reference
#' @export
#' @describeIn globe-layers Adds labels to a globe
add_polygons <- function(
    globe,
    layer_id = NULL,
    id_var = NULL,
    time_var = NULL,
    constant_space = TRUE,
    material = czml_solid_material(),
    outline_color = czml_color("black"),
    outline_width = czml_double(1),
    fill = TRUE,
    outline = TRUE,
    height = 0.0,
    extruded_height = 0.0,
    arc_type = czml_arc_type(),
    popup = NULL,
    options = polygon_options(),
    interpolation = interpolation_options(),
    data = getGlobeData(globe),
    ...) {

  data <- health_checks(data, layer_id, id_var, constant_space, time_var, height)
  n_entities <- nrow(data)

  add_args <- list(...)

  args <- filterNULL(
    c(list(
      material = material,
      outline_color = outline_color,
      outline_width = outline_width,
      fill = fill,
      outline = outline,
      height = height,
      extruded_height = extruded_height,
      arc_type = arc_type,
      popup = popup
    ),
    options,
    list(interpolation = interpolation),
    list(add_args = add_args)))

  packets <- prep_packets(
    data,
    args,
    layer_id,
    id_var,
    time_var,
    constant_space,
    packet_fun = czml_polygon,
    "polygon",
    progress = options$progress)

  document <- czml_document(layer_id = layer_id, packets = packets)
  invoke_czml(globe, data, document, layer_id, to_disk = FALSE, debug = options$debug)
}


#' @param what A numeric of character vector of length 1 or free to select the
#'   attributes of the stars data object for plotting. If length is 1, the
#'   the values will be mapped to a color scale based on the what is specified
#'   in the colors argument. If length is 3, an RGB image is plotted and the
#'   order of the elements specified which attrbibutes of the data object will
#'   be mapped to which color channel
#' @param colors Either a function accepting pixel values and returning a color
#'   code or the name of a color paletta to be used to color the raster image.
#' @param probs see \code{link{stars::st_rgb()}}, used only for RGB images
#' @param stretch see \code{link{stars::st_rgb()}} used only for RGB images
#' @export
#' @describeIn globe-layers Adds raster images to the globe
add_raster <- function(
    globe,
    layer_id = NULL,
    what = NULL,
    colors = "Spectral",
    probs = c(0.05, 0.95),
    stretch = "percent",
    data = getGlobeData(globe)) {

  stopifnot(inherits(data, "stars"))
  dimensions <- stars::st_dimensions(data)

  if (length(dimensions) > 3 || length(setdiff(names(dimensions), c("x", "y", "time"))) > 0 ) {
    rlang::abort("expecting 'data' object to have dimensions [x,y,time]")
  }

  if (!st_is_longlat(data)) {
    rlang::warn("expecting 'data' object to be in geographic coordinates. attempting to warp...")
    data <- st_warp(data, crs = st_crs("EPSG:4326"))
  }

  if (!length(what) %in%  c(1,3)) {
    rlang::abort("expecting 'what' to be of length one for single band raster image or length 3 for RGB images.")
  }

  is_factor <- is.factor(data[[1]])
  is_rgb <- length(what) == 3

  if (is_rgb) { # plot rgb
    data <- st_redimension(data[what], name = "cesium-rgb")
    data <- st_rgb(data, dimension = "cesium-rgb", probs=probs, stretch=stretch)
  } else {
    data <- data[what]
  }

  if (!is.function(colors)) {
    if (is_factor) {
      colors <- color_factor(colors, domain = NULL, na_color = "#00000000", alpha = TRUE)
    } else {
      colors <- color_numeric(colors, domain = NULL, na_color = "#00000000", alpha = TRUE)
    }
  }

  bbox <- as.numeric(st_bbox(data))
  timesteps <- st_get_dimension_values(data, "time")
  timesteps <- format(timesteps, format = .pkgenv$time_format)

  images <- lapply(seq_along(timesteps), function(i) {
    if (is_rgb) {
      rgba <- as.raw(col2rgb(as.vector(data[,,,i][[1]]), alpha = TRUE))
    } else {
      rgba <- as.numeric(as.vector(data[,,,i])[[1]])
      rgba <- as.raw(col2rgb(colors(rgba), alpha = TRUE))
    }
    dim(rgba) <- c(4, nrow(data), ncol(data))
    rgba
  })

  filenames <- vector(length = length(timesteps))
  for (i in seq_along(timesteps)){
    filename <- file.path(.pkgenv$outdir, paste0(layer_id, "-", i, ".png"))
    png::writePNG(images[[i]], target = filename)
    filenames[i] <- filename
  }

  filenames <- file.path(
    "lib", paste0("cesium-data-", utils::packageVersion("cesium")),
    basename(filenames))

  raster_rectangle <- czml_rectangle(
    show = TRUE,
    coordinates = list(wsenDegrees = bbox),
    material = czml_image_material(
      image_url =  czml_url(filenames, timesteps)
    )
  )

  packet <- czml_packet(id = layer_id,
                        name = layer_id,
                        fill = TRUE,
                        availability = paste(c(min(timesteps), max(timesteps)), collapse = "/"),
                        show = TRUE,
                        rectangle = raster_rectangle)

  document <- czml_document(layer_id = layer_id, packets = list(packet))
  invoke_czml(globe, data, document, layer_id, to_disk = FALSE, debug = F)
}
