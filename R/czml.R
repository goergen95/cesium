# given a property value as vector or lists sets the valid intervals
# to set a valid interval at least two time steps are needed
# the last specified value is  omitted
set_interval <- function(x, timesteps = NULL, name){
  if (any(is.null(x), is.null(timesteps))) return(NULL)
  lengths <- sapply(list(x, timesteps), length)
  if (any(lengths < 2))
    rlang::abort("Cannot set intervals for sequences of length <2")
  if (length(unique(lengths)) > 1)
    rlang::abort("x and timesteps need to have equal lengths")
  n <- length(timesteps)
  index <- order(timesteps)
  x <- x[index]
  timesteps <- timesteps[index]
  if (inherits(timesteps, "POSIXct"))
    timesteps <- format(timesteps, format = .pkgenv$time_format)
  intervals <- as.list(paste(timesteps[1:(n-1)], timesteps[2:n], sep="/"))
  x <- as.list(x[1:(n-1)])

  # filter null, e.g. for polygon holes
  index_null <- sapply(x, function(y) is.null(y))
  n <- sum(!index_null)
  x <- x[!index_null]
  intervals <- intervals[!index_null]

  x <- c(rbind(intervals, x))
  names(x) <- rep(c("interval", name), length(x)/2)
  x <- split(x, ceiling(seq_along(x) / 2))
  names(x) <- NULL
  x
}


# alternates a property value given as vector or list with its timesteps
set_temporal_property <- function(x, timesteps = NULL) {
  if (is.null(timesteps)) return(x)
  if (length(timesteps) != length(x))
    rlang::abort("values and time need to be of the same length")
  index <- order(timesteps)
  x <- x[index]
  timesteps <- timesteps[index]
  if (inherits(timesteps, "POSIXct")) timesteps <- format(timesteps, format = .pkgenv$time_format)
  if (is.list(x))
    return(do.call(c, mapply(function(v, t) c(t, as.list(v)), x, timesteps, SIMPLIFY = F)))
  return(c(rbind(as.list(timesteps), as.list(x))))
}


# development helpers
simt <- function(n, f = 60) format(Sys.time() - 1:n * f, .pkgenv$time_format)
jsp <- function(x) jsonify::pretty_json(jsonify::to_json(x, unbox = TRUE))


#' CZML values
#'
#' Use these functions set CZML-confirming values for properties
#'
#' @param color A character matching an R color code or a hex-color string.
#' @param alpha A numeric between 0 and 1 indicating the opacity of the color.
#' @param timesteps A vector of timesteps equal to the length of input values
#'   for the property
#' @param interpolation If not supplied but timesteps are supplied, the values
#'   will automatically be set to intervals. Use interpolation_options() if
#'   instead you would like to interpolate the property between time steps
#'
#' @return A list.
#' @export
#' @name czml
#' @source \url{https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/}
czml_color <- function(color = "white", alpha = NULL, timesteps = NULL, interpolation) {
  if (is.null(color)) return(NULL)
  color <- col2hex(color, alpha)
  color <- grDevices::col2rgb(color, alpha = TRUE)
  if (length(color) == 4)
    return(list(rgba = as.numeric( color[ ,1])))
  if (length(unique(color)) == 2) return(list(rgba = as.numeric(color[,1])))
  if (is.null(timesteps))
    rlang::abort("More than one color specified, but no time variable is present")
  if (ncol(color) != length(timesteps))
    rlang::abort("Number of colors does not match supplied timesteps")
  if (missing(interpolation)) {
    color <- lapply(seq_len(ncol(color)), \(i) as.numeric(color[,i]))
    color <- set_interval(color, timesteps, name = "rgba")
  } else {
    color <- lapply(seq_len(ncol(color)), \(i) as.numeric(color[,i]))
    color <- set_temporal_property(color, timesteps)
    color <- c(interpolation, rgba = list(color))
  }
  color
}

#' @param string A character string
#' @export
#'
#' @name czml
czml_string <- function(string = NULL, timesteps = NULL) {
  if (is.null(string)) return(NULL)
  if (!inherits(string, "character"))
    rlang::abort("string must be of type character")
  if (length(string) == 1) return(string)
  if (length(unique(string)) == 1) return(string[1])
  if (is.null(timesteps))
    rlang::abort("More than one string specified, but no time variable is present")
  set_interval(string, timesteps, "string")
}

#' @param x Value in x direction
#' @param y Value in y direction
#' @param z Value in z direction, or height
#' @export
#' @name czml
czml_cartesian3 <- function(
    x = NULL,
    y = NULL,
    z = NULL,
    timesteps = NULL,
    interpolation) {

  if (any(is.null(x), is.null(y), is.null(z))) return(NULL)
  coords <- list(x, y, z)
  lengths <- sapply(coords, length)
  if (length(unique(lengths)) != 1)
    rlang::abort("cartesian3 values need to be supplied with equal length")
  n <- unique(lengths)
  if (n == 1) return(list(cartesian = coords))
  if (is.null(timesteps))
    rlang::abort("More than one coordinate specified, but no time variable is present")
  if (n != length(timesteps))
    rlang::abort("Number of coordinates does not match supplied timesteps")
  if (missing(interpolation)) {
    coords <- do.call(rbind, coords)
    coords <- lapply(seq_len(ncol(coords)), \(i) as.numeric(coords[,i]))
    coords <- set_interval(coords, timesteps, name = "cartesian")
  } else {
    coords <- do.call(rbind, coords)
    coords <- lapply(seq_len(ncol(coords)), \(i) as.numeric(coords[,i]))
    coords <- set_temporal_property(coords, timesteps)
    coords <- c(interpolation, cartesian = list(coords))
  }
  coords
}

#' @param width Size either in pixels or meters
#' @param height Size eiterh in pixels or meters
#' @export
#' @name czml
czml_bounding_rectangle <- function(x= NULL, y = NULL, width = NULL, height= NULL,
                                    timesteps= NULL, interpolation) {
  if (any(is.null(x), is.null(y), is.null(width), is.null(height))) return(NULL)
  bbox <- list(x, y, width, height)
  lengths <- sapply(bbox, length)
  if (length(unique(lengths)) != 1)
    rlang::abort("bounding rectangle needs to be supplied with equal length")
  n <- unique(lengths)
  if (n == 1) return(list(boundingRectangle = as.numeric(bbox)))
  if (is.null(timesteps))
    rlang::abort("More than one bounding rectangle specified, but no time variable is present")
  if (n != length(timesteps))
    rlang::abort("Number of rectangles does not match supplied timesteps")
  if (missing(interpolation)) {
    bbox <- do.call(rbind, bbox)
    bbox <- lapply(seq_len(ncol(bbox)), \(i) as.numeric(bbox[,i]))
    bbox <- set_interval(bbox, timesteps, name = "boundingRectangle")
  } else {
    bbox <- do.call(rbind, bbox)
    bbox <- lapply(seq_len(ncol(bbox)), \(i) as.numeric(bbox[,i]))
    bbox <- set_temporal_property(bbox, timesteps)
    bbox <- c(interpolation, boundingRectangle = list(bbox))
  }
  bbox
}


#' @export
#' @name czml
czml_cartesian2 <- function(x = NULL, y = NULL, timesteps = NULL, interpolation) {
  if (any(is.null(x), is.null(y))) return(NULL)
  coords <- list(x, y)
  lengths <- sapply(coords, length)
  if (length(unique(lengths)) != 1)
    rlang::abort("cartesian2 values need to be supplied with equal length")
  n <- unique(lengths)
  if (n == 1) return(list(cartesian2 = coords))
  if (is.null(timesteps))
    rlang::abort("More than one coordinate specified, but no time variable is present")
  if (n != length(timesteps))
    rlang::abort("Number of coordinates does not match supplied timesteps")
  if (missing(interpolation)) {
    coords <- do.call(rbind, coords)
    coords <- lapply(seq_len(ncol(coords)), \(i) as.numeric(coords[,i]))
    coords <- set_interval(coords, timesteps, name = "cartesian2")
  } else {
    coords <- do.call(rbind, coords)
    coords <- lapply(seq_len(ncol(coords)), \(i) as.numeric(coords[,i]))
    coords <- set_temporal_property(coords, timesteps)
    coords <- c(interpolation, cartesian2 = list(coords))
  }
  coords
}


#' @param url A charachter vector pointing to an external URL, a base64-encoded
#'    data URI, or a local file path.
#' @export
#' @name czml
czml_url <- function(url = NULL, timesteps = NULL) {
  if (is.null(url)) return(NULL)
  if (!inherits(url, "character"))
    rlang::abort("url must be of type character")
  if (length(url) == 1) return(list(uri = url))
  if (length(unique(url)) == 1) return(list(uri = url[1]))
  if (is.null(timesteps))
    rlang::abort("More than one url specified, but no time variable is present")
  set_interval(url, timesteps, "uri")
}

#' @param value A single numeric or integer value
#' @export
#' @name czml
czml_double <- function(value = NULL, timesteps = NULL, interpolation) {
  if (is.null(value)) return(NULL)
  if (!inherits(value, "numeric"))
    rlang::abort("czml_double needs to be a numeric")
  if (length(value) == 1) return(value)
  if (missing(timesteps))
    rlang::abort("More than one value specified, but no time variable is present")
  if (length(value) != length(timesteps))
    rlang::abort("Number of values does not match supplied timesteps")
  if (missing(interpolation)) {
    value <- set_interval(value, timesteps, name = "number")
  } else {
    value <- set_temporal_property(as.list(value), timesteps)
    value <- c(interpolation, number = list(value))
  }
  value
}

#' @export
#' @name czml
czml_integer <- function(value = NULL, timesteps = NULL, interpolation) {
  if (is.null(value)) return(NULL)
  if (!inherits(value, c("numeric", "integer")))
    rlang::abort("czml_integer needs to be a integer")
  if (inherits(value, "numeric")) value <- as.integer(value)
  if (length(value) == 1) return(value)
  if (missing(timesteps))
    rlang::abort("More than one value specified, but no time variable is present")
  if (length(value) != length(timesteps))
    rlang::abort("Number of values does not match supplied timesteps")
  if (missing(interpolation)) {
    value <- set_interval(value, timesteps, name = "number")
  } else {
    value <- set_temporal_property(as.list(value), timesteps)
    value <- c(interpolation, number = list(value))
  }
  value
}


#' @param near_dist The lower distance in meters
#' @param near_value The value of the property at \code{near_dist}
#' @param far_dist The upper distance in meters
#' @param far_value The value of the property at \code{far_dist}
#' @export
#' @name czml
czml_near_far_scalar <- function(near_dist = NULL, near_value = NULL,
                                 far_dist = NULL, far_value = NULL,
                                 timesteps = NULL, interpolation) {
  if (any(is.null(near_dist), is.null(near_value), is.null(far_dist), is.null(far_value))) return(NULL)
  near_far <- list(near_dist, near_value, far_dist, far_value)
  lengths <- sapply(near_far, length)
  if (length(unique(lengths)) != 1)
    rlang::abort("near_var_scalar values need to be supplied with equal length")
  n <- unique(lengths)
  if (n == 1) return(list(nearFarScalar = near_far))
  if (is.null(timesteps))
    rlang::abort("More than one coordinate specified, but no time variable is present")
  if (n != length(timesteps))
    rlang::abort("Number of coordinates does not match supplied timesteps")
  if (missing(interpolation)) {
    near_far <- do.call(rbind, near_far)
    near_far <- lapply(seq_len(ncol(near_far)), \(i) as.numeric(near_far[,i]))
    near_far <- set_interval(near_far, timesteps, name = "nearFarScalar")
  } else {
    near_far <- do.call(rbind, near_far)
    near_far <- lapply(seq_len(ncol(near_far)), \(i) as.numeric(near_far[,i]))
    near_far <- set_temporal_property(near_far, timesteps)
    near_far <- c(interpolation, nearFarScalar = list(near_far))
  }
  near_far
}


#' @export
#' @name czml
czml_dist_display_cond <- function(near_dist = NULL, far_dist = NULL,
                                   timesteps = NULL, interpolation) {
  if (any(is.null(near_dist), is.null(far_dist))) return(NULL)
  dist_disp <- list(near_dist, far_dist)
  lengths <- sapply(dist_disp, length)
  if (length(unique(lengths)) != 1)
    rlang::abort("dist_display_cond values need to be supplied with equal length")
  n <- unique(lengths)
  if (n == 1) return(list(distanceDisplayCondition = dist_disp))
  if (is.null(timesteps))
    rlang::abort("More than one coordinate specified, but no time variable is present")
  if (n != length(timesteps))
    rlang::abort("Number of coordinates does not match supplied timesteps")
  if (missing(interpolation)) {
    dist_disp <- do.call(rbind, dist_disp)
    dist_disp <- lapply(seq_len(ncol(dist_disp)), \(i) as.numeric(dist_disp[,i]))
    dist_disp <- set_interval(dist_disp, timesteps, name = "distanceDisplayCondition")
  } else {
    dist_disp <- do.call(rbind, dist_disp)
    dist_disp <- lapply(seq_len(ncol(dist_disp)), \(i) as.numeric(dist_disp[,i]))
    dist_disp <- set_temporal_property(dist_disp, timesteps)
    dist_disp <- c(interpolation, distanceDisplayCondition = list(dist_disp))
  }
  dist_disp
}

#' @param reference The value a property refers to.
#' @export
#' @name czml
czml_height_reference <- function(
    reference = c("RELATIVE_TO_GROUND", "CLAMP_TO_GROUND", "NONE")) {
  match.arg(reference)
}

#' @param origin The horizontal or vertical origin a property refers to
#' @export
#' @name czml
czml_horizontal_origin <- function(
    origin = c("CENTER", "LEFT", "RIGHT")) {
  match.arg(origin)
}

#' @export
#' @name czml
czml_vertical_origin <- function(
    origin = c("CENTER", "TOP", "BOTTOM", "BASELINE")) {
  match.arg(origin)
}

#' @param style The style with wich a label is rendered.
#' @export
#' @name czml
czml_label_style <- function(
    style = c("FILL", "OUTLINE", "FILL_AND_OUTLINE")) {
  match.arg(style)
}

#' @param orientation The orientation with which stripes are plotted
#' @export
#' @name czml
czml_stripe_orientation <- function(
    orientation = c("HORIZONTAL", "VERTICAL")) {
  match.arg(orientation)
}

#' @param arc_type The type that determines how lines are plotted.
#' @export
#' @name czml
czml_arc_type <- function(
    arc_type = c("GEODESIC", "RHUMB", "NONE")) {
  match.arg(arc_type)
}

#' @param shadow_mode The mode with which shadows are drawn to and from an entity.
#' @export
#' @name czml
czml_shadow_mode <- function(
    shadow_mode = c("DISABLED", "ENABLED", "CAST_ONLY", "RECEIVE_ONLY")) {
  match.arg(shadow_mode)
}


#' @param classification_type A value indicating if a classification has
#'   an effect on terrain and/or 3D tiles.
#' @export
#' @name czml
czml_classification_type <- function(
    classification_type = c("BOTH", "TERRAIN", "CESIUM_3D_TILE")) {
  match.arg(classification_type)
}


#' Materials to be used to draw polygons and lines and other geometries
#'
#' Note, that not all materials can be used with every geometry type
#'
#'
#' @param color a czml_color used for the main color
#' @export
#' @describeIn globe-material Solid color material
czml_solid_material <- function(color = czml_color("white")) {
  if (is.null(color)) return(NULL)
  list(solidColor = list(color = color))
}

#' @export
#' @describeIn globe-material Arrow material, lines only
czml_arrow_material <- function(
    color = czml_color("white")) {
  filterNULL(
    list(polylineArrow = list(color = color))
  )
}


#' @param alpha value to control the opacity of a color
#' @param line_count czml_cartesian2 indicating the number of lines
#'   in x and y direction
#' @param line_thickness czml_cartesian2 indicating the thickness
#'   of lines both for lines in x and y direction
#' @param line_offset czml_cartesian2 to determine the offset between
#'   lines in x and y direction
#' @export
#' @describeIn globe-material Grid material
czml_grid_material <- function(
    color = czml_color("white"),
    alpha = 0.1,
    line_count = czml_cartesian2(8,8),
    line_thickness = czml_cartesian2(1,1),
    line_offset = czml_cartesian2(0,0)) {

  filterNULL(
    list(grid = list(
      color = color,
      cellAlpha = alpha,
      lineCount = line_count,
      lineThickness = line_thickness,
      lineOffset = line_offset))
  )
}


#' @param image_url czml_url indicating an image used to fill the entity
#' @param image_repeat czml_cartesian2 indicating the number of times
#'   an image is repeated in x and y direction
#' @param image_transparent logical indicating if the image has a transparent
#'   background or not
#' @export
#' @describeIn globe-material Image material
czml_image_material <- function(
    image_url = czml_url(NULL),
    image_repeat = czml_cartesian2(1,1),
    color = czml_color("white"),
    image_transparent = FALSE) {

  if (is.null(image_url)) return(NULL)

  filterNULL(
    list(image = filterNULL(list(
      image = image_url,
      "repeat" = image_repeat,
      color = color,
      transparent = image_transparent))
    )
  )
}



#' @param orientation czml_stripe_orientation indicating the
#'   orientation used to draw stripes
#' @param even_color czml_color for even positions
#' @param odd_color czml_color for odd positions
#' @param offset_stripes czml_double indicating the offset between stripes
#' @param repeat_stripes czml_double indicating the number of times
#'   stripes are repeated
#' @export
#' @describeIn globe-material Stripe material
czml_stripe_material <- function(
    orientation = czml_stripe_orientation(),
    even_color =  czml_color("white"),
    odd_color = czml_color("black"),
    offset_stripes = czml_double(0),
    repeat_stripes = czml_double(1)) {

  filterNULL(
    list(stripe = list(
      orientation = orientation,
      evenColor = even_color,
      oddColor = odd_color,
      offset = offset_stripes,
      "repeat"  = repeat_stripes))
  )
}

#' @param repeat_checker czml_double indicating the number of times
#'   a checkerboard is repeated
#' @export
#' @describeIn globe-material Checkerboard material
czml_checkerboard_material <- function(
    even_color =  czml_color("white"),
    odd_color = czml_color("black"),
    repeat_checker = czml_cartesian2(2,2)) {

  filterNULL(
    list(checkerboard = list(
      evenColor = even_color,
      oddColor = odd_color,
      "repeat" = repeat_checker))
  )
}


#' @param outline_color czml_color for the outline
#' @param outline_width czml_double for the width of an outline
#' @export
#' @describeIn globe-material Outline material
czml_outline_material <- function(
    color = czml_color("white"),
    outline_color = czml_color("black"),
    outline_width = czml_double(1)) {

  filterNULL(
    list(polylineOutline = list(
      color = color,
      outlineColor = outline_color,
      outlineWidth = outline_width))
  )
}

#' @param glow_power czml_double value
#' @param taper_power czml_double value
#' @export
#' @describeIn globe-material Glow material, lines only
czml_glow_material <- function(
    color = czml_color("white"),
    glow_power = czml_double(0.25),
    taper_power = czml_double(1)) {

  filterNULL(
    list(polylineGlow = list(
      color = color,
      glowPower = glow_power,
      taperPower = taper_power
    ))
  )
}


#' @param gap_color czml_color to color the gaps between dashes
#' @param dash_length czml_double indicating the length of dashes
#' @param dash_pattern  czml_integer representing a 16-bit bitfield determining
#'   which portions along a single dash_length are the dash (1) and which are
#'   the gap (0)
#' @export
#' @describeIn globe-material Dash material, lines only
czml_dash_material <- function(
    color = czml_color("white"),
    gap_color =  czml_color("black"),
    dash_length = czml_double(16),
    dash_pattern = czml_integer(255L)) {

  filterNULL(
    list(polylineDash = list(
      color = color,
      gapColor = gap_color,
      dashLength = dash_length,
      dashPattern = dash_pattern
    ))
  )
}


czml_billboard <- function(
    show = TRUE,
    image_url = NULL,
    scale = NULL,
    pixel_offset = NULL,
    eye_offset = NULL,
    horizontal_origin = NULL,
    vertical_origin = NULL,
    height_reference = NULL,
    color = NULL,
    rotation = NULL,
    size_in_meters = NULL,
    image_width = NULL,
    image_height = NULL,
    scale_by_distance = NULL,
    translucency_by_distance = NULL,
    pixel_offset_scale_by_distance = NULL,
    image_sub_region = NULL,
    distance_display_condition = NULL,
    disable_depth_test_distance = NULL,
    ...){

  filterNULL(
    list(
      show = show,
      image = image_url,
      scale = scale,
      pixelOffset = pixel_offset,
      eyeOffset = eye_offset,
      horizontalOrigin = horizontal_origin,
      verticalOrigin = vertical_origin,
      heightReference = height_reference,
      color = color,
      rotation = rotation,
      sizeInMeters = size_in_meters,
      width = image_width,
      height = image_height,
      scaleByDistance = scale_by_distance,
      translucencyByDistance = translucency_by_distance,
      pixelOffsetScaleByDistance = pixel_offset_scale_by_distance,
      imageSubRegion = image_sub_region,
      distanceDisplayCondition = distance_display_condition,
      disableDepthTestDistance = disable_depth_test_distance
    )
  )
}


czml_label <- function(
    show = NULL,
    text = NULL,
    font = NULL,
    label_style = NULL,
    scale = NULL,
    show_background = NULL,
    background_color = NULL,
    background_padding = NULL,
    pixel_offset = NULL,
    eye_offset = NULL,
    horizontal_origin = NULL,
    vertical_origin = NULL,
    height_reference = NULL,
    fill_color = NULL,
    outline_color = NULL,
    outline_width = NULL,
    translucency_by_distance = NULL,
    pixel_offset_scale_by_distance = NULL,
    scale_by_distance = NULL,
    distance_display_condition = NULL,
    disable_depth_test_distance = NULL,
    ...) {

  filterNULL(
    list(
      show = show,
      text = text,
      font = font,
      style = label_style,
      scale = scale,
      showBackground = show_background,
      backgroundColor = background_color,
      backgroundPadding = background_padding,
      pixelOffset = pixel_offset,
      eyeOffset = eye_offset,
      horizontalOrigin = horizontal_origin,
      verticalOrigin = vertical_origin,
      heightReference = height_reference,
      fillColor = fill_color,
      outlineColor = outline_color,
      outlineWidth = outline_width,
      translucencyByDistance = translucency_by_distance,
      pixelOffsetScaleByDistance = pixel_offset_scale_by_distance,
      scaleByDistance =scale_by_distance,
      distanceDisplayCondition = distance_display_condition,
      disableDeptTestDistance = disable_depth_test_distance))
}

czml_polyline<- function(
    show = NULL,
    positions = NULL,
    arc_type = NULL,
    width = NULL,
    granularity = NULL,
    material = NULL,
    follow_surface = NULL,
    shadows = NULL,
    fail_material = NULL,
    distance_display_condition = NULL,
    clamp_to_ground = NULL,
    classification_type = NULL,
    z_index = NULL,
    ...) {

  filterNULL(
    list(
      show = show,
      positions = positions,
      arcType = arc_type,
      width = width,
      granularity = granularity,
      material = material,
      followSurface = follow_surface,
      shadows = shadows,
      depthFailMaterial = fail_material,
      distanceDisplayCondition = distance_display_condition,
      clampToGround = clamp_to_ground,
      classificationType = classification_type,
      zIndex = z_index))
}


czml_polygon <- function(
    show = NULL,
    positions = NULL,
    holes = NULL,
    arc_type = NULL,
    height = NULL,
    height_reference = NULL,
    extruded_height = NULL,
    extruded_height_reference = NULL,
    st_rotation = NULL,
    granularity = NULL,
    fill = NULL,
    material = NULL,
    outline = NULL,
    outline_color = NULL,
    outline_width = NULL,
    per_position_height = NULL,
    close_top = NULL,
    close_bottom = NULL,
    shadows = NULL,
    distance_display_condition = NULL,
    classification_type = NULL,
    z_index = NULL,
    ...) {

  filterNULL(
    list(
      show = show,
      positions = positions,
      height = height,
      extrudedHeight = extruded_height,
      arcType = arc_type,
      heightReference = height_reference,
      extrudedHeightReference = extruded_height_reference,
      stRotation = st_rotation,
      granularity = granularity,
      fill = fill,
      material = material,
      outline = outline,
      outlineColor = outline_color,
      outlineWidth = outline_width,
      perPositionHeight = per_position_height,
      closeTop = close_top,
      closeBottom = close_bottom,
      shadows = shadows,
      distanceDisplayContion = distance_display_condition,
      classificationType = classification_type,
      zIndex = z_index))
}


czml_rectangle <- function(
    show = NULL,
    coordinates = NULL,
    height = NULL,
    height_reference = NULL,
    extruded_height = NULL,
    extruded_height_reference = NULL,
    rotation = NULL,
    st_rotation = NULL,
    granularity = NULL,
    fill = NULL,
    material = NULL,
    outline = NULL,
    outline_color = NULL,
    outline_width = NULL,
    distance_display_condition = NULL,
    classification_type = NULL,
    z_index = NULL,
    ...) {
  filterNULL(
    list(
      show = show,
      coordinates = coordinates,
      height = height,
      heightReference = height_reference,
      extrudedHeight = extruded_height,
      extrudedHeightReference = extruded_height_reference,
      rotation = rotation,
      stRotation = st_rotation,
      granularity = granularity,
      fill = fill,
      material = material,
      outline = outline,
      outlineColor = outline_color,
      outlineWidth = outline_width,
      distanceDisplayCondition = distance_display_condition,
      classificationType = classification_type,
      zIndex = z_index
    )
  )
}


czml_path <- function(
    lead_time = NULL,
    trail_time = NULL,
    width = NULL,
    resolution = NULL,
    material = NULL,
    distance_display_condition = NULL
) {
  filterNULL(
    list(
      leadTime = lead_time,
      trailTime = trail_time,
      width = width,
      resolution = resolution,
      material = material,
      distanceDisplayCondition = distance_display_condition
    )
  )
}

