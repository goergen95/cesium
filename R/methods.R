#' Methods to manipulate a globe widget
#'
#' A number of methods to manipulate a globe widget
#'
#' @param globe a globe widget created from \code{\link{cesium}()}
#' @param x the center longitude of the new view
#' @param y the center latitude of the new view
#' @param z the center height of the new view, in meters
#' @param duration duration for the flight, in seconds
#' @param data optional data argument to derive center location,
#'   will only be used if x, y, and z are NULL. can be inherited
#'   from the globe
#' @param ... ignored
#'
#' @describeIn globe-methods Fly to a given location
#' @export
fly_to <- function(
    globe,
    x = NULL,
    y = NULL,
    z = NULL,
    duration = NULL,
    data = getGlobeData(globe),
    ...) {

  if (!is.null(data) & all(is.null(x), is.null(y), is.null(z))) {
    bbox <- st_transform(st_as_sfc(st_bbox(data)), "EPSG:4326")
    centroid <- st_centroid(bbox)
    bbox <- st_bbox(bbox)
    extent <- abs(bbox[1] - bbox[3])
    z <- as.numeric(extent * 1e5)
    destination <- c(st_coordinates(centroid), z)
  } else {
    destination <- c(x, y, z)
  }
  if (any(is.null(destination))) {
    rlang::abort("fly_to expect either a data argument or values for x,y, and z")
  }
  invoke_method(globe, data, "flyTo", destination, duration)
}


#' @describeIn globe-methods Set the view to a given location
#' @export
set_view <- function(
    globe,
    x = NULL,
    y = NULL,
    z = NULL,
    data = getGlobeData(globe),
    ...) {

  if (!is.null(data) && all(is.null(x), is.null(y), is.null(z))) {
    bbox <- st_transform(st_as_sfc(st_bbox(data)), "EPSG:4326")
    centroid <- st_centroid(bbox)
    bbox <- st_bbox(bbox)
    extent <- abs(bbox[1] - bbox[3])
    z <- as.numeric(extent * 1e5)
    destination <- c(st_coordinates(centroid), z)
  } else {
    destination <- c(x, y, z)
  }
  if (any(is.null(destination))) {
    rlang::abort("set_view expect either a data argument or values for x,y, and z")
  }
  invoke_method(globe, data, "setView", destination)
}

#' @param start_time start time of the clock, needs to be convertible to POSIXct
#' @param stop_time stop time of the clock, needs to be convertible to POSIXct
#' @param current_time end time of the clock, needs to be convertible to POSIXct
#' @param multiplier multiplier used to determine the step size when the clock ticks
#' @param clock_step sets the behavior how much time passes between ticks
#' @param clock_range sets the behavior if start_time or end_time is reached
#' @describeIn globe-methods Set the clock of the globe widget
#' @export
set_clock <- function(
    globe,
    start_time,
    stop_time,
    current_time,
    multiplier = 1,
    clock_step = c("SYSTEM_CLOCK_MULTIPLIER", "TICK_DEPENDENT", "SYSTEM_CLOCK"),
    clock_range = c( "LOOP_STOP", "UNBOUNDED", "CLAMPED"),
    data = getGlobeData(globe),
    ...) {

  times <- list(startTime = start_time, stopTime = stop_time, currentTime = current_time)
  times <- lapply(times, as.POSIXct, ... = list(format = .pkgenv$time_format))
  if (any(sapply(times, length) != 1))
    rlang::abort("start_time, stop_time, and current_time need to be of length 1 and convertible to POSIXct")

  if (!inherits(multiplier, "numeric") | length(multiplier) !=1 )
    rlang::abort("multiplier needs to be a length one numeric")
  clock_step <- match.arg(clock_step)
  clock_range <- match.arg(clock_range)

  args <- c(times, clockStep = clock_step, clockRange = clock_range, multiplier = multiplier)
  invoke_method(globe, data, "setClock", args)
}


