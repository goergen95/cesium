# adopted from https://github.com/rstudio/leaflet/blob/main/R/leaflet.R
# Distributed under GPL-3 (GNU GENERAL PUBLIC
# LICENSE version 3).

#' Create a Cesium globe widget
#'
#' Creates a globe widget using \pkg{htmlwidgets}.
#' The data argument needs only to be specified if you want
#' to refer to this object in later add_layers calls. It needs to
#' be an sf object with either POINT, LINESTRING or POLYGON geometries.
#'
#' @import htmlwidgets
#' @param data an sf object
#' @param width the width of the globe widget
#' @param height the height of the globe widget
#' @param padding the padding of the globe widget
#' @param options the globe options
#' @param elementId Use an explicit element ID for the widget
#'   (rather than an automatically generated one).
#' @param sizingPolicy htmlwidgets sizing policy object. Defaults to \code{cesiumSizingPolicy()}
#' @return A HTML widget object
#' @export
cesium <- function(data = NULL, width = NULL, height = NULL,
                   padding = 0, options = cesium_options(),
                   elementId = NULL, sizingPolicy = cesium_sizing_policy(padding = padding)) {

  globe <- htmlwidgets::createWidget(
    "cesium",
    structure(
      list(options = options),
      cesiumData = data
    ),
    width = width, height = height,
    sizingPolicy = sizingPolicy,
    preRenderHook = function(widget) {
      if (!is.null(widget$jsHooks$render)) {
        widget$jsHooks$render <- lapply(widget$jsHooks$render, function(hook) {
          if (is.list(hook)) {
            hook$code <- sprintf(hookWrapperTemplate, paste(hook$code, collapse = "\n"))
          } else if (is.character(hook)) {
            hook <- sprintf(hookWrapperTemplate, paste(hook, collapse = "\n"))
          } else {
            stop("Unknown hook class ", class(hook))
          }
          hook
        })
      }
      widget
    },
    elementId = elementId,
    dependencies = cesiumBindingDependencies()
  )
  globe
}



#' @param animation Logicam adds animation widget
#' @param base_layer_picker Logical, adds a base layer picker
#' @param fullscreen_button Logical, adds a full screen button
#' @param geocoder Logical, adds a geocoder widget
#' @param home_button Logical, addas a home button
#' @param info_box Logical, adds info box on clicks on entities
#' @param scene_mode_picker Logical, adds the scene mode picker widget
#' @param selection_indicator Logical, adds the selection indicator widget
#' @param timeline Logica, adds the timeline widget
#' @param navigation_help_button Logical, adds the navigation help button
#' @param scene_3D_only Logical, geometries will only be rendered in 3D to save GPU memory
#' @param sky_box Logical, should star background be rendered
#' @param sky_atmosphere Logical, should sky and atmosphere glow be rendered
#' @param target_frame_rate Integer, the frame rate for rendering
#' @param projection_picker Logical, adds the projection picker widget
#' @param ... Other options accepted by the viewer widget
#'
#' @return A list with options
#' @source \url{https://cesium.com/learn/cesiumjs/ref-doc/Viewer.html#.ConstructorOptions}
#' @export
#' @describeIn globe-options Options for the globe
cesium_options <- function(animation = FALSE,
                           base_layer_picker = FALSE,
                           fullscreen_button = FALSE,
                           geocoder = FALSE,
                           home_button = TRUE,
                           info_box = TRUE,
                           scene_mode_picker = TRUE,
                           selection_indicator = FALSE,
                           timeline = TRUE,
                           navigation_help_button = FALSE,
                           scene_3D_only = FALSE,
                           sky_box = FALSE,
                           sky_atmosphere = FALSE,
                           target_frame_rate = 100,
                           projection_picker = TRUE,
                           ...) {
  filterNULL(
    list(
      animation = animation,
      baseLayerPicker = base_layer_picker,
      fullscreenButton = fullscreen_button,
      geocoder = geocoder,
      homeButton = home_button,
      infoBox = info_box,
      sceneModePicker = scene_mode_picker,
      selectionIndicator = selection_indicator,
      timeline = timeline,
      navigationHelpButton = navigation_help_button,
      scene3DOnly = scene_3D_only,
      skyBox = sky_box,
      skyAtmosphere = sky_atmosphere,
      targetFrameRate = target_frame_rate,
      projectionPicker = projection_picker,
      ...
    )
  )
}

cesium_sizing_policy <- function(defaultWidth = "100%",
                                 defaultHeight = 400,
                                 padding = 0,
                                 browser.fill = TRUE,
                                 ...) {
  htmlwidgets::sizingPolicy(
    defaultWidth = defaultWidth,
    defaultHeight = defaultHeight,
    padding = padding,
    browser.fill = browser.fill,
    ...
  )
}

hookWrapperTemplate <- "function(el, x, data) {
  return (%s).call(this.getglobe(), el, x, data);
}"


getGlobeData <- function(globe) {
  attr(globe$x, "cesiumData", exact = TRUE)
}


filterNULL = function (x) {
  if (length(x) == 0 || !is.list(x))
    return(x)
  x[!unlist(lapply(x, is.null))]
}


cesiumOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'cesium', width, height, package = 'cesium')
}


renderCesium <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, cesiumOutput, env, quoted = TRUE)
}
