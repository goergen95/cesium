% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cesium.R, R/layers.R
\name{cesium_options}
\alias{cesium_options}
\alias{interpolation_options}
\alias{path_options}
\alias{point_options}
\alias{marker_options}
\alias{label_options}
\alias{line_options}
\alias{polygon_options}
\title{Options for geometries and layers}
\source{
\url{https://cesium.com/learn/cesiumjs/ref-doc/Viewer.html#.ConstructorOptions}

\url{https://github.com/AnalyticalGraphicsInc/czml-writer/wiki/}
}
\usage{
cesium_options(
  animation = FALSE,
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
  ...
)

interpolation_options(
  algorithm = c("LINEAR", "LAGRANGE", "HERMITE"),
  degree = NULL,
  forward_type = c("NONE", "HOLD", "EXTRAPOLATE"),
  forward_duration = NULL,
  backward_type = c("NONE", "HOLD", "EXTRAPOLATE"),
  backward_duration = NULL
)

path_options(
  lead_time = czml_double(),
  trail_time = czml_double(),
  width = czml_double(),
  resolution = czml_double(),
  material = czml_solid_material(NULL),
  distance_display_condition = czml_dist_display_cond()
)

point_options(
  show = TRUE,
  height_reference = czml_height_reference(),
  scale_by_distance = czml_near_far_scalar(),
  translucency_by_distance = czml_near_far_scalar(),
  distance_display_condition = czml_dist_display_cond(),
  disable_depth_test_distance = czml_double(),
  path = path_options(),
  progress = FALSE,
  debug = FALSE
)

marker_options(
  show = TRUE,
  pixel_offset = czml_cartesian2(0, 0),
  eye_offset = czml_cartesian3(0, 0, 0),
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
  debug = FALSE
)

label_options(
  show = TRUE,
  font = "30px sans-serif",
  background_padding = czml_cartesian2(7, 5),
  pixel_offset = czml_cartesian2(0, 0),
  eye_offset = czml_cartesian3(0, 0, 0),
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
  debug = FALSE
)

line_options(
  fail_material = NULL,
  shadows = czml_shadow_mode(),
  distance_display_condition = czml_dist_display_cond(),
  clamp_to_ground = FALSE,
  classification_type = czml_classification_type(),
  granularity = czml_double(pi/180),
  z_index = czml_integer(),
  progress = FALSE,
  debug = FALSE
)

polygon_options(
  show = TRUE,
  height_reference = czml_height_reference(),
  extruded_height_reference = czml_height_reference(),
  close_top = TRUE,
  close_bottom = TRUE,
  per_position_height = FALSE,
  shadows = czml_shadow_mode(),
  st_rotation = czml_double(),
  granularity = czml_double(pi/180),
  distance_display_condition = czml_dist_display_cond(),
  classification_type = czml_classification_type(),
  z_index = czml_integer(),
  progress = FALSE,
  debug = FALSE
)
}
\arguments{
\item{animation}{Logicam adds animation widget}

\item{base_layer_picker}{Logical, adds a base layer picker}

\item{fullscreen_button}{Logical, adds a full screen button}

\item{geocoder}{Logical, adds a geocoder widget}

\item{home_button}{Logical, addas a home button}

\item{info_box}{Logical, adds info box on clicks on entities}

\item{scene_mode_picker}{Logical, adds the scene mode picker widget}

\item{selection_indicator}{Logical, adds the selection indicator widget}

\item{timeline}{Logica, adds the timeline widget}

\item{navigation_help_button}{Logical, adds the navigation help button}

\item{scene_3D_only}{Logical, geometries will only be rendered in 3D to save GPU memory}

\item{sky_box}{Logical, should star background be rendered}

\item{sky_atmosphere}{Logical, should sky and atmosphere glow be rendered}

\item{target_frame_rate}{Integer, the frame rate for rendering}

\item{projection_picker}{Logical, adds the projection picker widget}

\item{...}{Other options accepted by the viewer widget}

\item{algorithm}{The interpolation algorithm to use.}

\item{degree}{The degree of interpolation as an integer value.}

\item{forward_type}{The type of extrapolation to perform when a value is
requested at a time after any available samples}

\item{forward_duration}{The amount of time to extrapolate forward before the
property becomes undefined.}

\item{backward_type}{The type of extrapolation to perform when a value is
requested at a time before any available samples.}

\item{backward_duration}{The amount of time to extrapolate backward before
the property becomes undefined.}

\item{lead_time}{time ahead of the animation time to draw the path, in seconds}

\item{trail_time}{time behind of the animation time to draw the path, in seconds}

\item{width}{the width of the path line}

\item{resolution}{maximum step size to sample the path, in seconds}

\item{material}{material to draw the path line}

\item{distance_display_condition}{toggles the visibility of an entity based
on the viewing distance, interpolatebale}

\item{show}{Logical, show the entities of this layer on the globe}

\item{height_reference}{height reference to interpret height values, not-interpolateable}

\item{scale_by_distance}{scale an entity's size based on the viewing distance, interpolateable}

\item{translucency_by_distance}{scale an entity's opacity based on the viewing distance, interpolateable}

\item{disable_depth_test_distance}{Logical, enables depth test for the
visibility of entities below the ground.}

\item{path}{optional output of \code{path_options()} to draw a path for
an entity}

\item{progress}{Logical, if TRUE progressr is used to send progress ticks.
To show a progress bar, user-side code must be wrapped with
\code{progressr::with_progress()}.}

\item{debug}{Logical, if TRUE the layer functions will output the list
of entities instead of the globe widget. used for debugging during
development and to resolve issues.}

\item{pixel_offset}{offset in pixels from the original position}

\item{eye_offset}{offset in pixels expressed from the viewers perspective
(x is right, y is up, z is in the direction of the screen)}

\item{horizontal_origin}{sets the origin in relation to the anchor position}

\item{vertical_origin}{sets the origin in relation to the anchor position}

\item{size_in_meters}{logical, interpret sizes in meters or pixels}

\item{pixel_offset_scale_by_distance}{scale the pixel offset based on the viewing distance}

\item{image_sub_region}{A rectangle measured in pixels from the bottom left
to crop an image}

\item{font}{character, sets the font for labels, expects valid CSS font
definitions}

\item{background_padding}{horizontal and vertical padding in pixels}

\item{outline_color}{czml_color value to draw the outline}

\item{outline_width}{czml_double for the outline width}

\item{fail_material}{material to use when an entitiy is below the surface}

\item{shadows}{sets if an entity recieves or casts shadows}

\item{clamp_to_ground}{logical, should the entity be clamped to the ground}

\item{classification_type}{sets if a classification affects the terrain and/or 3D tiles}

\item{granularity}{sampling distance, in radians}

\item{z_index}{integer, used to order geometries positioned on the ground}

\item{extruded_height_reference}{sets the reference for the extruded height}

\item{close_top}{logical if the top of an entity should be closed}

\item{close_bottom}{logical, if the bottom of an entity should be closed}

\item{per_position_height}{logical, if height is to be interpreted
for each vertex or for the entire entity}

\item{st_rotation}{rotation of any applied texture}
}
\value{
A list with options

A list of options
}
\description{
Use these functions to set options to fine control the generation of the
globe and the added entities
}
\section{Functions}{
\itemize{
\item \code{cesium_options()}: Options for the globe

\item \code{interpolation_options()}: Options for interpolateable properties

\item \code{path_options()}: Options for plotting paths

\item \code{point_options()}: Options for point geometries

\item \code{marker_options()}: Options for markers

\item \code{label_options()}: Options for labels

\item \code{line_options()}: Options for line geometries

\item \code{polygon_options()}: Options for polygon geometries

}}
