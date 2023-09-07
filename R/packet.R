# gets coordinates from sf objects
normalize_positions <- function(obj, height = 0){

  geom_type <- unique(as.character(st_geometry_type(obj)))
  if (length(geom_type) > 1)
    rlang::abort("Expecing only single type geometries.")

  coords <- st_coordinates(obj)
  coords <- unname(coords)

  if (geom_type == "POINT"){

    return(unname(cbind(coords, height)))

  } else if (geom_type == "LINESTRING") {

    n_features <- unique(coords[ ,3])
    if (length(height) == 1)
      height <- rep(height, length(n_features))

    return(
      lapply(n_features,
             function(i){
               cbind(coords[coords[ ,3] == i, 1:2], height[i])
             })
    )

  } else {

    n_features <- unique(coords[ ,4])
    if (length(height) == 1)
      height <- rep(height, length(n_features))

    return(
      lapply(n_features, function(i) {
        tmp <- coords[coords[ ,4] == i, ]
        lapply(unique(tmp[ ,3]), function(j){
          cbind(tmp[tmp[ ,3] == j, 1:2], height[i])
        })
      })
    )
  }
}

get_time_info <- function(timesteps) {

  availability <- c(
    min(timesteps, na.rm = TRUE),
    max(timesteps, na.rm = TRUE))

  availability <- paste(
    format(availability, .pkgenv$time_format),
    sep = "", collapse = "/")

  epoch <- format(
    timesteps[1] - as.numeric(timesteps[1]),
    .pkgenv$time_format)

  list(
    availability = availability,
    epoch = epoch,
    timesteps=timesteps)

}

# representation of a packet
czml_packet <- function(
    id = NULL,
    name = NULL,
    description = NULL,
    availability = NULL,
    properties = NULL,
    show = TRUE,
    ...) {

  if (is.null(id))
    rlang::abort("id of packet cannot be NULL")

  other_properties <- filterNULL(list(...))

  header <- filterNULL(list(
    id = id,
    name = name,
    description = description,
    availability = availability,
    properties = properties,
    show = show
  ))

  append(header, other_properties)
}

# adds document header to a collection of packets
czml_document <- function(
    id = "document",
    layer_id = NULL,
    version = "1.0",
    packets) {

  preambel <- list(id = id, name = layer_id, version = version)
  append(list(preambel), packets, after = 1)
}

# in debug mode returns the list object representing the document
# can be switched to write the czml file to the cesium data directory
# or send the JSON directly to the browser
invoke_czml <- function(
    globe,
    data,
    document,
    layer_id,
    to_disk = TRUE,
    debug = FALSE) {

  if (debug == TRUE) return(document)
  document <- jsonify::to_json(document, unbox = TRUE, numeric_dates = FALSE)

  if (to_disk) {
    document_file <- file.path(.pkgenv$outdir, paste0(layer_id, ".czml"))
    writeLines(document, document_file)
    document <- file.path("lib/cesium-data-0.0.1", paste0(layer_id, ".czml"))
  }

  invoke_method(globe, data, method = "addCZML", document, layer_id)
}

# abstract function to prepare packets
prep_packets <- function(
    data,
    args,
    layer_id,
    id_var,
    time_var,
    constant_space,
    packet_fun,
    packet_name,
    progress = FALSE) {

  unique_ids <- unique(data[[id_var]])

  if (progress) {
    if(!requireNamespace("progressr", quietly = TRUE))
      rlang::abort("please run 'install.packages('progressr')' if you want to use a progress bar.")
    factor <- 1
    n <- length(unique_ids)
    if(n > 100) factor <- round(n * 0.01)
    p <- progressr::progressor(length(unique_ids) / factor)
  }

  packets <- lapply(seq_along(unique_ids), function(e) {

    entity_data <- data$entities[[e]]
    tinfo <- NULL

    entity_args <- eval_formula(args, entity_data)
    heights <- entity_args$height
    entity <- do.call(packet_fun, entity_args)
    do_interpolate <- !is.null(entity_args$interpolation)

    if (!is.null(time_var)) {

      tinfo <- get_time_info(entity_data[[time_var]])
      entity_data <- entity_data[order(tinfo$timesteps), ]
      tinfo$timesteps <- tinfo$timesteps[order(tinfo$timesteps)]

    }

    if (constant_space) {

      positions <- normalize_positions(
        entity_data[1,],
        height = heights[1])

    } else {

      positions <- normalize_positions(
        entity_data,
        height = heights)

    }

    is_point <- !is.list(positions)
    is_polyline <- !is_point & !is.list(positions[[1]])

    if (constant_space) {

      if (is_point) {

        positions <- list(cartographicDegrees = as.vector(t(positions)))

      } else if (is_polyline) {

        positions <- list(cartographicDegrees = as.vector(t(positions[[1]])))

      } else { # polygon

        positions <- lapply(positions[[1]], function(x) list(cartographicDegrees = as.vector(t(x))))
        positions <- list(exterior = positions[[1]], holes = filterNULL(positions[-1]))

      }

    } else {

      if (is_point) {

        positions <- lapply(seq_len(nrow(positions)), function(i) positions[i, ]) # to list

        if (do_interpolate) {

          positions <- set_temporal_property(positions, tinfo$timesteps)
          positions <- c(entity_args$interpolation, cartographicDegrees =  list(positions))

        } else { # interval

          positions <- set_interval(positions, tinfo$timesteps, name = "cartographicDegrees")

        }

      } else if (is_polyline) { # polygons and polylines are not interpolatable

        positions <- lapply(positions, function(x) as.vector(t(x)))
        positions <- set_interval(positions, tinfo$timesteps, name = "cartographicDegrees")

      } else { # polygon

        positions <- lapply(positions, function(poly){
          exterior <- as.vector(t(poly[[1]]))
          holes <- poly[-1]
          holes <- lapply(holes, function(x) as.vector(t(x)))
          list(exterior=exterior, holes=holes)
        })

        exteriors <- lapply(positions, function(x) x$exterior)
        exteriors <- set_interval(exteriors, tinfo$timesteps, name = "cartographicDegrees")
        holes <- lapply(positions, function(x) x$holes)
        holes <- set_interval(holes, tinfo$timesteps, name = "cartographicDegrees")
        positions <- list(exterior = exteriors, holes = filterNULL(holes))

      }

    }

    if (is_point) {

      packet <- czml_packet(id = paste0(layer_id, "-", e),
                            name = unique_ids[e],
                            description = entity_args$popup,
                            availability = tinfo[["availability"]],
                            position = positions,
                            path = entity_args$path,
                            show = entity_args$show)
      packet <- append(packet, entity_args$add_args)
      packet[[packet_name]] <- entity

    } else {

      packet <- czml_packet(id = paste0(layer_id, "-", e),
                            name = unique_ids[e],
                            description = entity_args$popup,
                            availability = tinfo[["availability"]],
                            show = entity_args$show)
      packet <- append(packet, entity_args$add_args)

      if (is_polyline) {

        entity$positions <- positions

      } else {

        entity$positions <- positions$exterior
        if (length(positions$holes) > 0)
          entity$holes <- positions$holes

      }

      packet[[packet_name]] <- entity

    }

    if (progress)
      if (e %% factor == 0) p()

    packet

  })
  packets
}
