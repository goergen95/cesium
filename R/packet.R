# gets coordinates from sf objects
normalize_positions <- function(obj, height = 0){
  if (nrow(obj) > 1)
    rlang::abort("expected to retrieve positions only for single entities")

  geom_type <- as.character(st_geometry_type(obj))
  coords <- st_coordinates(obj)

  get_polygons <- function(x) {
    n <- unique(x[ ,3])
    rings <- lapply(n, function(i) {
      index <- x[ ,3] == i
      c(rbind(x[index,1], x[index,2], height))
    })
    exterior <- rings[[1]]
    holes <- NULL
    if(length(n) > 1) holes <- rings[2:length(rings)]
    return(list(exterior = exterior, holes = holes))
  }

  switch(
    geom_type,
    "POINT" = c(rbind(coords[1], coords[2], height)),
    "LINESTRING" = c(rbind(coords[ ,1], coords[ ,2], height)),
    "POLYGON" = get_polygons(coords),
    rlang::abort(
      paste("normalize_positions() not implemented for geometries of type",
            geom_type))
  )
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
    availability <- NULL
    timesteps <- NULL

    entity_args <- eval_formula(args, entity_data)
    heights <- entity_args$height
    entity <- do.call(packet_fun, entity_args)

    if (!is.null(time_var)) {

      timesteps <- entity_data[[time_var]]

      availability <- c(
        min(timesteps, na.rm = TRUE),
        max(timesteps, na.rm = TRUE))

      availability <- paste(
        format(availability, .pkgenv$time_format),
        sep = "", collapse = "/")

      entity_data <- entity_data[order(timesteps), ]
      timesteps <- timesteps[order(timesteps)]
      timesteps <- format(timesteps, .pkgenv$time_format)

    }

    if (constant_space) {

      positions <- normalize_positions(
        entity_data[1,],
        height = heights[1])

      n <- length(positions[1])

      if (n == 1) {

      positions <- list(cartographicDegrees = positions)

      } else {

        positions <- lapply(positions, function(x) {
            filterNULL(list(cartographicDegrees = x))})
      }

    } else {

      if (length(heights) != nrow(entity_data))
        heights <- rep(heights, nrow(entity_data))

      positions <- lapply(
        seq_len(nrow(entity_data)), \(i)
        normalize_positions(entity_data[i, ],
                            height = heights[i]))

      n <- length(positions[1])

      if (n == 1) { # points and lines

        if (is.null(entity_args$interpolation)) {
          positions <- set_interval(positions, timesteps, "cartographicDegrees")
        } else {
          positions <- set_temporal_property(positions, timesteps)
          positions <- list(cartographicDegrees = positions)
          positions <- c(entity_args$interpolation, positions)
        }

      } else { # polygons, potentially with holes, positions are not interpolatable

        positions <- sapply(1:n, function(i) {
          tmp <- lapply(positions, function(x) x[[i]])
          filterNULL(set_interval(tmp, timesteps, name = "cartographicDegrees"))
        }, simplify = FALSE, USE.NAMES = TRUE)
        names(positions) <- c("exterior", "holes")
      }
    }

    if (!packet_name %in% c("polyline", "polygon")) {

      packet <- czml_packet(id = paste0(layer_id, "-", e),
                            name = unique_ids[e],
                            description = entity_args$popup,
                            availability = availability,
                            position = positions,
                            path = entity_args$path,
                            show = entity_args$show)
      packet <- append(packet, entity_args$add_args)
      packet[[packet_name]] <- entity

    } else {

      packet <- czml_packet(id = paste0(layer_id, "-", e),
                            name = unique_ids[e],
                            description = entity_args$popup,
                            availability = availability,
                            show = entity_args$show)
      packet <- append(packet, entity_args$add_args)

      if (packet_name != "polygon") {

        entity$positions <- positions

      } else {

        entity$positions <- positions$exterior
        if (length(positions$holes) > 0)
          entity$holes <- positions$holes

      }

      packet[[packet_name]] <- entity
    }

    if (progress) {
      if (e %% factor == 0) p()
    }
    packet
  })
  packets
}
