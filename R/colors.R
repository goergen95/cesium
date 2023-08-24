# adopted from https://github.com/rstudio/leaflet/blob/main/R/colors.R
# Distributed under GPL-3 (GNU GENERAL PUBLIC
# LICENSE version 3).

col2hex <-   function(x, alpha) {
  x <- tolower(x)
  is_hex <- grepl("^#([0-9A-Fa-f]{2}){3,4}$", x)
  if (any(!is_hex)) {
    colMat <- try(col2rgb(x[!is_hex]), silent = TRUE)
    if (inherits(colMat, "try-error")){
      rlang::abort("color not convertible to hexadecimal color code.")
    }
    new_hex <- rgb(
      red=colMat[1,]/255,
      green=colMat[2,]/255,
      blue=colMat[3,]/255,
    )
    x[!is_hex] <- new_hex
  }
  if (!is.null(alpha)) x <- scales::alpha(x, alpha = alpha)
  x
}

#' Color mapping
#'
#' Conveniently maps data values (numeric or factor/character) to colors
#' according to a given palette, which can be provided in a variety of formats.
#'
#' \code{color_numeric} is a simple linear mapping from continuous numeric data
#' to an interpolated palette.
#'
#' @param palette The colors or color function that values will be mapped to
#' @param domain The possible values that can be mapped.
#'
#'   For \code{color_numeric} and \code{color_bin}, this can be a simple numeric
#'   range (e.g. \code{c(0, 100)}); \code{color_quantile} needs representative
#'   numeric data; and \code{color_factor} needs categorical data.
#'
#'   If \code{NULL}, then whenever the resulting color function is called, the
#'   \code{x} value will represent the domain. This implies that if the function
#'   is invoked multiple times, the encoding between values and colors may not
#'   be consistent; if consistency is needed, you must provide a non-\code{NULL}
#'   domain.
#' @param na_color The color to return for \code{NA} values. Note that
#'   \code{na_color = NA} is valid.
#' @param alpha Whether alpha channels should be respected or ignored. If
#'   \code{TRUE} then colors without explicit alpha information will be treated
#'   as fully opaque.
#' @param reverse Whether the colors (or color function) in \code{palette}
#'   should be used in reverse order. For example, if the default order of a
#'   palette goes from blue to green, then \code{reverse = TRUE} will result in
#'   the colors going from green to blue.
#'
#' @return A function that takes a single parameter \code{x}; when called with a
#'   vector of numbers (except for \code{color_factor}, which expects
#'   factors/characters), #RRGGBB color strings are returned (unless
#'   \code{alpha = TRUE} in which case #RRGGBBAA may also be possible).
#'
#' @export
color_numeric <- function(palette, domain, na_color = "#808080", alpha = FALSE, reverse = FALSE) {
  rng <- NULL
  if (length(domain) > 0) {
    rng <- range(domain, na.rm = TRUE)
    if (!all(is.finite(rng))) {
      stop("Wasn't able to determine range of domain")
    }
  }

  pf <- safe_palette_func(palette, na_color, alpha)

  with_color_attr("numeric", list(na_color = na_color), function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(pf(x))
    }

    if (is.null(rng)) rng <- range(x, na.rm = TRUE)

    rescaled <- scales::rescale(x, from = rng)
    if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE))
      warning("Some values were outside the color scale and will be treated as NA")

    if (reverse) {
      rescaled <- 1 - rescaled
    }
    pf(rescaled)
  })
}

# Attach an attribute colorType to a color function f so we can derive legend
# items from it
with_color_attr <- function(type, args = list(), fun) {
  structure(fun, colorType = type, colorArgs = args)
}

# domain may or may not be NULL.
# Iff domain is non-NULL, x may be NULL.
# bins is non-NULL. It may be a scalar value (# of breaks) or a set of breaks.
get_bins <- function(domain, x, bins, pretty) {
  if (is.null(domain) && is.null(x)) {
    stop("Assertion failed: domain and x can't both be NULL")
  }

  # Hard-coded bins
  if (length(bins) > 1) {
    return(bins)
  }

  if (bins < 2) {
    stop("Invalid bins value of ", bins, "; bin count must be at least 2")
  }
  if (pretty) {
    base::pretty(domain %||% x, n = bins)
  } else {
    rng <- range(domain %||% x, na.rm = TRUE)
    seq(rng[1], rng[2], length.out = bins + 1)
  }
}

#' @details \code{color_bin} also maps continuous numeric data, but performs
#'   binning based on value (see the \code{\link[base]{cut}} function). \code{color_bin}
#'   defaults for the \code{\link[base]{cut}} function are \code{include.lowest
#'   = TRUE} and \code{right = FALSE}.
#' @param bins Either a numeric vector of two or more unique cut points or a
#'   single number (greater than or equal to 2) giving the number of intervals
#'   into which the domain values are to be cut.
#' @param pretty Whether to use the function \code{\link{pretty}()} to generate
#'   the bins when the argument \code{bins} is a single number. When
#'   \code{pretty = TRUE}, the actual number of bins may not be the number of
#'   bins you specified. When \code{pretty = FALSE}, \code{\link{seq}()} is used
#'   to generate the bins and the breaks may not be "pretty".
#' @param right parameter supplied to cut. See Details
#' @rdname color_numeric
#' @export
color_bin <- function(palette, domain, bins = 7, pretty = TRUE,
                     na_color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE) {

  # domain usually needs to be explicitly provided (even if NULL) but not if
  # breaks are specified
  if (missing(domain) && length(bins) > 1) {
    domain <- NULL
  }
  autobin <- is.null(domain) && length(bins) == 1
  if (!is.null(domain))
    bins <- get_bins(domain, NULL, bins, pretty)
  num_colors <- if (length(bins) == 1) bins else length(bins) - 1
  color_func <- color_factor(palette, domain = if (!autobin) 1:num_colors,
                           na_color = na_color, alpha = alpha, reverse = reverse)
  pf <- safe_palette_func(palette, na_color, alpha)

  with_color_attr("bin", list(bins = bins, na_color = na_color), function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(pf(x))
    }
    bins_to_use <- get_bins(domain, x, bins, pretty)
    ints <- cut(x, bins_to_use, labels = FALSE, include.lowest = TRUE, right = right)
    if (any(is.na(x) != is.na(ints)))
      warning("Some values were outside the color scale and will be treated as NA")
    color_func(ints)
  })
}

#' @details \code{color_quantile} similarly bins numeric data, but via the
#'   \code{\link[stats]{quantile}} function.
#' @param n Number of equal-size quantiles desired. For more precise control,
#'   use the \code{probs} argument instead.
#' @param probs See \code{\link[stats]{quantile}}. If provided, the \code{n}
#'   argument is ignored.
#' @rdname color_numeric
#' @export
color_quantile <- function(palette, domain, n = 4,
                          probs = seq(0, 1, length.out = n + 1), na_color = "#808080", alpha = FALSE,
                          reverse = FALSE, right = FALSE) {

  if (!is.null(domain)) {
    bins <- stats::quantile(domain, probs, na.rm = TRUE, names = FALSE)
    return(with_color_attr(
      "quantile", list(probs = probs, na_color = na_color),
      color_bin(palette, domain = NULL, bins = bins, na_color = na_color,
               alpha = alpha, reverse = reverse)
    ))
  }

  # I don't have a precise understanding of how quantiles are meant to map to colors.
  # If you say probs = seq(0, 1, 0.25), which has length 5, does that map to 4 colors
  # or 5? 4, right?
  color_func <- color_factor(palette, domain = 1:(length(probs) - 1),
                           na_color = na_color, alpha = alpha, reverse = reverse)

  with_color_attr("quantile", list(probs = probs, na_color = na_color), function(x) {
    bins_to_use <- quantile(x, probs, na.rm = TRUE, names = FALSE)
    ints <- cut(x, bins_to_use, labels = FALSE, include.lowest = TRUE, right = right)
    if (any(is.na(x) != is.na(ints)))
      warning("Some values were outside the color scale and will be treated as NA")
    color_func(ints)
  })
}

# If already a factor, return the levels. Otherwise, convert to factor then
# return the levels.
calc_levels <- function(x, ordered) {
  if (is.null(x)) {
    NULL
  } else if (is.factor(x)) {
    levels(x)
  } else if (ordered) {
    unique(x)
  } else {
    sort(unique(x))
  }
}

get_levels <- function(domain, x, lvls, ordered) {
  if (!is.null(lvls))
    return(as.character(lvls))

  if (!is.null(domain)) {
    return(calc_levels(domain, ordered))
  }

  if (!is.null(x)) {
    return(calc_levels(x, ordered))
  }
}

#' @details \code{color_factor} maps factors to colors. If the palette is
#'   discrete and has a different number of colors than the number of factors,
#'   interpolation is used.
#' @param levels An alternate way of specifying levels; if specified, domain is
#'   ignored
#' @param ordered If \code{TRUE} and \code{domain} needs to be coerced to a
#'   factor, treat it as already in the correct order
#' @rdname color_numeric
#' @export
color_factor <- function(palette, domain, levels = NULL, ordered = FALSE,
                        na_color = "#808080", alpha = FALSE, reverse = FALSE) {

  # domain usually needs to be explicitly provided (even if NULL) but not if
  # levels are specified
  if (missing(domain) && !is.null(levels)) {
    domain <- NULL
  }

  if (!is.null(levels) && anyDuplicated(levels)) {
    warning("Duplicate levels detected")
    levels <- unique(levels)
  }
  lvls <- get_levels(domain, NULL, levels, ordered)

  force(palette) # palette loses scope
  with_color_attr("factor", list(na_color = na_color), function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(rep.int(na_color, length(x)))
    }

    lvls <- get_levels(domain, x, lvls, ordered)
    pf <- safe_palette_func(palette, na_color, alpha, nlevels = length(lvls) * ifelse(reverse, -1, 1))

    orig_na <- is.na(x)
    x <- match(as.character(x), lvls)
    if (any(is.na(x) != orig_na)) {
      warning("Some values were outside the color scale and will be treated as NA")
    }

    scaled <- scales::rescale(as.integer(x), from = c(1, length(lvls)))
    if (any(scaled < 0 | scaled > 1, na.rm = TRUE)) {
      warning("Some values were outside the color scale and will be treated as NA")
    }
    if (reverse) {
      scaled <- 1 - scaled
    }
    pf(scaled)
  })
}

#' @details The \code{palette} argument can be any of the following:
#' \enumerate{
#'   \item{A character vector of RGB or named colors. Examples: \code{palette()}, \code{c("#000000", "#0000FF", "#FFFFFF")}, \code{topo.colors(10)}}
#'   \item{The name of an RColorBrewer palette, e.g. \code{"BuPu"} or \code{"Greens"}.}
#'   \item{The full name of a viridis palette: \code{"viridis"}, \code{"magma"}, \code{"inferno"}, or \code{"plasma"}.}
#'   \item{A function that receives a single value between 0 and 1 and returns a color. Examples: \code{colorRamp(c("#000000", "#FFFFFF"), interpolate = "spline")}.}
#' }
#' @rdname color_numeric
#' @name color_numeric
NULL

safe_palette_func <- function(pal, na_color, alpha, nlevels = NULL) {
  to_palette_func(pal, alpha = alpha, nlevels = nlevels) %>%
    filter_rgb() %>%
    filter_zero_length() %>%
    filter_na(na_color) %>%
    filter_range()
}

# nlevels is a positive or negative integer (or integral number) indicating the
# number of levels to use for a discrete scale (i.e. factor, i.e. qualitative,
# i.e. categorical); or NULL if it is a continuous scale. A negative value means
# that the user has asked for a "reversed" palette, so pull from the tail of the
# color palette rather than from the head.
#
# (Previous versions of this code didn't have nlevels and simply interpolated
# between colors in a qualitative palette--clearly the wrong thing to do.)
to_palette_func <- function(pal, alpha, nlevels) {
  UseMethod("to_palette_func")
}

# Wrapper function for brewer.pal that deals with n < 3, plus returns maxcolors
# by default
brewer_pal <- function(palette, n = NULL) {
  if (is.null(n))
    n <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]

  # Work around the fact that if brewer.pal is passed a number smaller than 3,
  # it returns 3 colors anyway with a warning.
  #
  # It also warns if passed a number greater than maxcolors, but that's OK, we
  # want the user to see that warning.
  colors <- RColorBrewer::brewer.pal(max(3, n), palette)
  if (n == 1) {
    colors[1]
  } else if (n == 2) {
    colors[c(1, 3)]
  } else {
    colors
  }
}

# Strings are interpreted as color names, unless length is 1 and it's the name
# of an RColorBrewer palette that is marked as qualitative
to_palette_func.character <- function(pal, alpha, nlevels) {
  if (length(pal) == 1 && pal %in% row.names(RColorBrewer::brewer.pal.info)) {
    paletteInfo <- RColorBrewer::brewer.pal.info[pal, ]
    if (!is.null(nlevels)) {
      colors <- brewer_pal(pal, abs(nlevels))
    } else {
      colors <- brewer_pal(pal) # Get all colors
    }
  } else if (length(pal) == 1 && pal %in% c("viridis", "magma", "inferno", "plasma")) {
    colors <- viridis::viridis(n = 256, option = pal)
  } else {
    colors <- pal
  }

  scales::colour_ramp(colors, alpha = alpha)
}

# Accept colorRamp style matrix
to_palette_func.matrix <- function(pal, alpha, nlevels) {
  to_palette_func(rgb(pal, maxColorValue = 255), alpha = alpha)
}

# If a function, just assume it's already a function over [0-1]
to_palette_func.function <- function(pal, alpha, nlevels) {
  pal
}

#' Color previewing utility
#'
#' @param pal A color mapping function, like those returned from \code{\link{color_numeric}}, et al
#' @param values A set of values to preview colors for
#' @return An HTML-based list of the colors and values
#' @export
preview_colors <- function(pal, values) {
  heading <- htmltools::tags$code(deparse(substitute(pal)))
  subheading <- htmltools::tags$code(deparse(substitute(values)))

  htmltools::browsable(
    with(htmltools::tags, htmltools::tagList(
      head(
        style(type = "text/css",
              "table { border-spacing: 1px; }",
              "body { font-family: Helvetica; font-size: 13px; color: #444; }",
              ".swatch { width: 24px; height: 18px; }",
              ".value { padding-left: 6px; }",
              "h3 code { font-weight: normal; }"
        )
      ),
      h3("Colors:", heading, br(), "Values:", class = "subhead", subheading),
      table(
        mapply(pal(values), values, FUN = function(color, x) {
          htmltools::tagList(tr(
            td(class = "swatch", style = paste0("background-color:", color)),
            td(class = "value", format(x, digits = 5))
          ))
        })
      )
    ))
  )
}

# colorRamp(space = "Lab") throws error when called with
# zero-length input
filter_zero_length <- function(f) {
  force(f)
  function(x) {
    if (length(x) == 0) {
      character(0)
    } else {
      f(x)
    }
  }
}

# Wraps an underlying non-NA-safe function (like colorRamp).
filter_na <- function(f, na_color) {
  force(f)
  function(x) {
    results <- character(length(x))
    nas <- is.na(x)
    results[nas] <- na_color
    results[!nas] <- f(x[!nas])
    results
  }
}

# Wraps a function that may return RGB color matrix instead of rgb string.
filter_rgb <- function(f) {
  force(f)
  function(x) {
    results <- f(x)
    if (is.character(results)) {
      results
    } else if (is.matrix(results)) {
      rgb(results, maxColorValue = 255)
    } else {
      stop("Unexpected result type ", class(x)[[1]])
    }
  }
}

filter_range <- function(f) {
  force(f)
  function(x) {
    x[x < 0 | x > 1] <- NA
    f(x)
  }
}
