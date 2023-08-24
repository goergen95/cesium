#' @param name character, name of the icon understood by \code{fontawsome::fa()}
#' @param ... other arguments passed to \code{fontawsome::fa()}.
#'
#' @describeIn globe-material Helper function to integrate fontawsome icons
#' @export
fa_icon <- function(name, ...) {
  if (!requireNamespace("fontawesome", quietly = TRUE)){
    rlang::abort("install {fontawesome} with install.packages('fontawesome') first.")
  }
  outdir <- file.path(.pkgenv$outdir, "fa-icons")
  dir.create(outdir, showWarnings = FALSE)
  filename <- file.path(outdir, paste0(name, ".png"))
  fontawesome::fa_png(name, file = filename, ...)
  file.path("lib", paste0("cesium-data-", .pkgenv$version), "fa-icons", basename(filename))
}
