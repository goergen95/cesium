.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname,pkgname) {
 cesium_dir <- file.path(tempdir(), "cesium-data")
 dir.create(cesium_dir, showWarnings = FALSE)
 .pkgenv$outdir <- cesium_dir
 .pkgenv$epoch <- "1970-01-01T00:00:00Z"
 .pkgenv$time_format <- "%Y-%m-%dT%H:%M:%SZ"
 .pkgenv$version <- utils::packageVersion("cesium")
 .pkgenv$cesium_version <- "1.108.0"
}
