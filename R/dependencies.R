cesiumBindingDependencies <- function() {
  list(
    cesium = htmltools::htmlDependency(
      name = "cesium",
      version = "1.108.0",
      package = "cesium",
      src = "htmlwidgets/lib/Cesium/",
      script = "Cesium.js",
      stylesheet = "Widgets/widgets.css"
    ),
    cesium_binding = htmltools::htmlDependency(
      name = "cesium-binding",
      version =  utils::packageVersion("cesium"),
      package = "cesium",
      src = "htmlwidgets/assets",
      script = "cesium.js",
      all_files = T
    ),
    htmltools::htmlDependency(
      name = "cesium-data",
      version =  utils::packageVersion("cesium"),
      src = .pkgenv$outdir,
      all_files = T
    )
  )
}
