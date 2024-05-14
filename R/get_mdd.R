#' Function to return mammal shapefiles of Rodentia order
#' @importFrom utils download.file
#' @param dir A directory where to write the output
#' @param version The mdd version order want to download
#' @return A SpatVector object with mammal shapefiles
#' @export
#' @source Expert range maps of global mammal distributions harmonized to
#'  according mdd database from American Mammalogist society.
#'  Data storage in \doi{doi:10.5281/zenodo.10806734}
#' @examples
#' get_mdd(version = "test")
get_mdd <- function(dir = NULL, version = "v1_2"){

  if(version == "test"){
    url_test <- "https://mdd-aligned-shp-1-0-0.fra1.digitaloceanspaces.com/test"
    if (!url_exists(url_test)) {
      return("Can't access mammal data.")
    } else {
      return("mammal data maps is online")
    }
  }

  mddSpList <- get(paste0("mddSpList_",version ))
  #mddSpList <- get_mdd_list(version)
  order <- unique(mddSpList$Order)

  #### progress call

  map_mdd_progress <- function(.x = order, version, ...) {
    pb <- progress::progress_bar$new(total = length(.x), format = " [:bar] :current/:total (:percent) eta: :eta", force = TRUE)

    f <- function(...) {
      pb$tick()
      get_mdd_order(...,)
    }
    Map(f = f, .x, version )
  }

  root <- system.file(package = "mddmaps")
  filePath <- paste0(root, "/", "data/MDD_", version, ".rds")

  if(!file.exists(filePath)){
    MapList <- map_mdd_progress(order, version)
    shp <- terra::vect(MapList)
    shp <- terra::wrap(shp)
    readr::write_rds(x = shp, file = filePath, compress = "xz")
  } else{
    shp <- readr::read_rds(filePath)
  }
  if (!is.null(dir)) {
    readr::write_rds(shp, file = paste0(dir, "/MDD_", version, ".rds"), compress = "xz")
  }
  return(terra::unwrap(shp))
}
