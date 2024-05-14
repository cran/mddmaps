#' Function to return mammal shapefiles of Rodentia order
#' @param dir A directory where to write the output
#' @param order The mammal order want to  download
#' @param version The taxonomy version
#' @return A SpatVector object with mammal shapefiles
#' @export
#' @source Expert range maps of global mammal distributions harmonised to
#'  three taxonomic authorities
#'  \doi{doi:10.5281/zenodo.10806734}
#' @examples
#' get_mdd_order(version = "test")
get_mdd_order <- function(order , version, dir = NULL){

  if(version == "test"){
    url_test <- "https://mdd-aligned-shp-1-0-0.fra1.digitaloceanspaces.com/test"
    if (!url_exists(url_test)) {
      return("Can't access mammal data.")
    } else {
      return("mammal data maps is online")
    }
  }

  #mddSpList <- get(paste0("mddSpList_", version ))
  mddSpList <- get_mdd_list(version)

  if(is.na(match(order, mddSpList$Order))){
    stop("Order does not exist! Check spell")
  }

  root <- system.file(package = "mddmaps")

  filePath <- paste0(root, "/", "data/MDD_", version, "_", order, ".rds")
  if(!file.exists(filePath)){
    url <-
      paste0(
        "https://mdd-aligned-shp-1-0-0.fra1.digitaloceanspaces.com/",
        version,
        "/",
        order,
        ".rds"
      )

    # check if the url exists
    if (!url_exists(url)) {
      return("Can't access mammal data.")
    }

    Sys.sleep(1)
    shp <- readr::read_rds(url)
    readr::write_rds(shp, file = filePath, compress = "xz")
  }
  else{
    shp <- readr::read_rds(filePath)
  }

  if (!is.null(dir)) {
    readr::write_rds(shp, file = paste0(dir, "/MDD_", version, "_", order, ".rds"), compress = "xz")
  }
  return(terra::unwrap(shp))
}
