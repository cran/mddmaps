#' Title
#'
#' @param species A vector of mammal species name.
#' @param order A vector of mammal orders to retrive.
#' @param version The version number of the mammal diversity database.
#' @return A SpatVector object with mammal shapefiles
#' @export
#' @source Expert curated range maps of global mammal  distributions
#' aligned according mdd database of American Mamalogist Society.
#' \doi{doi:10.5281/zenodo.10806734}
#'
#' @examples
#' get_mdd_map(version = "test")
get_mdd_map <- function(species = NULL,
                          order = NULL,
                          version = "v1_3"){

  if(version == "test"){
    url_test <- "https://mdd-aligned-shp-1-0-0.fra1.digitaloceanspaces.com/test"
    if (!url_exists(url_test)) {
      return("Can't access mammal data.")
    } else {
      return("mammal data maps is online")
    }
  }

  if(!(is.null(species) || is.null(order))){
    stop("Please provide only species or order, not both!")
  }
  mddSpList <- get(paste0("mddSpList_", version))

  allOrder <- unique(mddSpList$Order)
  allSp <- unique(mddSpList$MDD_SciName)

  matchOrder <- match(order, allOrder)
  matchSp <- match(species, allSp)
  if( any(is.na( matchOrder) ) ){
    stop("Order not found. Check spell")
  }

  if( any(is.na(matchSp) ) ){
    stop("Species not found. Check spell")
  }


  if(is.null(order)){
    order = unique(mddSpList$Order[matchSp] )
  }
  if(length(order) == 27){
    shp <- get_mdd(version)
  } else {
    MapList <- Map(get_mdd_order, order, version)
    shp <- terra::vect(MapList)
  }

  if(!is.null(species)){
    shp <- terra::subset(shp, shp$sciname %in% species)
  }

  return(shp)
}
