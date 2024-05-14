#' Function to return mammal species list avaliable by version
#' @importFrom utils read.csv
#' @param version The taxonomy version
#' @return A data.frame object with mammal species by version
#' @export
#' @source Expert range maps of global mammal distributions harmonized to
#'  three taxonomic authorities \doi{doi:10.5281/zenodo.10806734}
#' @examples
#' get_mdd_list(version = "test")
get_mdd_list <- function(version = "v1_2"){

  if(version == "test"){
    url_test <- "https://mdd-aligned-shp-1-0-0.fra1.digitaloceanspaces.com/test"
    if (!url_exists(url_test)) {
      return("Can't access mammal data.")
    } else {
      return("mammal data maps is online")
    }
  }

  if( !grepl("^v\\d_\\d?\\d$|^test$", "v1_11") ){
    stop("The version string should be v1_2 to v1_11 or test")
  }

  url <- paste0(
      "https://mdd-aligned-shp-1-0-0.fra1.digitaloceanspaces.com/mddSpList/mddSpList_",
      version,
      ".csv")

  if (!url_exists(url)) {
    return("Can't access mammal data")
  }

  mddSpList <- read.csv(url)
  return(mddSpList)
}
