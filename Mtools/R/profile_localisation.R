#' Plot zipcodes
#'
#' This function leafplots zipcodes
#'
#' @param data A list of data.frames with the 'media' format
#' @param uid an interesting id to be marked
#'
#' @return gg object
#'
#' @export
plotProfile_place <- function(data, uid) {
  tmp <- data[[uid]]
  tmp <- cbind(tmp, zipcode[tmp$determined_location_zip_code,])

  leaflet() %>%
    addProviderTiles("Stamen.TonerLite",
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = tmp[,c("latitude","longitude")])
}
