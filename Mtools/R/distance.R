#' Distances between two geo-coordinates
#'
#' This function is just a copy of javascript version available on
#' https://www.geodatasource.com/developers/javascript.
#' (under LGPLv3 license).
#'
#' Official Web site: http://www.geodatasource.com
#'
#' @param lat1 Latitude and Longitude of point 1 (in decimal degrees)
#' @param lon1 Latitude and Longitude of point 1 (in decimal degrees)
#' @param lat2 Latitude and Longitude of point 2 (in decimal degrees)
#' @param lon2 Latitude and Longitude of point 2 (in decimal degrees)
#' @param unit 'M' for miles (default), 'K' for kilometers, 'N' for nautical miles
#'
#' @return Distance betwen two points
#'
#' @export
distance <- function(lat1, lon1, lat2, lon2, unit) {
  radlat1 <- pi * lat1/180
  radlat2 <- pi * lat2/180
  theta <- lon1 - lon2
  radtheta = pi * theta/180
  dist <- sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(radtheta);
  dist <- acos(dist)
  dist <- dist * 180/pi
  dist <- dist * 60 * 1.1515
  if (unit=="K") { dist <- dist * 1.609344 }
  if (unit=="N") { dist <- dist * 0.8684 }
  dist
}
