#' Distances between two zip-codes
#'
#' This function is just a copy of javascript version available on
#' https://www.geodatasource.com/developers/javascript.
#' (under LGPLv3 license).
#'
#' Official Web site: http://www.geodatasource.com
#'
#' @param code1 Zip code 1
#' @param code2 Zip code 2
#' @param unit 'M' for miles (default), 'K' for kilometers, 'N' for nautical miles
#'
#' @return Distance betwen two zip codes
#'
#' @export
distanceZIP <- function(code1, code2, unit) {
  p1 <- locations2[code1,]
  p2 <- locations2[code2,]
  distance(p1$LAT, p1$LON, p2$LAT, p2$LON, unit)
}
