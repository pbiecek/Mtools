#' Plot zip-codes
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
plotZIP <- function(codes) {
  us <- map_data("state")

  rownames(zipcode) <- zipcode$zip
  zc <- zipcode[zipcode$zip %in% codes,]

  ggplot() +
    geom_map(data=us, map=us,
             aes(x=long, y=lat, map_id=region),
             fill="#ffffff", color="#000000", size=0.15)+
    geom_point(data=zc, aes(x=longitude, y=latitude, colour=state)) +
    theme_minimal() + scale_x_continuous(limits = c(-125,-66)) +
    scale_y_continuous(limits = c(25,50)) + theme(legend.position="none") +
    xlab("") + ylab("")
}
