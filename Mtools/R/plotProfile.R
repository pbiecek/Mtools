#' Plot profile with vioplots
#'
#' This function ggplots vioplots for few variables and mark
#' interesting points on these distributions
#'
#' @param data A data.frame in the long format with columns id, variable, value
#' @param uid an interesting id to be marked
#' @param sort.max shall variables be sorted along max value?
#'
#' @return gg object
#'
#' @export
plotProfile <- function(data, uid, sort.max = TRUE) {
  if (sort.max) {
    data$variable <- reorder(data$variable, data$value, max, na.rm=TRUE)
  }

}
