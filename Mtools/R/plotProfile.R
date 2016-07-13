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
plotProfile <- function(dataWide, uid, sort.max = TRUE) {
  data <- gather(dataWide, variable, value, -id)
  if (sort.max)
    data$variable <- reorder(data$variable, data$value, max, na.rm=TRUE)

  selected <- data[data$id == uid,]
  selected$quant <- 0
  for (i in 1:nrow(selected)) {
    selected$quant[i] <-
      round(100*(mean(dataWide[,as.character(selected$variable[i])] < selected$value[i], na.rm = TRUE) +
                   mean(dataWide[,as.character(selected$variable[i])] <= selected$value[i], na.rm = TRUE))/2,1)
  }

  ggplot(data, aes(x=variable, value)) +
    geom_violin(fill="black", scale="width") +
    geom_point(data=selected, size=4, color="red") +
    geom_text(data=selected, aes(label=paste0(quant, "%")), vjust=-0.6, hjust=.5, color="blue", size=5) +
    theme_classic() + coord_flip() + xlab("") + ylab("")
}
