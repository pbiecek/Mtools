#' Plot profile with vioplots
#'
#' This function ggplots vioplots for few variables and mark
#' interesting points on these distributions
#'
#' @param dataWide A data.frame in the wide format with columns id, variable, value
#' @param uid an interesting id to be marked
#' @param sort.max shall variables be sorted along max value?
#' @param log shall the log scale be applied?
#'
#' @return gg object
#'
#' @export
plotProfile <- function(dataWide, uid, sort.max = TRUE, log=TRUE) {
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

  max_y <- max(data$value, na.rm = TRUE)
  pl <- ggplot(data, aes(x=variable, value+log)) +
    geom_violin(fill="grey", color="grey", scale="width") +
    geom_point(data=selected, size=8, aes(color=quant), shape="I") +
    geom_text(data=selected, aes(label=paste0(quant, "%"), color=quant, y=max_y, size=quant), vjust=0.5, hjust=.5) +
    theme_classic() + coord_flip() + xlab("") + ylab("") + theme(axis.text.y = element_text(size=12), legend.position="none")+
    scale_colour_gradient(low="green", high="red", limits=c(0,100)) +
    ggtitle(paste("Profile for id:",uid))

  if (log) pl <- pl + scale_y_log10(breaks=1+10^c(0,2,4,6,8), labels=10^c(0,2,4,6,8))

  pl
}
