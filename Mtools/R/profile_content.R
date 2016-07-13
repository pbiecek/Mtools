#' Plot content consumed with barplots
#'
#' This function ggplots number of consumed hours of content per content
#'
#' @param data A list of data.frames with the 'media' format
#' @param uid an interesting id to be marked
#'
#' @return gg object
#'
#' @export
plotProfile_content <- function(data, uid) {
  tmp <- data[[uid]]

  hours <- tmp %>%
    mutate(dow = wday(substr(request_date_et, 1, 10), label=T)) %>%
    group_by(content_title, dow) %>%
    summarise(play_time = sum(elapsed_time_ms-pause_or_stop_time_ms)/1000/60/60)

  hours$content_title <- reorder(hours$content_title, hours$play_time, sum, na.rm=TRUE)
  hours <- hours[hours$content_title %in% tail(levels(hours$content_title), 25),]

  ggplot(hours, aes(content_title, play_time, fill=dow)) +
    geom_bar(stat="identity") +
    theme_classic() + theme(legend.position="none") +
    coord_flip() + xlab("") + ylab("Hours of consumed content")+
    ggtitle(paste("Top 25 contents\nProfile for id:",uid))

}
