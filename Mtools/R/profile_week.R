#' Plot content consumed during the week with barplots
#'
#' This function ggplots number of consumed hours of content per day and hour
#'
#' @param data A list of data.frames with the 'media' format
#' @param uid an interesting id to be marked
#'
#' @return gg object
#'
#' @export
plotProfile_week <- function(data, uid) {
  tmp <- data[[uid]]

  hours <- tmp %>%
    mutate(day = substr(request_time, 1, 10),
           hour = substr(request_time, 12, 13),
           dow = wday(day, label=T)) %>%
    group_by(hour, dow) %>%
    summarise(play_time = sum(elapsed_time_ms-pause_or_stop_time_ms)/1000/60/60)

  ggplot(hours, aes(as.numeric(as.character(hour)), play_time, fill=dow)) +
    geom_bar(stat="identity") +
    theme_classic() + theme(legend.position="none") +
     xlab("") + ylab("Hours of consumed content")+
    facet_grid(~dow) + scale_x_continuous(breaks = c(0,6,12,18,24)) +
    ggtitle(paste("Time during the day activity\nProfile for id:",uid))

}
