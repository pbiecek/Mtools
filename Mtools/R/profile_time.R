#' Plot time consumed with barplots
#'
#' This function ggplots number of consumed hours of content
#'
#' @param data A list of data.frames with the 'media' format
#' @param uid an interesting id to be marked
#'
#' @return gg object
#'
#' @export
plotProfile_time <- function(data, uid) {
  tmp <- data[[uid]]

  hours <- tmp %>%
    mutate(day = substr(request_date_et, 1, 10),
           dow = wday(day, label=T)) %>%
    group_by(day, dow) %>%
    summarise(play_time = sum(elapsed_time_ms-pause_or_stop_time_ms)/1000/60/60)

  ggplot(hours, aes(as.Date(day), play_time, fill=dow)) +
    geom_bar(stat="identity") +
    theme_classic() + xlab("Date") + ylab("Consumed content [hours]") +theme(legend.position="none")+
    ggtitle(paste("Most active days\nProfile for id:",uid))
}
