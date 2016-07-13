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
  tmp <- dat[[uid]]

  hours <- tmp %>%
    mutate(day = substr(request_date_et, 1, 10),
           dow = wday(day, label=T)) %>%
    group_by(day, dow,user_id_at_time_of_access) %>%
    summarise(play_time = sum(elapsed_time_ms-pause_or_stop_time_ms)/1000/60/60)

  ggplot(hours, aes(as.Date(day), play_time, fill=user_id_at_time_of_access)) +
    geom_bar(stat="identity") + theme(legend.position="none")
}
