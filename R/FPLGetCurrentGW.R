FPLGetCurrentGW <- function() {

  resp <- GET("https://fantasy.premierleague.com/api/bootstrap-static/")
  stop_for_status(resp)

  dat <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  events <- data.table::as.data.table(dat$events)

  Output <- list(
    current = events[is_current == TRUE, id],
    `next` = events[is_next == TRUE, id],
    finished = events[finished == TRUE, max(id, na.rm = TRUE)]
  )

  return(Output)
}
