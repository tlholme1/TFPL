library(httr)
library(jsonlite)

# --- helper: get live stats for a gameweek ---
get_gw_live_stats <- function(gameweek) {
  url <- paste0("https://fantasy.premierleague.com/api/event/", gameweek, "/live/")
  gw_data <- fromJSON(content(GET(url), as = "text", encoding = "UTF-8"))

  # returns dataframe with player_id, goals, assists, clean_sheets
  elements <- gw_data$elements
  stats <- data.frame(
    player_id = sapply(elements, function(x) x$id),
    goals = sapply(elements, function(x) x$stats$goals_scored),
    assists = sapply(elements, function(x) x$stats$assists),
    clean_sheets = sapply(elements, function(x) x$stats$clean_sheets)
  )

  return(stats)
}

# --- helper: get a manager’s picks for a GW ---
get_manager_picks <- function(entry_id, gameweek) {
  url <- paste0("https://fantasy.premierleague.com/api/entry/", entry_id, "/event/", gameweek, "/picks/")
  data <- fromJSON(content(GET(url), as = "text", encoding = "UTF-8"))
  return(data$picks$element)
}

# --- compute totals for each manager in a league, for a single GW ---
league_gw_totals <- function(league_id, gameweek) {
  league_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/",
                       league_id, "/standings/")
  league <- fromJSON(content(GET(league_url), as = "text", encoding = "UTF-8"))
  managers <- league$standings$results$entry

  stats <- get_gw_live_stats(gameweek)

  results <- data.frame(manager = character(),
                        goals = numeric(),
                        assists = numeric(),
                        clean_sheets = numeric())

  for (m in managers) {
    picks <- get_manager_picks(m, gameweek)
    subset_stats <- stats[stats$player_id %in% picks, ]

    goals <- sum(subset_stats$goals, na.rm = TRUE)
    assists <- sum(subset_stats$assists, na.rm = TRUE)
    clean_sheets <- sum(subset_stats$clean_sheets, na.rm = TRUE)

    results <- rbind(results, data.frame(manager = m,
                                         goals = goals,
                                         assists = assists,
                                         clean_sheets = clean_sheets))
  }

  return(results)
}

# --- compute totals across a range of gameweeks ---
league_range_totals <- function(league_id, start_gw, end_gw) {
  results <- data.frame(manager = character(),
                        goals = numeric(),
                        assists = numeric(),
                        clean_sheets = numeric())

  for (gw in start_gw:end_gw) {
    gw_results <- league_gw_totals(league_id, gw)

    if (nrow(results) == 0) {
      results <- gw_results
    } else {
      results <- merge(results, gw_results, by = "manager", all = TRUE, suffixes = c("", ".new"))
      results$goals <- rowSums(cbind(results$goals, results$goals.new), na.rm = TRUE)
      results$assists <- rowSums(cbind(results$assists, results$assists.new), na.rm = TRUE)
      results$clean_sheets <- rowSums(cbind(results$clean_sheets, results$clean_sheets.new), na.rm = TRUE)
      results <- results[, c("manager", "goals", "assists", "clean_sheets")]
    }
  }

  return(results[order(-results$goals, -results$assists, -results$clean_sheets), ])
}
