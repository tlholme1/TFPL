library(httr)
library(jsonlite)
library(dplyr)

# --- Modal XI for the world ---
get_modal_xi_world <- function(gameweek) {
  url <- paste0("https://fantasy.premierleague.com/api/event/", gameweek, "/live/")
  resp <- fromJSON(content(GET(url), as = "text", encoding = "UTF-8"))

  static <- fromJSON(content(GET("https://fantasy.premierleague.com/api/bootstrap-static/"),
                             as = "text", encoding = "UTF-8"))

  player_ids <- static$elements$id
  picks <- data.frame()

  for (pid in player_ids) {
    player_url <- paste0("https://fantasy.premierleague.com/api/element-summary/", pid, "/")
    player_data <- fromJSON(content(GET(player_url), as = "text", encoding = "UTF-8"))

    selected_by <- static$elements$selected_by_percent[static$elements$id == pid]
    picks <- rbind(picks, data.frame(player_id = pid, selected_by = as.numeric(selected_by)))
  }

  picks <- picks[order(-picks$selected_by), ]
  modal_xi <- head(picks, 11)

  modal_xi <- merge(modal_xi,
                    static$elements[, c("id", "web_name", "team", "element_type")],
                    by.x = "player_id", by.y = "id", all.x = TRUE)

  return(modal_xi)
}

# --- Modal XI for a given league ---
get_modal_xi_league <- function(league_id, gameweek) {
  league_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/",
                       league_id, "/standings/")
  league <- fromJSON(content(GET(league_url), as = "text", encoding = "UTF-8"))

  managers <- league$standings$results$entry
  all_picks <- data.frame()

  for (m in managers) {
    picks_url <- paste0("https://fantasy.premierleague.com/api/entry/", m,
                        "/event/", gameweek, "/picks/")
    picks <- fromJSON(content(GET(picks_url), as = "text", encoding = "UTF-8"))
    all_picks <- rbind(all_picks, data.frame(player_id = picks$picks$element))
  }

  counts <- aggregate(player_id ~ player_id, data = all_picks, FUN = length)
  counts <- counts[order(-counts$player_id), ]
  modal_xi <- head(counts, 11)

  return(modal_xi)
}

# --- Compare two teams ---
compare_teams <- function(team1, team2) {
  intersection <- length(intersect(team1, team2))
  union <- length(union(team1, team2))
  similarity <- intersection / union
  return(similarity)
}

# --- Find most similar teams in a league ---
find_most_similar_in_league <- function(league_id, gameweek) {
  league_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/",
                       league_id, "/standings/")
  league <- fromJSON(content(GET(league_url), as = "text", encoding = "UTF-8"))
  managers <- league$standings$results$entry

  teams <- list()
  for (m in managers) {
    picks_url <- paste0("https://fantasy.premierleague.com/api/entry/", m,
                        "/event/", gameweek, "/picks/")
    picks <- fromJSON(content(GET(picks_url), as = "text", encoding = "UTF-8"))
    teams[[as.character(m)]] <- picks$picks$element
  }

  sims <- data.frame(manager1 = character(), manager2 = character(), sim = numeric())
  for (i in 1:(length(teams)-1)) {
    for (j in (i+1):length(teams)) {
      sim <- compare_teams(teams[[i]], teams[[j]])
      sims <- rbind(sims, data.frame(manager1 = names(teams)[i],
                                     manager2 = names(teams)[j],
                                     sim = sim))
    }
  }

  sims <- sims[order(-sims$sim), ]
  return(sims)
}

# --- Compare league teams to world modal XI ---
find_closest_to_world_modal <- function(league_id, gameweek) {
  modal_world <- get_modal_xi_world(gameweek)$player_id
  league_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/",
                       league_id, "/standings/")
  league <- fromJSON(content(GET(league_url), as = "text", encoding = "UTF-8"))
  managers <- league$standings$results$entry

  sims <- data.frame(manager = character(), sim = numeric())
  for (m in managers) {
    picks_url <- paste0("https://fantasy.premierleague.com/api/entry/", m,
                        "/event/", gameweek, "/picks/")
    picks <- fromJSON(content(GET(picks_url), as = "text", encoding = "UTF-8"))

    sim <- compare_teams(picks$picks$element, modal_world)
    sims <- rbind(sims, data.frame(manager = m, sim = sim))
  }

  sims <- sims[order(-sims$sim), ]
  return(sims)
}

# --- Compare league teams to league modal XI ---
find_closest_to_league_modal <- function(league_id, gameweek) {
  modal_league <- get_modal_xi_league(league_id, gameweek)$player_id
  league_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/",
                       league_id, "/standings/")
  league <- fromJSON(content(GET(league_url), as = "text", encoding = "UTF-8"))
  managers <- league$standings$results$entry

  sims <- data.frame(manager = character(), sim = numeric())
  for (m in managers) {
    picks_url <- paste0("https://fantasy.premierleague.com/api/entry/", m,
                        "/event/", gameweek, "/picks/")
    picks <- fromJSON(content(GET(picks_url), as = "text", encoding = "UTF-8"))

    sim <- compare_teams(picks$picks$element, modal_league)
    sims <- rbind(sims, data.frame(manager = m, sim = sim))
  }

  sims <- sims[order(-sims$sim), ]
  return(sims)
}
