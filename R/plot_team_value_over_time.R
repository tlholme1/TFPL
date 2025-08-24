#' Plot Team Value Over Time for a Fantasy Premier League (FPL) League
#'
#' This function fetches team value data for gameweeks 1:LastGW using
#' FPLGetTeamValue() and plots the team value time series using ggplot2.
#'
#' Notes:
#' - This version does NOT perform runtime checks for ggplot2/data.table being installed;
#'   it assumes those packages (or the equivalent functions) are available in the environment.
#' - The x and y axis labels and the theme are fixed (kept simple and sensible). If you want
#'   them customizable, I can add parameters back in.
#'
#' @param LeagueCode string. League code passed to FPLGetTeamValue().
#' @param LastGW integer. Last gameweek to fetch (will fetch 1:LastGW).
#' @param teams character vector or NULL. If provided, only these teams (column `rn`) will be plotted.
#' @param divide_by numeric. Value to divide TeamValue by for plotting (default 10 to convert to GBP).
#' @param colors character vector or NULL. Named or unnamed colors to use for teams. If shorter than needed,
#'               colors will be extended with a palette based on provided values.
#' @param title string. Plot title.
#' @param line_size numeric. Size of lines.
#' @param breaks numeric vector or NULL. Breaks for x-axis. If NULL, uses 1:LastGW (or pretty breaks for large LastGW).
#'
#' @return A list with elements:
#'   - plot: the ggplot2 object
#'   - data: the data.table used to create the plot
#'
#' @examples
#' \dontrun{
#'   out <- plot_team_value_over_time(LeagueCode = "myLeague", LastGW = 10)
#'   out$plot
#' }
#'
#' @export
plot_team_value_over_time <- function(
  LeagueCode,
  LastGW,
  teams = NULL,
  divide_by = 10,
  colors = NULL,
  title = "Team Value Over Time",
  line_size = 1,
  breaks = NULL
) {
  if (!is.numeric(LastGW) || LastGW < 1) stop("LastGW must be an integer >= 1")
  if (!nzchar(LeagueCode)) stop("LeagueCode must be provided")

  # Fetch data for each GW. Fail fast with informative message if fetch fails.
  gws <- seq_len(as.integer(LastGW))
  fetched <- lapply(gws, function(gw) {
    res <- tryCatch(
      FPLGetTeamValue(LeagueCode = LeagueCode, GW = gw),
      error = function(e) {
        stop(sprintf("Error fetching data for GW %s: %s", gw, e$message))
      }
    )
    if (is.null(res)) stop(sprintf("FPLGetTeamValue returned NULL for GW %s", gw))
    res
  })

  ChartData <- data.table::rbindlist(l = fetched, idcol = "GW")

  # Validate expected columns
  if (!"rn" %in% names(ChartData)) stop("ChartData does not contain column 'rn' (team names).")
  if (!"TeamValue" %in% names(ChartData)) stop("ChartData does not contain column 'TeamValue'.")

  # Optionally filter teams
  if (!is.null(teams)) {
    ChartData <- ChartData[ChartData$rn %in% teams, , drop = FALSE]
    if (nrow(ChartData) == 0) stop("No rows remain after filtering by 'teams'. Check team names.")
  }

  # Determine colors
  default_colors <- c("#5B9BD5", "#ED7D31", "#A5A5A5", "#FFC000", "#ff33ba", "#70AD47", "#255E91", "#9E480E")
  if (is.null(colors)) colors <- default_colors
  teams_present <- unique(as.character(ChartData$rn))
  n_teams <- length(teams_present)
  if (length(colors) < n_teams) {
    colors <- grDevices::colorRampPalette(colors)(n_teams)
  }
  # Ensure named vector mapping team -> color for scale_color_manual
  if (is.null(names(colors)) || !all(teams_present %in% names(colors))) {
    names(colors) <- teams_present
  }

  # x-axis breaks
  if (is.null(breaks)) {
    breaks <- if (LastGW <= 30) seq_len(LastGW) else pretty(seq_len(LastGW), n = 10)
  }

  # Build plot; axis labels and theme are fixed (not exposed as parameters)
  p <- ggplot2::ggplot(
      data = ChartData,
      mapping = ggplot2::aes(x = GW, y = TeamValue / divide_by, group = rn, color = rn)
    ) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = "Gameweek", y = "Team Value (GBP)", title = title, color = NULL) +
    ggplot2::theme(
      legend.position = c(0.025, 0.975),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.5),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.margin = ggplot2::margin(6, 6, 6, 6)
    ) +
    ggplot2::scale_x_continuous(breaks = breaks)

  print(p)
  invisible(list(plot = p, data = ChartData))
}