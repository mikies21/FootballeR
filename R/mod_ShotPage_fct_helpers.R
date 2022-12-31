#library(tidyverse)
# expected goals function -------------------------------------------------
### matchEvents = all events in the a competition
### matchID = select match in the comp
### ExtraTime = where to plot full and extra time
#data("Competitions")
#testMatchID = 303731
#testMatches <- StatsBombR::FreeMatches(Competitions = Competitions[Competitions$competition_name == "La Liga" & Competitions$season_name == "2019/2020",])
#testMatchEvents <- StatsBombR::free_allevents(MatchesDF = testMatches[testMatches$match_id == testMatchID, ])
#plot_xG_RaceChart(matchEvents = testMatchEvents, testMatches,  matchID = 303731)

plot_xG_RaceChart <- function(matchEvents, MatchesDF, matchID, ExtraTime = F) {

  ## using the season MatchDF get home and away infomation
  ## uild the plot title here
  home_team <- subset(x = MatchesDF, subset = match_id == matchID)$home_team.home_team_name
  home_score <- subset(x = MatchesDF, subset = match_id == matchID)$home_score
  away_team <- subset(x = MatchesDF, subset = match_id == matchID)$away_team.away_team_name
  away_score <- subset(x = MatchesDF, subset = match_id == matchID)$away_score

  plot_subtitle <- paste0(home_team, " (", home_score, ") v (",away_score,") ", away_team)
  ## get times HT and FT
  halfs_end <- unique(subset(matchEvents, subset = type.name == "Half End" & match_id == matchID)$minute)

  addedTime1 <- halfs_end[1] - 45
  addedTime2 <- halfs_end[2] - 90
  AddedTime <- c(addedTime1, addedTime2)

  if (ExtraTime == T) {
    ET <- c(1, 2, 3, 4)
    addedTime3 <- halfs_end[3] - 105
    addedTime4 <- halfs_end[4] - 120
    breaks <- c(0, 15, 30, 45, halfs_end[1], 60 + addedTime1, 75 + addedTime1, 90 + addedTime1, halfs_end[2] + addedTime1, 105 + addedTime1 + addedTime2, 120 + addedTime1 + addedTime2 + addedTime3)
    labels <- c("KO", "15", "30", "45", "HT", "60", "75", "90", "ET", "105", "FT")
    AddedTime <- c(addedTime1, addedTime2, addedTime3, addedTime4)

    Shot_df <- subset(
      x = matchEvents,
      subset = type.name == "Shot" & match_id == matchID & period %in% ET,
      select = c(team.name, period, player.name, minute, shot.statsbomb_xg, shot.outcome.name)
    ) %>%
      dplyr::group_by(team.name) %>%
      dplyr::mutate(cum_xG = cumsum(shot.statsbomb_xg)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(minute = dplyr::case_when(
        period == 1 ~ minute,
        period == 2 ~ minute + as.integer(addedTime1),
        period == 3 ~ minute + as.integer(addedTime1) + as.integer(addedTime2),
        period == 4 ~ minute + as.integer(addedTime1) + as.integer(addedTime2) + as.integer(addedTime3)
      ))
  } else {
    halfs_end <- halfs_end[1:2]
    ET <- c(1, 2)
    breaks <- c(0, 15, 30, 45, halfs_end[1], 60 + addedTime1, 75 + addedTime1, 90 + addedTime1, halfs_end[2] + addedTime1)
    labels <- c("KO", "15", "30", "45", "HT", "60", "75", "90", "FT")

    Shot_df <- subset(
      x = matchEvents,
      subset = type.name == "Shot" & match_id == matchID & period %in% ET,
      select = c(team.name, period, player.name, minute, shot.statsbomb_xg, shot.outcome.name)
    ) %>%
      dplyr::group_by(team.name) %>%
      dplyr::mutate(cum_xG = cumsum(shot.statsbomb_xg)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(minute = dplyr::case_when(
        period == 1 ~ minute,
        period == 2 ~ minute + as.integer(addedTime1)
      ))
  }

  Goal_df <- subset(
    x = Shot_df,
    subset = shot.outcome.name == "Goal"
  )


  team_xG_df <- Shot_df %>%
    dplyr::group_split(team.name) %>%
    lapply(function(x) {
      minute_shot <- x$minute
      x_minute <- c()
      for (i in minute_shot) {
        x_minute <- c(x_minute, rep(i, 2))
      }
      xg_shot <- x$cum_xG
      y_shot <- c()
      for (i in xg_shot) {
        y_shot <- c(y_shot, rep(i, 2))
      }

      data.frame(
        "team.name" = x$team.name %>% unique(),
        "minute" = c(0, x_minute, max(halfs_end) + sum(AddedTime)),
        "cum_xG" = c(0, 0, y_shot)
      )
    }) %>%
    dplyr::bind_rows()


  ggplot2::ggplot() +
    #ggplot2::geom_segment(ggplot2::aes(y = 0, yend = round(max(Shot_df$cum_xG)), x = halfs_end[1], xend = halfs_end[1]), colour = "grey") +
    ggplot2::geom_vline(xintercept = halfs_end[1], colour = "grey")+
    ggplot2::geom_line(
      data = team_xG_df,
      ggplot2::aes(x = as.numeric(minute), y = cum_xG, colour = team.name), show.legend = F
    ) +
    ggplot2::geom_point(
      data = Goal_df,
      ggplot2::aes(x = as.numeric(minute), y = cum_xG, colour = team.name),
      size = 3
    ) +
    ggrepel::geom_text_repel(
      data = Goal_df,
      ggplot2::aes(x = as.numeric(minute), y = cum_xG, label = player.name),
      size = 3,
      vjust = 1,
      hjust = 1
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0), limits = c(0, NA),
      breaks = breaks,
      labels = labels
    ) +
    #ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::theme_minimal()+
    ggplot2::labs(x = NULL, y = NULL, title = "xG Race Chart", subtitle = plot_subtitle) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}
