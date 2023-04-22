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

  ### example DATA DELETE
  #matchEvents = StatsBombData
  #MatchesDF = MatchID
  #matchID = 69164
  #ExtraTime = F
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

  plotly::plot_ly(type = "scatter",
                  mode = 'lines+markers',
                  data = team_xG_df,
                  color=~team.name,
                  x=~as.numeric(minute),
                  y=~cum_xG,
                  text=~minute,
                  hoverinfo = "none",
                  marker = list(size = 1)) %>%
    plotly::add_markers(data = Goal_df,
                        type = "scatter",
                        x=~as.numeric(minute),
                        y=~cum_xG,
                        color =~team.name,
                        text=~player.name,
                        hovertemplate = paste("<b>%{text}</b><extra></extra>"),
                        marker = list(size = 13),
                        showlegend = FALSE) %>%
    plotly::add_markers(data = Shot_df,
                        type = "scatter",
                        x=~as.numeric(minute),
                        y=~cum_xG,
                        color =~team.name,
                        text=~paste0(player.name, "<br>",
                                    shot.outcome.name),
                        hovertemplate = paste("%{text}<extra></extra>"),
                        marker = list(size = 5),
                        showlegend = FALSE,
                        symbols = "x") %>%
    plotly::layout(
      showlegend = T,
      legend = list(orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5),
      shapes = list(
        list(type = "line", layer='below', line = list(color = "grey"), x0 = halfs_end[1], y0 = 0, x1 = halfs_end[1], y1 = max(Shot_df$cum_xG))
        ),
      title = paste("<b>xG Race Chart</b><br>", plot_subtitle),
      xaxis = list(
        title = "",
        ticktext=labels,
        tickvals=breaks
      ),
      yaxis = list(
        title = "cumulative xG"
      ))
}

plot_shots <- function(MatchShots){

  #####################################################
  #comp <- StatsBombR::FreeCompetitions() %>%
  #  dplyr::filter(competition_id==11 & season_name=="2005/2006")
  #MatchID = 69164
  #Matches <- StatsBombR::FreeMatches(comp)
  #matchesDF <- Matches %>%
  #  dplyr::filter(match_id == 69164)
  #StatsBombData <- StatsBombR::free_allevents(MatchesDF = matchesDF, Parallel = T)
  #StatsBombData = StatsBombR::allclean(StatsBombData)

  ##################################################





  #MatchShots = StatsBombData %>%
  #  dplyr::filter(type.name == "Shot") %>%
  #  dplyr::mutate(location.x = ifelse(team.name == unique(matchesDF$home_team.home_team_name), location.x, 120 - location.x),
  #                location.y = ifelse(team.name == unique(matchesDF$home_team.home_team_name), location.y, 80 - location.y))

  ShotPlot <- annotate_pitch_plotly() %>%
    plotly::add_trace(type="scatter",
                      data = MatchShots,
                      x=~location.x,
                      y=~location.y,
                      color=~shot.statsbomb_xg,
                      symbol=~shot.type.name,
                      text =~paste0(player.name, "<br>", shot.outcome.name, "<br>xG: ", round(shot.statsbomb_xg, 3)),
                      hovertemplate = paste('%{text}<extra></extra>'),
                      showlegend = T) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      ))
  #### shot types c("Corner", "Free Kick", "Open Play", "Penalty", "Kick Off", "Blocked")
  #### select the shape for each
  #ggplot2::scale_shape_manual()
  #xlim(c(60,120))
  #ggplot2::coord_flip()

  ShotPlot
}

