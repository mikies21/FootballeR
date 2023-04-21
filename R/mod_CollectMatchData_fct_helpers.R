#' create a plot with both teams line ups
#' @param selected_match a selected match from the matches dataframe obtained from freematches function
#' @return A ggplot with the lineup for the selected match
#' @export

lineups_plot <- function(selected_match) {
  lineup <- StatsBombR::cleanlineups(StatsBombR::get.lineupsFree(Match = matchID)) %>%
    tidyr::unnest(cols = positions) %>%
    dplyr::filter(start_reason == "Starting XI") %>%
    dplyr::left_join(lineup_pitch_positions, by = "position_id") %>%
    dplyr::mutate(player_nickname = ifelse(is.na(player_nickname), player_name, player_nickname),
                  positionx = ifelse(team_name == unique(matchID$home_team.home_team_name),
                                     yes = positionx/1.9,
                                     no = 120-(positionx/2)),
                  positiony = ifelse(team_name == unique(matchID$home_team.home_team_name),
                                     yes = positiony,
                                     no = 80-positiony))

  team_lineup <-  SBpitch::create_Pitch()+
      ggplot2::geom_point(data = lineup,
                          aes(x = positionx,
                              y = positiony,
                              colour = team_name),
                          shape = 1,
                          size = 5)+
      ggplot2::geom_text(data = lineup,
                         aes(x = positionx,
                             y = positiony,
                             label = jersey_number),
                         size = 3)+
      ggplot2::geom_label(data = lineup,
                          aes(x = positionx,
                              y = positiony,
                              label = player_name),
                          vjust = 1.5,
                          size = 3)+
    theme_void()

  SBpitch::create_Pitch()%>%
    plotly::ggplotly() %>%
    plotly::layout(shapes = list(
      list(type = "rect",
           fillcolor = "blue")
    ))
  team_lineup
}

annotate_pitch_plotly <- function(x){
  plot_ly() %>%
  add_markers(type = "scatter", x=c(11, 109), y=c(40, 40), marker = list(size = 2, color = "black"), hoverinfo = "skip",showlegend = F) %>%
  plotly::layout(
    showlegend = T,
    shapes = list(
      ## master shape
      list(type = "rect", layer='below',line = list(color = "black"), x0 = 0, x1 = 120, y0 = 80),
      ## halfway line
      list(type = "line", layer='below', line = list(color = "black"), x0 = 60, y0 = 80, x1 = 60, y1 = 0),
      ## center circle
      list(type = "circle", layer='below', line = list(color = "black"), x0 = 50.85, y0 = 30.85, x1 = 69.15, y1 = 49.15),
      ##penalty box home
      list(type = "rect", layer='below', line = list(color = "black"), x0 = 0, y0 = 60.16, x1 = 16.5, y1 = 19.84),
      ## penalty box away
      list(type = "rect", layer='below', line = list(color = "black"), x0 = 120, y0 = 60.16, x1 = 103.5, y1 = 19.84),
      ## 6 yard box home
      list(type = "rect", layer='below', line = list(color = "black"), x0 = 0, y0 = 49.16, x1 = 5.5, y1 = 30.84),
      ## 6 yard box away
      list(type = "rect", layer='below', line = list(color = "black"), x0 = 120, y0 = 49.16, x1 = 114.5, y1 = 30.84)
      ## box circle to adjust
      #list(type = 'circle',
      #     xref = 'x', x0 = 20.15, x1 = 11-9.15,
      #     yref = 'y', y0 = 40-9.15, y1 = 40+9.15,
      #     line = list(color = 'blue'))
      ))
}

annotate_pitch_plotly() %>%
  add_trace(type="scatter",
            data = lineup,
            x =~positionx,
            y=~positiony,
            color=~team_name,
            text =~player_nickname,
            #opacity = 1,
            hovertemplate = paste('<b>%{text}</b><extra></extra>'),
            marker = list(size = 15,
                          line = list(color =~team_name, width = 2)),
            symbols = "x")


80-40-11-5.5-7.32/2
80-19.84
