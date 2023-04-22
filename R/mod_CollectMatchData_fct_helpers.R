
# Create a Plotly pitch ---------------------------------------------------

annotate_pitch_plotly <- function(x){
  plotly::plot_ly() %>%
    plotly::add_markers(type = "scatter", x=c(11, 109), y=c(40, 40), marker = list(size = 2, color = "black"), hoverinfo = "skip",showlegend = F) %>%
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



# Line up Plot ------------------------------------------------------------

lineups_plot <- function(selected_match) {
  lineup <- StatsBombR::cleanlineups(StatsBombR::get.lineupsFree(Match = selected_match)) %>%
    tidyr::unnest(cols = positions) %>%
    dplyr::filter(start_reason == "Starting XI") %>%
    dplyr::left_join(lineup_pitch_positions, by = "position_id") %>%
    dplyr::mutate(player_nickname = ifelse(is.na(player_nickname), player_name, player_nickname),
                  positionx = ifelse(team_name == unique(selected_match$home_team.home_team_name),
                                     yes = positionx/1.9,
                                     no = 120-(positionx/2)),
                  positiony = ifelse(team_name == unique(selected_match$home_team.home_team_name),
                                     yes = positiony,
                                     no = 80-positiony))

  annotate_pitch_plotly() %>%
    plotly::add_trace(type="scatter",
                      data = lineup,
                      x =~positionx,
                      y=~positiony,
                      #line=~team_name,
                      color=~team_name,
                      text =~player_nickname,
                      #opacity = 1,
                      hovertemplate = paste('<b>%{text}</b><extra></extra>'),
                      marker = list(color = 'rgba(255, 255, 255, 1)',
                                    size = 20,
                                    line = list(width = 2)),
                      symbols = "o") %>%
    plotly::add_trace(type = "scatter",
                      mode = "text",
                      data = lineup,
                      x =~positionx,
                      y=~positiony,
                      #line=~team_name,
                      text =~jersey_number,
                      showlegend = FALSE) %>%
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

}






