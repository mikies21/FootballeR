
lineups_plot <- function(selected_match) {
  lineup <- StatsBombR::cleanlineups(StatsBombR::get.lineupsFree(selected_match)) %>%
    tidyr::unnest(cols = positions) %>%
    dplyr::filter(start_reason == "Starting XI") %>%
    dplyr::left_join(lineup_pitch_positions, by = "position_id") %>%
    dplyr::mutate(player_name = ifelse(is.na(player_nickname), player_name, player_nickname),
                  positionx = ifelse(team_name == unique(selected_match$home_team.home_team_name),
                                     yes = positionx/1.9,
                                     no = 120-(positionx/2)),
                  positiony = ifelse(team_name == unique(selected_match$home_team.home_team_name),
                                     yes = positiony,
                                     no = 80-positiony))

  team_lineup <-  SBpitch::create_Pitch()+
      ggplot2::geom_point(data = lineup, aes(x = positionx, y = positiony, colour = team_name), shape = 1, size = 5)+
      ggplot2::geom_text(data = lineup, aes(x = positionx, y = positiony, label = jersey_number), size = 3)+
      ggplot2::geom_label(data = lineup, aes(x = positionx, y = positiony, label = player_name),
                          vjust = 1.5,
                          size = 3)+
    ggplot2::annotate(geom = "text")
    theme_void()
  team_lineup
}
