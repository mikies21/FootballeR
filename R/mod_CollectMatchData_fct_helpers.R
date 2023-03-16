
lineups_plot <- function(selected_match) {
  lineup <- StatsBombR::cleanlineups(StatsBombR::StatsBombFreeLineups(selected_match)) %>%
    tidyr::unnest(cols = positions) %>%
    dplyr::filter(start_reason == "Starting XI") %>%
    dplyr::left_join(lineup_pitch_positions, by = "position_id")

  team_lineup <- lapply(split(lineup, lineup$team_name), function(x){
    SBpitch::create_Pitch()+
      ggplot2::geom_point(data = x, aes(x = positionx, y = positiony), size = 3)+
      ggplot2::geom_label(data = x, aes(x = positionx, y = positiony, label = player_name),
                          vjust = 0,
                          nudge_y = -0.5,
                          size = 2)+
      ggplot2::coord_flip()
  })

}
