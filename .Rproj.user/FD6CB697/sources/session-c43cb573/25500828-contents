### leagues averages
#' @param MatchEvents a dataframe of match events. multiple matches
#'
#'

LeagueAverages <- function(MatchEvents, Matches) {
  ## possession average
  ## drop unused levels in match id
  MatchEvents$match_id <- droplevels(MatchEvents$match_id)
  ## split matches into single match dataframes
  MatchPasses <- MatchEvents |>
    dplyr::select(match_id, team.name, type.name) |>
    dplyr::filter(type.name == "Pass")
  MatchPossessions <- split(MatchPasses, MatchPasses$match_id) |>
    lapply(function(x) {
      ## to each dataframe find total passes attempted
      TotPasses <- length(x$type.name)
      ## find percentage possession
      home_team <- Matches[Matches$match_id == unique(x$match_id),]$home_team.home_team_name
      away_team <- Matches[Matches$match_id == unique(x$match_id),]$away_team.away_team_name

      x |>
        dplyr::select(team.name, type.name) |>
        table() |>
        as.data.frame() |>
        dplyr::mutate(perc_possession = Freq/TotPasses*100,
                      team_status = ifelse(team.name == home_team, "home", "away"))
    })

  TeamAverage <- MatchPossessions |>
    bind_rows(.id = "match_id") |>
    group_by(team.name, team_status) |>
    summarise(meanPasses = mean(Freq),
              meanPerc_possession = mean(perc_possession)) |>
    tidyr::pivot_wider(names_from = team_status,values_from = c(meanPasses, meanPerc_possession)) |>
    dplyr::mutate(meanPasses = (meanPasses_home + meanPasses_away)/2,
                  meanPerc_possession = (meanPerc_possession_home + meanPerc_possession_away)/2,
                  .after = team.name)

  return(TeamAverage)
}



perstatus <- LeagueAverages(MatchEvents = MatchesEvents, Matches = Matches)

whole <- LeagueAverages(MatchEvents = MatchesEvents, Matches = Matches)
perstatus %>%
  pivot_wider(names_from = team_status,values_from = c(meanPasses, meanPerc_possession))
  summarise(meanv = mean(meanPerc_possession))
whole
