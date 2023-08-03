#' Create a League table for the selected matches
#' @param MatchEvents A datafame of Matches Events obtained from StasBombR functions free_allevents. multiple matches are allowed
#' @param Matches description
#' @returns a list of dataframes with the amount of passees attempted and the percentage possiseesion for each teeam


PossessionMatch <- function(MatchEvents, Matches) {
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

  return(MatchPossessions)
}



PossessionMatch(MatchEvents = MatchesEvents, Matches = Matches)
