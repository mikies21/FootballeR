#' Create a League table for the selected matches
#' @param Matches dataframe a datafame of matches obtained from StasBombR functions get.matches or FreeMatches
#' @param date vector a string in the format YYYY-MM-DD. it can be a selection between 2 dates. see example
#' @returns a leage table dataframe
#' @examples
#' library(StatsBombR)
#' Competition <- FreeCompetitions() |>
#'   filter(competition_name == "Serie A", season_name == "2015/2016")
#'   Matches <- FreeMatches(Competitions = Competition)
#' ## match date as date
#' Matches <- Matches |>
#'   mutate(match_date = as.Date(match_date),
#'          match_id = as.factor(match_id))
#' ## whole season league table
#' CreateLeagueTable(Matches)
#'
#' ## partial season table
#' CreateLeagueTable(Matches, date = c("2026-01-01"))
#'
#' ## season table in between two dates
#' CreateLeagueTable(Matches, date = c("2026-01-01", 2016-05-26))
#' @exa

CreateLeagueTable <- function(Matches, date = c("")){
  ## filter date
  if (date == "") {
    date <-  max(Matches$match_date)
    MatchesFilt <- Matches
  } else {
    MatchesFilt <- Matches |> filter(match_date <= date)
  }
  HomeGames <- split(MatchesFilt, MatchesFilt$home_team.home_team_name)
  HomePoints <- lapply(HomeGames, function(x){
    x %>% dplyr::transmute(Played = 1,
                    Points = dplyr::case_when(home_score > away_score ~ 3,
                                       home_score == away_score ~ 1,
                                       home_score < away_score ~ 0),
                    W = ifelse(home_score > away_score, 1, 0),
                    D = ifelse(home_score == away_score, 1, 0),
                    L = ifelse(home_score < away_score, 1, 0),
                    GF = home_score,
                    GA = away_score,
                    GD = GF-GA) %>%
      dplyr::summarise(Played.h = sum(Played),
                Points.h = sum(Points),
                GF.h = sum(GF),
                GA.h = sum(GA),
                GD.h = sum(GD),
                W.h = sum(W),
                D.h = sum(D),
                L.h = sum(L))
  }) %>% dplyr::bind_rows(.id = "Team") %>%
    dplyr::arrange(dplyr::desc(Points.h))

  AwayGames <- split(MatchesFilt, MatchesFilt$away_team.away_team_name)
  AwayPoints <- lapply(AwayGames, function(x){
    x %>% dplyr::transmute(Played = 1,
                    Points = dplyr::case_when(away_score > home_score ~ 3,
                                       away_score == home_score ~ 1,
                                       away_score < home_score ~ 0),
                    W = ifelse(away_score > home_score, 1, 0),
                    D = ifelse(away_score == home_score, 1, 0),
                    L = ifelse(away_score < home_score, 1, 0),
                    GF = away_score,
                    GA = home_score,
                    GD = GF-GA) %>%
      dplyr::summarise(Played.a = sum(Played),
                Points.a = sum(Points),
                GF.a= sum(GF),
                GA.a = sum(GA),
                GD.a = sum(GD),
                W.a = sum(W),
                D.a = sum(D),
                L.a = sum(L))
  }) %>% dplyr::bind_rows(.id = "Team") %>%
    dplyr::arrange(desc(Points.a))


  LeagueTable <- merge(HomePoints, AwayPoints, by = "Team") |>
    dplyr::mutate(Points = Points.h+Points.a,
           Played = Played.h+Played.a,
           GF = GF.h+GF.a,
           GA = GA.h+GA.a,
           GD = GD.h+GD.a,
           W = W.h+W.a,
           D = D.h+W.a,
           L = L.h+L.a,
           .before = Played.h) |>
    dplyr::arrange(dplyr::desc(Points))
  return(LeagueTable)
}
