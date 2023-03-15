#' CollectMatchData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_CollectMatchData_ui <- function(id){
  ns <- NS(id)
  tagList(
    ###

    #shiny::fluidRow(

      shiny::uiOutput(outputId = ns("TeamsUI")),
      shinyWidgets::radioGroupButtons(
        inputId = ns("HomeAway"),
        label = NULL,
        choices = c("Home", "Away"),
        selected = "Home",
        justified = TRUE,
        checkIcon = list(
          yes = shiny::icon("ok",
                            lib = "glyphicon"))
      ),
      shiny::uiOutput(outputId = ns("MatchUI")),
      shiny::actionButton(inputId = ns("CollectMatchData"),
                          label = "Collect Match Data"),
      "Now Press Here!\n
      get all the Match Events and start the Match Analysis!"

      #)

    )
}

#' CollectMatchData Server Functions
#'
#' @noRd
mod_CollectMatchData_server <- function(id, r, matchesDF){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### get unique teams in the league and season
    Teams <- reactive({
      unique(c(matchesDF()$home_team.home_team_name, matchesDF()$away_team.away_team_name))
    })
    ### ui output with the unique teams
    output$TeamsUI <- shiny::renderUI({
      selectInput(inputId = ns("Team"),
                  label = "Team",
                  selectize = T,
                  choices = Teams())
    })

    ##### get all the matches for the team selected
    TeamMatches <- reactive({
      if (input$HomeAway == "Home") {
        dfTeamMatch <- subset(x = matchesDF(), subset = home_team.home_team_name == input$Team)
      } else {
        dfTeamMatch <- subset(x = matchesDF(), subset = away_team.away_team_name == input$Team)
      }

      dplyr::mutate(.data = dfTeamMatch,
                    PastedMatches = paste0(home_team.home_team_name, " (", home_score,") v (",away_score, ") " ,away_team.away_team_name))
    })

    ##### UI that select match
    output$MatchUI <- shiny::renderUI({
      selectInput(inputId = ns("Match"),
                  label = "Match",
                  selectize = T,
                  choices = TeamMatches()$PastedMatches)
    })

    ##### Get the Match events for the selected match.
    ##### runs only after actionButton input$CollectMatchData
    r$MatchEvents <- shiny::eventReactive(input$CollectMatchData, {
      SelectedMatch <- subset(TeamMatches(), subset = PastedMatches == input$Match)
      StatsBombR::allclean(StatsBombR::free_allevents(MatchesDF = SelectedMatch))
    })

  })
}

## To be copied in the UI
# mod_CollectMatchData_ui("CollectMatchData_1")

## To be copied in the server
# mod_CollectMatchData_server("CollectMatchData_1")
