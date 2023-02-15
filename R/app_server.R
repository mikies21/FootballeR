#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  ### competition name based on contry selection
  output$CompetitionUI <- shiny::renderUI({
    country_name <- subset(x = Competitions, subset = country_name == input$Country)
    ## ui output
    shiny::selectizeInput(inputId = "Competition",
                       label = NULL,
                       choices = unique(country_name$competition_name),
                       multiple = F
                       )
  })
  ### season id based on competition selection
  output$SeasonUI <- shiny::renderUI({
    country_name <- subset(x = Competitions, subset = country_name == input$Country)
    shiny::selectizeInput(inputId = "Season",
                       label = NULL,
                       choices = unique(country_name$season_name),
                       multiple = F
                       )
  })

  ######################
  # if ever this connection failed, we notify the user
  # about this failed connection, so that they can know
  # what has gone wrong
  matchesDF <- eventReactive(input$CollectData,{
    if (attempt::is_try_error(StatsBombR::FreeCompetitions())){
      # Notify the user
      shinyalert::shinyalert(text = "Could not connect")
      } else {
        # Continue computing if the connection was successful
        comp <- subset(x = Competitions,
                       subset = country_name == input$Country & competition_name == input$Competition & season_name == input$Season)

        StatsBombR::FreeMatches(Competitions = comp)
        }
    })

  ######################
  # both servers take a reactiveValue,
  # which is set in the first module
  # and printed in the second one.
  # The server functions don't return any value per se
  r <- reactiveValues()

  ### start page module that shows league data
  mod_StartPage_server("StartPage_1")


  mod_CollectMatchData_server("CollectMatchData_1",
                              r,
                              matchesDF)

  mod_ShotPage_server("ShotPage_1",
                      r,
                      matchesDF)

  mod_PossessionPage_server("PossessionPage_1",
                            r,
                            matchesDF)

}
