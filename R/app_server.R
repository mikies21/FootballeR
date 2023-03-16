#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  Comp <- reactive({
    comp <- subset(x = Competitions,
                   subset = competition_gender == input$Gender)
  })
    #shiny::eventReactive(input$StartApp ,{
    #if (attempt::is_try_error(StatsBombR::FreeCompetitions())){
    #  # Notify the user
    #  shinyalert::shinyalert(text = "Could not connect")
    #} else {
    #  return(StatsBombR::FreeCompetitions())
    #}
    #})

  ### competition name based on contry selection

  output$CompetitionUI <- shiny::renderUI({
    comp <- Comp()[, c("country_name", "competition_name")]
    comp <- split(comp, comp$country_name)
    comp <- lapply(comp, function(x) {
      setNames(unique(x)$competition_name, unique(x)$competition_name)

    })
    #comp <- lapply(comp, function(x) unique(x)$competition_name)
    ## ui output
    shiny::selectizeInput(inputId = "Competition",
                            label = "Competition",
                            choices = comp,
                            multiple = F
                            )

  })

  ### season id based on competition selection
  output$SeasonUI <- shiny::renderUI({
    countryComp <- subset(x = Comp(),
                           subset = competition_name == input$Competition &
                            competition_gender == input$Gender)
    shiny::selectizeInput(inputId = "Season",
                          label = "Season",
                          choices = unique(countryComp$season_name),
                          multiple = F
                       )
  })

  ######################
  #country <- reactive({
  #
  #})

  matchesDF <- eventReactive(input$CollectData,{
        # Continue computing if the connection was successful
        comp <- subset(x = Comp(),
                       subset =
                         ### need to add country in case more comps have same name
                         competition_name == input$Competition &
                         season_name == input$Season &
                         competition_gender == input$Gender)
        StatsBombR::FreeMatches(Competitions = comp)
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
