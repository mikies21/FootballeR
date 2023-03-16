#' CollectMatchData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_CollectMatchData_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ###
    bs4Dash::box(
      width = 12,
      collapsible = F,
      closable = F,
      title = "Welcome",
      "Thank you for visiting my app",
      shiny::fluidRow(
        shiny::column(
          width = 1,
          shinyWidgets::radioGroupButtons(
            inputId = ns("Gender"),
            label = "Gender:",
            choices = c(
              `<i class='fa-solid fa-venus'></i><p>female</p>` = "female",
              `<i class='fa-solid fa-mars'></i><p>male</p>` = "male"
            ),
            justified = TRUE,
            selected = "male",
            status = "primary",
            direction = "horizontal",
            size = "sm"
          )
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput(outputId = ns("CompetitionUI"))
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput(outputId = ns("SeasonUI"))
        ),
        column(
          width = 5,
          shiny::uiOutput(outputId = ns("TeamsUI")),
          shinyWidgets::radioGroupButtons(
            inputId = ns("HomeAway"),
            label = NULL,
            choices = c("Home", "Away"),
            selected = "Home",
            justified = TRUE,
            checkIcon = list(
              yes = shiny::icon("ok",
                                lib = "glyphicon"
              )
            )
          ),
          shiny::uiOutput(outputId = ns("MatchUI")),
          shiny::actionButton(
            inputId = ns("CollectMatchData"),
            label = "Collect Match Data"
          ),
          "Now Press Here!\n
      get all the Match Events and start the Match Analysis!"

        )

        # shiny::fluidRow(


        # )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shinycssloaders::withSpinner(
            ui_element = shiny::plotOutput(outputId = ns("matchInfo")),
            color="#0dc5c1")
        )
      )
    )
  )
}

#' CollectMatchData Server Functions
#'
#' @noRd
mod_CollectMatchData_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      shiny::selectizeInput(inputId = ns("Competition"),
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
      shiny::selectizeInput(inputId = ns("Season"),
                            label = "Season",
                            choices = unique(countryComp$season_name),
                            multiple = F
      )
    })

    ######################
    #country <- reactive({
    #
    #})

    matchesDF <- reactive({
      # Continue computing if the connection was successful
      comp <- subset(x = Comp(),
                     subset =
                       ### need to add country in case more comps have same name
                       competition_name == input$Competition &
                       season_name == input$Season &
                       competition_gender == input$Gender)
      compID <- unique(comp$competition_id)
      seasonID <- unique(comp$season_id)

      matches <- subset(x = AllMatches,
                        subset = competition.competition_id == compID &
                          season.season_id == seasonID)

      return(matches)
    })


    r$matchesDF <- shiny::reactive({matchesDF()})


    output$matchInfo <- shiny::renderPlot({
      plot(rnorm(1:100000))
    })


    ##### get unique teams in the league and season
    Teams <- reactive({
      unique(c(matchesDF()$home_team.home_team_name, matchesDF()$away_team.away_team_name))
    })
    ### ui output with the unique teams
    output$TeamsUI <- shiny::renderUI({
      selectInput(
        inputId = ns("Team"),
        label = "Team",
        selectize = T,
        choices = Teams()
      )
    })

    ##### get all the matches for the team selected
    TeamMatches <- reactive({
      if (input$HomeAway == "Home") {
        dfTeamMatch <- subset(x = matchesDF(), subset = home_team.home_team_name == input$Team)
      } else {
        dfTeamMatch <- subset(x = matchesDF(), subset = away_team.away_team_name == input$Team)
      }

      dplyr::mutate(
        .data = dfTeamMatch,
        PastedMatches = paste0(home_team.home_team_name, " (", home_score, ") v (", away_score, ") ", away_team.away_team_name)
      )
    })

    ##### UI that select match
    output$MatchUI <- shiny::renderUI({
      selectInput(
        inputId = ns("Match"),
        label = "Match",
        selectize = T,
        choices = TeamMatches()$PastedMatches
      )
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
