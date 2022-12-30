#' ShotPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ShotPage_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 8,
        bs4Dash::tabBox(
          collapsible = F,
          width = 12,
          selected = "xgPlot",
          shiny::tabPanel(
            title = "xgPlot",
            shiny::plotOutput(
              outputId = ns("xG_matchPlot"),
              width = "100%"
              )
            ),
          shiny::tabPanel(
            title = "test2",
            "test 2 content"
            )
          )
        ),
      shiny::column(
        width = 4,
        DT::DTOutput(
          outputId = ns("data_table")
          )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::plotOutput(
          outputId = ns("pitchPlot"),
          width = "100%")
        ),
      shiny::column(
        width = 4,
        shiny::selectizeInput(
          inputId = ns("player_id"),
          label = "Shots by", choices = c("P1", "P2", "P3"))
        )
    )
  )
}

#' ShotPage Server Functions
#'
#' @noRd
mod_ShotPage_server <- function(id, r, matchesDF){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$xG_matchPlot <- renderPlot({
      plot_xG_RaceChart(matchEvents = r$MatchEvents(), matchID = r$MatchEvents()$match_id)
    })

    output$data_table <-  DT::renderDT({
      r$MatchEvents()
    })

    output$pitchPlot <- renderPlot({
      shinipsum::random_ggplot()
    })

  })
}

## To be copied in the UI
# mod_ShotPage_ui("ShotPage_1")

## To be copied in the server
# mod_ShotPage_server("ShotPage_1")
