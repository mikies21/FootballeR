#' PossessionPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PossessionPage_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      width = 12,
      shiny::plotOutput(outputId = ns("Possession"))
      )

  )
}

#' PossessionPage Server Functions
#'
#' @noRd
mod_PossessionPage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_PossessionPage_ui("PossessionPage_1")

## To be copied in the server
# mod_PossessionPage_server("PossessionPage_1")
