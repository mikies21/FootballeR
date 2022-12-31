#' StartPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_StartPage_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(width = 4, title = "Table"),

    bs4Dash::box(width = 4, title = "Next Match"),

    bs4Dash::box(width = 4, title = "other")

  )
}

#' StartPage Server Functions
#'
#' @noRd
mod_StartPage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_StartPage_ui("StartPage_1")

## To be copied in the server
# mod_StartPage_server("StartPage_1")
