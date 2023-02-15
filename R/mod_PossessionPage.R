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
    fluidRow(
      bs4Dash::box(
        width = 6,
        collapsed = F,
        collapsible = F,
        title = "Home",
        shiny::plotOutput(outputId = ns("PossessionPlotHome"))
      ),
      bs4Dash::box(
        width = 6,
        collapsed = F,
        collapsible = F,
        title = "Away",
        shiny::plotOutput(outputId = ns("PossessionPlotAway"))
      )
    )


  )
}

#' PossessionPage Server Functions
#'
#' @noRd
mod_PossessionPage_server <- function(id, r, matchesDF){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## get home teams possession
    HomeTeamPossession <- reactive({
      r$MatchEvents() %>%
        dplyr::filter(possession_team.name == matchesDF()$home_team.home_team_name,
                      play_pattern.name == "Regular Play")
    })

    AwayTeamPossession <- reactive({
      r$MatchEvents() %>%
        dplyr::filter(possession_team.name == matchesDF()$away_team.away_team_name,
                      play_pattern.name == "Regular Play")
    })


    ### plot home and away team possession
    output$PossessionPlotHome <- renderPlot({
      SBpitch::create_Pitch()+
        stat_density_2d_filled(data = HomeTeamPossession(), aes(x = location.x, y  = location.y),
                               geom = "polygon",
                               alpha = 0.5,
                               show.legend = F,
                               na.rm = T,
                               h = 15)+## change h bandwith
        scale_fill_brewer(palette = "OrRd")
    })


    output$PossessionPlotAway <- renderPlot({
      SBpitch::create_Pitch()+
        stat_density_2d_filled(data = AwayTeamPossession(), aes(x = location.x, y  = location.y),
                               geom = "polygon",
                               alpha = 0.5,
                               show.legend = F,
                               na.rm = T,
                               h = 15)+## change h bandwith
        scale_fill_brewer(palette = "PuBu")+
        scale_x_reverse()+
        scale_y_reverse()
    })



  })
}

## To be copied in the UI
# mod_PossessionPage_ui("PossessionPage_1")

## To be copied in the server
# mod_PossessionPage_server("PossessionPage_1")
