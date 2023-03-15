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
    ),
    DT::DTOutput(outputId = ns("possession_table"))


  )
}

#' PossessionPage Server Functions
#'
#' @noRd
mod_PossessionPage_server <- function(id, r, matchesDF){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## possession stats
    possession_stats <- reactive({
      team_possessions <- split(r$MatchEvents(), r$MatchEvents()$possession_team.name)

      team_possessions_list <- lapply(team_possessions, function(x){
        sum(x$duration, na.rm = T)
      })
      total_possession_time <- team_possessions_list[[1]]+team_possessions_list[[2]]

      data.frame("home" = 100*team_possessions_list[[matchesDF()$home_team.home_team_name]]/total_possession_time,
                 "away" = 100*team_possessions_list[[matchesDF()$away_team.away_team_name]]/total_possession_time)
    })

    output$possession_table <- DT::renderDT({
      possession_stats()
    })

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
