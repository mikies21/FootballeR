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
          outputId = ns("ShotStats")
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
      shiny::column(width = 4,
        shiny::uiOutput(outputId = ns("ShotPlayerUI")),
        shiny::textOutput(outputId = ns("PlayerShotStat"))
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
      plot_xG_RaceChart(matchEvents = r$MatchEvents(), MatchesDF = matchesDF(), matchID = unique(r$MatchEvents()$match_id))
    })

    MatchShots <- reactive({r$MatchEvents() %>%
      dplyr::filter(type.name == "Shot")})

    output$ShotPlayerUI <- shiny::renderUI({
      MatchPlayers <- unique(MatchShots()$player.name)
      MatchTeams <- unique(MatchShots()$team.name)
      shiny::selectizeInput(
        inputId = ns("ShotPlayer"),
        label = "Shots by",
        choices = c("Everyone", MatchTeams, MatchPlayers))
    })

    output$ShotStats <-  DT::renderDT({


      MatchShots() %>%
        dplyr::group_by(team.name) %>%
        dplyr::summarise(`Total Shots` = dplyr::n(),
                         `Exp Goal` = round(sum(shot.statsbomb_xg), 2),
                         `xG/Shot` = round(sum(shot.statsbomb_xg)/dplyr::n(), 2)) %>%
        t() %>%
        janitor::row_to_names(row_number = 1)
    },style = "bootstrap")


    PlayerMatchShot <- shiny::reactive({
      if (input$ShotPlayer == "Everyone") {
        PlayerShots <- MatchShots()
      }
      else if (input$ShotPlayer == unique(MatchShots()$team.name)[1]) {
        PlayerShots <- MatchShots() %>%
          dplyr::filter(team.name == unique(MatchShots()$team.name)[1])
      }
      else if (input$ShotPlayer == unique(MatchShots()$team.name)[2]) {
        PlayerShots <- MatchShots() %>%
          dplyr::filter(team.name == unique(MatchShots()$team.name)[2])
      }
      else {
        PlayerShots <- MatchShots() %>%
        dplyr::filter(player.name == input$ShotPlayer)
      }

      ExGoals <- sum(PlayerShots$shot.statsbomb_xg)
      TotGoals <- PlayerShots %>%
        dplyr::filter(shot.outcome.name == "Goal") %>%
        nrow()
      TotShots <- nrow(PlayerShots)
      list("ExGoals" = round(ExGoals, 2),
           "TotGoals" = TotGoals,
           "TotShots" = TotShots)

    })

    output$PlayerShotStat <- shiny::renderText({
      paste0("Exp. goals: ", PlayerMatchShot()$ExGoals, " (", PlayerMatchShot()$TotGoals, " goals / ", PlayerMatchShot()$TotShots, " shots)")
    })


    output$pitchPlot <- renderPlot({
      if (input$ShotPlayer == "Everyone") {
        ShotPlot <- SBpitch::create_Pitch()+
          ggplot2::geom_point(data = MatchShots(),
                              ggplot2::aes(x = location.x,
                                           y = location.y,
                                           shape = shot.type.name,
                                           colour = shot.statsbomb_xg),
                              size = 9)
      } else if (input$ShotPlayer == unique(MatchShots()$team.name)[1]) {
        ShotPlot <- SBpitch::create_Pitch()+
          ggplot2::geom_point(data = MatchShots() %>%
                                dplyr::filter(team.name == unique(MatchShots()$team.name)[1]),
                              ggplot2::aes(x = location.x,
                                           y = location.y,
                                           shape = shot.type.name,
                                           colour = shot.statsbomb_xg),
                              size = 9)
      } else if (input$ShotPlayer == unique(MatchShots()$team.name)[2]) {
        ShotPlot <- SBpitch::create_Pitch()+
          ggplot2::geom_point(data = MatchShots() %>%
                                dplyr::filter(team.name == unique(MatchShots()$team.name)[2]),
                              ggplot2::aes(x = location.x,
                                           y = location.y,
                                           shape = shot.type.name,
                                           colour = shot.statsbomb_xg),
                              size = 9)
      } else {
        ShotPlot <- SBpitch::create_Pitch()+
          ggplot2::geom_point(data = MatchShots() %>%
                                dplyr::filter(player.name == input$ShotPlayer),
                              ggplot2::aes(x = location.x,
                                           y = location.y,
                                           shape = shot.type.name,
                                           colour = shot.statsbomb_xg),
                              size = 9)
      }

      ShotPlot <- ShotPlot +
        ggplot2::scale_color_gradientn(colours = c("blue", "yellow", "darkred"),
                                       values = c(0,0.5,1))+
        xlim(c(60,120))+
        ggplot2::coord_flip()

      ShotPlot

    })

  })
}

## To be copied in the UI
# mod_ShotPage_ui("ShotPage_1")

## To be copied in the server
# mod_ShotPage_server("ShotPage_1")
