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
        bs4Dash::tabBox(
          collapsible = F,
          width = 8,
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
          ),
      shiny::column(
        width = 4,
        DT::DTOutput(
          outputId = ns("ShotStats")
          )
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        collapsible = F,
        closable = F,
        width = 8,
        shiny::plotOutput(
          outputId = ns("pitchPlot"),
          width = "100%")
        ),
      shiny::column(width = 4,
        shiny::uiOutput(outputId = ns("ShotPlayerUI")),
        shiny::uiOutput(outputId = ns("ShotPlayersUI")),
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
        plot_xG_RaceChart(matchEvents = r$MatchEvents(),
                          MatchesDF = matchesDF(),
                          matchID = unique(r$MatchEvents()$match_id)) +
        ggplot2::theme(legend.position = "bottom")
    })

    MatchShots <- reactive({
      r$MatchEvents() %>%
        dplyr::filter(type.name == "Shot") %>%
        dplyr::mutate(location.x = ifelse(team.name == unique(matchesDF()$home_team.home_team_name), location.x, 120 - location.x),
                      location.y = ifelse(team.name == unique(matchesDF()$home_team.home_team_name), location.y, 80 - location.y))

      })

    output$ShotPlayerUI <- shiny::renderUI({
      MatchPlayers <- unique(MatchShots()$player.name)
      MatchTeams <- unique(MatchShots()$team.name)
      shiny::selectizeInput(
        inputId = ns("ShotPlayer"),
        label = "team Shots",
        multiple = F,
        selected = "Everyone",
        choices = c("Everyone", MatchTeams))
    })

    output$ShotPlayersUI <- shiny::renderUI({
      if (input$ShotPlayer == "Everyone") {
        MatchPlayers <- unique(MatchShots()$player.name)
      } else {
        MatchPlayers <- unique(MatchShots() %>%
                                 dplyr::filter(team.name == input$ShotPlayer) %>%
                                 dplyr::pull(player.name))
      }
      shinyWidgets::multiInput(
        inputId = ns("ShotPlayers"),
        label = "player shots",
        choices = MatchPlayers,
        selected = NULL
      )
    })

    output$ShotStats <-  DT::renderDT({
      MatchShots() %>%
        dplyr::group_by(team.name) %>%
        dplyr::summarise(`Total Shots` = dplyr::n(),
                         `Exp Goal` = round(sum(shot.statsbomb_xg), 2),
                         `xG/Shot` = round(sum(shot.statsbomb_xg)/dplyr::n(), 2)) %>%
        t() %>%
        janitor::row_to_names(row_number = 1)
    },options = list(dom = 't',ordering = F))


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
        MatchShots = MatchShots()
      } else if (input$ShotPlayer %in% c(unique(matchesDF()$home_team.home_team_name),
                                         unique(matchesDF()$away_team.away_team_name))) {
        MatchShots = MatchShots() %>%
          dplyr::filter(team.name == input$ShotPlayer)
      }

      if (!is.null(input$ShotPlayers)) {
        MatchShots <- MatchShots %>%
          dplyr::filter(player.name %in% input$ShotPlayers)
      }

      ShotPlot <- SBpitch::create_Pitch(line_colour = "black",
                                        grass_colour = "white",
                                        background_colour = "white",
                                        goal_colour = "black",
                                        goaltype = "box")+
        ggplot2::geom_point(data = MatchShots,
                            ggplot2::aes(x = location.x,
                                         y = location.y,
                                         shape = shot.type.name,
                                         colour = shot.statsbomb_xg),
                            size = 9) +
        ggplot2::scale_color_gradientn(colours = c("blue", "yellow", "darkred"),
                                       limits = c(0,1),
                                       values = c(0,0.5,1))+
        ggplot2::scale_shape_manual(values = c("Corner" = 18, "Free Kick" = 18, "Open Play" = 16, "Penalty" = 8, "Kick Off" = 18, "Blocked" = 4))+
        ggplot2::theme(legend.position = "bottom")+
        ggplot2::labs(colour = "xG", shape = "Shot Type")

        #### shot types c("Corner", "Free Kick", "Open Play", "Penalty", "Kick Off", "Blocked")
        #### select the shape for each
        #ggplot2::scale_shape_manual()
      #xlim(c(60,120))
      #ggplot2::coord_flip()

      ShotPlot

    })

  })
}

## To be copied in the UI
# mod_ShotPage_ui("ShotPage_1")

## To be copied in the server
# mod_ShotPage_server("ShotPage_1")
