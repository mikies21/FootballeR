#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(

      ########## Theme Colour Choice
      # freshTheme = fresh::create_theme(
      #  fresh::bs4dash_vars(
      #    navbar_light_color = "#bec5cb",
      #    navbar_light_active_color = "#FFF",
      #    navbar_light_hover_color = "#FFF"
      #  ),
      #  fresh::bs4dash_yiq(
      #    contrasted_threshold = 10,
      #    text_dark = "#FFF",
      #    text_light = "#272c30"
      #  ),
      #  fresh::bs4dash_layout(
      #    main_bg = "#353c42"
      #  ),
      #  fresh::bs4dash_sidebar_light(
      #    bg = "#272c30",
      #    color = "#bec5cb",
      #    hover_color = "#FFF",
      #    submenu_bg = "#272c30",
      #    submenu_color = "#FFF",
      #    submenu_hover_color = "#FFF"
      #  ),
      #  fresh::bs4dash_status(
      #    primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
      #  ),
      #  fresh::bs4dash_color(
      #    gray_900 = "#FFF"
      #  )
      # ),


      ########## HEADER UI
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "FootballeR",
          # color = "primary",
          # href = "https://www.google.fr",
          # image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
        ),
        border = TRUE,
        sidebarIcon = shiny::icon("bars"),
        controlbarIcon = shiny::icon("th"),
        fixed = FALSE
      ),

      ########## #LEFT SIDE BAR UI
      sidebar = bs4Dash::dashboardSidebar(
        minified = TRUE, # when sidebar collapsed keep sidebar visible
        expandOnHover = FALSE, # do not expand on hover
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem(
            "Start",
            tabName = "Start",
            icon = shiny::icon("sliders")
          ),
          bs4Dash::sidebarHeader("Attacking analysis"),
          ##### leaving this as an example of how to do subitems
          # bs4Dash:: menuItem(text = "test1",tabName = "test1", startExpanded = T,
          #                   bs4Dash::menuSubItem(text = "subtest1", tabName = "subtest1",
          #                                        ),
          #                   bs4Dash::menuSubItem(text = "subtest2", tabName = "subtest2",
          #                   )),
          #####
          bs4Dash::menuItem(
            "Shot",
            tabName = "Shot",
            icon = shiny::icon("sliders")
          ),
          bs4Dash::menuItem(
            "Possession",
            tabName = "Possession",
            icon = shiny::icon("sliders")
          )
        )
      ),

      #### RIGHT SIDEBAR UI
      controlbar = bs4Dash::dashboardControlbar(
        # id = "ControlBar",
        # disable = FALSE,
        # width = 250,
        # collapsed = FALSE,
        # overlay = FALSE,
        # skin = "dark",
        # pinned = TRUE,
        bs4Dash::controlbarMenu(
          id = "controlbarmenu",
          bs4Dash::controlbarItem(
            title = "Competition",
            #    shiny::selectizeInput(
            #      inputId = "Country",
            #      label = NULL,
            #      choices = unique(Competitions$country_name),
            #      multiple = F
            #    ),
            #    shiny::uiOutput("CompetitionUI"),
            #    shiny::uiOutput("SeasonUI"),
          )
        )
      ),


      ###### MAIN BODY UI
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          ####
          bs4Dash::tabItem(
            tabName = "Start",
            fluidRow(
              column(
                width = 12,
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
                        inputId = "Gender",
                        label = "Gender:",
                        choices = c(`<i class='fa-solid fa-venus'></i><p>female</p>` = "female",
                                    `<i class='fa-solid fa-mars'></i><p>male</p>` = "male"),
                        justified = FALSE,
                        selected = "male",
                        status = "primary",
                        direction = "vertical",
                        size = "sm"
                        )
                    ),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput(outputId = "CompetitionUI")
                    ),
                    shiny::column(
                      width = 3,
                      shiny::uiOutput(outputId = "SeasonUI")
                    ),
                    column(
                      width = 1,
                      shiny::actionButton(
                        inputId = "CollectData",
                        label = "Collect data"
                      )
                    )
                  )
                )
              )
            ),
            shiny::fluidRow(
              bs4Dash::box(
                title = "Team and Match",
                width = 6,
                closable = F,
                collapsible = F,
                conditionalPanel(
                  condition = "input.CollectData == 0",
                  "Click this and then all available matches will be shown",
                ),
                conditionalPanel(
                  condition = "input.CollectData != 0",
                  mod_CollectMatchData_ui("CollectMatchData_1")
                )
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "Shot",
            mod_ShotPage_ui("ShotPage_1")
          ),
          bs4Dash::tabItem(
            tabName = "Possession",
            mod_PossessionPage_ui("PossessionPage_1")
          )
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = "Thank you to StatsBomb for the free practice data",
        right = tags$a(href = "https://twitter.com/micfresneda", "@micfresneda"),
        fixed = FALSE
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FootballeR"
    ),
    # Add here other external resources
    # for example, you can add
    shinyalert::useShinyalert(force = T),
    # thematic::thematic_shiny()
  )
}
