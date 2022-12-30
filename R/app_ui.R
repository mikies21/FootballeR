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
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
        title = "FootballeR",
        #color = "primary",
        #href = "https://www.google.fr",
        #image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      ),
      border = TRUE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("th"),
      fixed = FALSE),
      sidebar = bs4Dash::dashboardSidebar(
        minified = TRUE, # when sidebar collapsed keep sidebar visible
        expandOnHover = FALSE, # do not expand on hover
        bs4Dash::sidebarMenu(
          bs4Dash:: menuItem(
            "Start",
            tabName = "Start",
            icon = shiny::icon("sliders")
          ),
          bs4Dash::sidebarHeader("Attacking analysis"),
          bs4Dash:: menuItem(
            "Shot",
            tabName = "Shot",
            icon = shiny::icon("sliders")
            ),
          bs4Dash:: menuItem(
            "Possession",
            tabName = "Possession",
            icon = shiny::icon("sliders")
            )
          )
        ),
      controlbar = bs4Dash::dashboardControlbar(
        id = "ControlBar",
        disable = FALSE,
        width = 250,
        collapsed = FALSE,
        overlay = TRUE,
        skin = "dark",
        pinned = TRUE,
        bs4Dash::controlbarMenu(
          id = "controlbarmenu",
          bs4Dash::controlbarItem(
            title = "Competition",
            shiny::selectizeInput(
              inputId = "Country",
              label = NULL,
              choices = unique(Competitions$country_name),
              multiple = F
            ),
            shiny::uiOutput("CompetitionUI"),
            shiny::uiOutput("SeasonUI"),
            shiny::actionButton(
              inputId = "CollectData",
              label = "Collect data"
            ),
            #shiny::conditionalPanel(
              mod_CollectMatchData_ui("CollectMatchData_1")
            #)
          )
        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "Start",
            bs4Dash::box(title = "starting Box", "test")
          ),
          bs4Dash::tabItem(
            tabName = "Shot",
            mod_ShotPage_ui("ShotPage_1")
          ),
          bs4Dash::tabItem(
            tabName = "Possession"
          )
        )
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
    shinyalert::useShinyalert(force = T)
  )
}
