#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic




  ######################
  # both servers take a reactiveValue,
  # which is set in the first module
  # and printed in the second one.
  # The server functions don't return any value per se
  r <- reactiveValues()

  ### start page module that shows league data



  mod_CollectMatchData_server("CollectMatchData_1",
                              r)

  mod_ShotPage_server("ShotPage_1",
                      r)

  mod_PossessionPage_server("PossessionPage_1",
                            r)

}
