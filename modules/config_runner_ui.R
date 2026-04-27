#config_list_ui
config_list_ui <- function(id){
  
  ns <- NS(id)
  
  
  bs4Dash::tabItem(tabName = "config_list",
    htmlOutput(ns("config_list_info")),hr(),
    p(""),
    fluidRow(
      tags$div(
        inputId = "config_list_actions",
        class = "col-md-4"
      )
    ),
    uiOutput(ns("workflow_manager"))
  )
  
}
