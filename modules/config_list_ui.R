#config_list_ui
config_list_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "config_list",
    htmlOutput(ns("config_list_info")),hr(),
    p(""),
    fluidRow(
      tags$div(
        inputId = "config_list_actions",
        class = "col-md-4",
        actionButton(ns("config_list_refresh"), "Refresh")
      )
    ),
    fluidRow(
      box(
        inputId = "config_list_wrapper", 
        title = "Workflows", status = "primary", width = 6,
        DT::DTOutput(ns("config_list_table"))
      ),
      box(
        inputId = "config_log_wrapper",
        title = "Console", status = "primary", width = 6,
        uiOutput(ns("config_job_status")),br(),
        uiOutput(ns("config_job_interactive_log"))
      )
    )
  )
  
}