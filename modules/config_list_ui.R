#config_list_ui
config_list_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "config_list",
    h3("List of configurations"),hr(),
    p(""),
    fluidRow(
      tags$div(
        inputId = "config_list_wrapper", style = "margin-left:12px;margin-right:12px;",
        DT::DTOutput(ns("config_list_table"))
      )
    )
  )
  
}