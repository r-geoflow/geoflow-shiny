#config_runner_ui
config_runner_ui <- function(id){
  
  ns <- NS(id)
  
  
  bs4Dash::tabItem(
    tabName = "config_runner",
    uiOutput(ns("workflow_manager"))
  )
  
}
