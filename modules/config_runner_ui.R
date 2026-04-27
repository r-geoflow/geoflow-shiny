#config_list_ui
config_list_ui <- function(id){
  
  ns <- NS(id)
  
  
  bs4Dash::tabItem(
    tabName = "config_list",
    uiOutput(ns("workflow_manager"))
  )
  
}
