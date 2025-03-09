#home_ui
home_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "home",
    uiOutput(ns("home_info")),
    uiOutput(ns("boxes"))
  )
  
}
