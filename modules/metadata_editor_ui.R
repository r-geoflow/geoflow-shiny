#metadata_editor_ui
metadata_editor_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "metadata_editor",
   htmlOutput(ns("metadata_editor_info")),hr(),
   p(""),
   uiOutput(ns("meta_editor"))
  )
  
}