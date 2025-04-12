#metadata_editor_ui
metadata_editor_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "metadata_editor",
   uiOutput(ns("meta_editor_choices")),
   uiOutput(ns("meta_editor"))
  )
  
}