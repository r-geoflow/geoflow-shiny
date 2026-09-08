#metadata_editor_ui
metadata_editor_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "metadata_editor",
   uiOutput(ns("meta_editor_choices")),
   uiOutput(ns("meta_editor_hrcustom")),
   tags$div(
    uiOutput(ns("meta_editor_entry_selector_wrapper")),
    tags$div(
      uiOutput(ns("meta_editor_entry_new_wrapper")),
      style = "margin-left:10px;margin-top:32px;"
    ),
    style = "display: inline-flex;"
   ),
   uiOutput(ns("meta_editor_wrapper"))
  )
  
}