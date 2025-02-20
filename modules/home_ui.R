#home_ui
home_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "home",
    fluidRow(
      div(
        width = 12, style = "margin:12px;",
        tags$h2("geoflow-shiny",tags$small(HTML("Run interactive workflows with <a href='https://github.com/r-geoflow/geoflow' target='_blank'>geoflow</a>")))
      )
    ),
    uiOutput(ns("boxes"))
  )
  
}