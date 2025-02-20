#home_ui
home_server <- function(id, auth_info, geoflow_configs, parent.session){
  
  moduleServer(id, function(input, output, session) {
    
    output$boxes = renderUI({
      fluidRow(
        valueBox(
          elevation = 4,
          value = length(geoflow_configs()),
          width = 3,
          gradient = TRUE,
          subtitle = "Workflows",
          color = "primary",
          icon = icon("gears")
        )
      )
    })
    
  })
}