# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  shiny::callModule(config_editor_server, "config_editor")
  shiny::callModule(config_list_server, "config_list")
   
}