#config_editor_ui
config_editor_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "config_editor",
    htmlOutput(ns("config_editor_info")),hr(),
    p("The geoflow configuration editor allows users to create a geoflow data flow configuration file in
      an interactive user-friendly manner. The user will be able to load an existing configuration file. Once 
      the configuration file created/edited, the user will be able to execute it workflow interactively."),
    h4(),
    fluidRow(
      box(
        width = 6, title = "Load configuration file?",
        fileInput(inputId = ns("jsonfile"), "Choose Json File", multiple = FALSE, accept = c(".json")),
        actionButton(inputId = ns("load_configuration"), "Load", style = "float:left;"),
        uiOutput(ns("jsonfile_msg"))
      ),
      box(
        width = 6, title = "Save/Export configuration file",
        actionButton(
          ns("saveConfiguration"),
          'Save configuration JSON file',
          icon = icon("save")
        ),
        downloadButtonCustom(
          ns("downloadConfiguration"),
          'Download configuration JSON file',
          icon = icon("download")
        )
      )
    ),
    uiOutput(ns("geoflow_config_tabpanel"))
  )
}