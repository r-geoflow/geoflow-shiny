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
    tabsetPanel(
      id = "geoflow_config_blocks", 
      type = "pills",
      tabPanel(
        value = "profile",
        title = "Profile",
        tags$div(id = "profile",
                 br(),
                 uiOutput(ns("profile"))
        )
      ),
      tabPanel(
        value = "metadata",
        title = "Metadata",
        br(),
        box(id = "metadata", width = 12,
                 tabsetPanel(
                   id = "metadata_tabs", 
                   type = "tabs",
                   tabPanel(
                     value = "contacts",
                     title = "Contacts",
                     shiny::tagList(
                       br(),
                       shiny::actionButton(inputId = ns("add_contact"), label = "Add a new contact source", class = "btn-primary"),
                       shiny::actionButton(inputId = ns("modify_contact"), label = "Modify a new contact source", class = "btn-warning"),
                       shiny::actionButton(inputId = ns("delete_contact"), label = "Delete a contact source", class = "btn-danger"),
                       DT::DTOutput(ns("tbl_contacts"))
                     )
                   ),
                   tabPanel(
                     value = "entities",
                     title = "Entities",
                     shiny::tagList(
                       br(),
                       shiny::actionButton(inputId = ns("add_entity"), label = "Add a new entity source", class = "btn-primary"),
                       shiny::actionButton(inputId = ns("modify_entity"), label = "Modify an entity source", class = "btn-warning"),
                       shiny::actionButton(inputId = ns("delete_entity"), label = "Delete an entity source", class = "btn-danger"),
                       DT::DTOutput(ns("tbl_entities"))
                     )
                   ),
                   tabPanel(
                     value = "dictionary",
                     title = "Dictionary",
                     shiny::tagList(
                       br(),
                       shiny::actionButton(inputId = ns("add_dictionary"), label = "Add a new dictionary source", class = "btn-primary"),
                       shiny::actionButton(inputId = ns("modify_dictionary"), label = "Modify an dictionary source", class = "btn-warning"),
                       shiny::actionButton(inputId = ns("delete_dictionary"), label = "Delete an dictionary source", class = "btn-danger"),
                       DT::DTOutput(ns("tbl_dictionary"))
                     )
                   )
                   
                 )
        )
      ),
      tabPanel(
        value = "software",
        title = "Software",
        br(),
        box(id = "software", width = 12,
          shiny::actionButton(inputId = ns("add_software"), label = "Add a new software", class = "btn-primary"),
          shiny::actionButton(inputId = ns("modify_software"), label = "Modify a software", class = "btn-warning"),
          shiny::actionButton(inputId = ns("delete_software"), label = "Delete a software", class = "btn-danger"),
          DT::DTOutput(ns("tbl_software"))
        )
      ),
      tabPanel(
        value = "actions",
        title = "Actions",
        br(),
        box(id = "actions", width = 12,
          shiny::actionButton(inputId = ns("add_action"), label = "Add a new action", class = "btn-primary"),
          shiny::actionButton(inputId = ns("modify_action"), label = "Modify an action", class = "btn-warning"),
          shiny::actionButton(inputId = ns("delete_action"), label = "Delete an action", class = "btn-danger"),
          DT::DTOutput(ns("tbl_actions"))
        )
      )
    )
  )
}