# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  if(appConfig$auth){  
  
    credentials <- authLoginServer(
      id = "login",
      config = appConfig,
      log_out = reactive(logout_init())
    )

    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    observe({
      if (credentials()$user_auth) {
        
        auth_endpoint <- reactive({ credentials()$auth_endpoint })
        logged <- reactive({ credentials()$user_auth })
        auth_info <- reactive({ credentials()$auth_info })
        
        AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
        if(!is.null(auth_info())){
          INFO("Load configuration editor module")
          config_editor_server("config_editor", auth_endpoint, auth_info, logged, parent.session = session)
          INFO("Load configuration list module")
          config_list_server("config_list", auth_endpoint, auth_info, logged, parent.session = session)
        }
        
        shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        shinyjs::show(selector = "header")
        
      } else {
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        shinyjs::hide(selector = "header")
      }
    })
    
  }else{
    #anonymous usage
    INFO("Set-up geoflow-shiny in anonymous mode")
    INFO("Load configuration editor module")
    config_editor_server("config_editor", parent.session = session)
    INFO("Load configuration list module")
    config_list_server("config_list", parent.session = session)
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    shinyjs::show(selector = "header")

  }
  
  output$side_ui <- renderUI({
    if(appConfig$auth){
      req(credentials()$user_auth)
      sidebarMenu(
        id = "geoflow-tabs",
        menuItem(
          text = "Configuration",
          tabName = "config",
          menuSubItem(text = "Configuration editor", tabName = "config_editor"),
          menuSubItem(text = "List of configurations", tabName = "config_list"),
          startExpanded = TRUE
        )
      )
    }else{
      sidebarMenu(
        id = "geoflow-tabs",
        menuItem(
          text = "Configuration",
          tabName = "config",
          menuSubItem(text = "Configuration editor", tabName = "config_editor"),
          menuSubItem(text = "List of configurations", tabName = "config_list")
        )
      )
    }
  })
  
  output$main_ui <- renderUI({
    if(appConfig$auth){
      req(credentials()$user_auth)
      tabItems(
        config_editor_ui("config_editor"),
        config_list_ui("config_list")
      )
    }else{
      tabItems(
        config_editor_ui("config_editor"),
        config_list_ui("config_list")
      )
    }
  })

  #module page management
  session$userData$module <- reactiveVal(NULL)
  
  observe({
    #Update moduleUrl
    moduleUrl <- gsub('_', '-', input$`geoflow-tabs`)
    session$userData$module(moduleUrl)
    updateModuleUrl(session, moduleUrl)
  })
  
}