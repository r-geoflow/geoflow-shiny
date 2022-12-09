# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  onStop(function(){
    resetAuthSessionVariables(session)
  })

  if(appConfig$auth){  
  
    auth_info <- reactiveVal(NULL)
    
    if(appConfig$auth_ui){
      #auth with UI
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
          
          info = credentials()$auth_info
          info$logged <- credentials()$user_auth
          auth_info(info)
          
          if(!is.null(auth_info())){
            initAuthSessionVariables(session, auth_info())
            INFO("Load configuration editor module")
            config_editor_server("config_editor", auth_info, parent.session = session)
            INFO("Load configuration list module")
            config_list_server("config_list", auth_info, parent.session = session)
          }
          
          shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
          shinyjs::show(selector = "header")
          
        } else {
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
          shinyjs::hide(selector = "header")
        }
      })
    }else{
      #auth without UI
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show(selector = "header")
      
      #JWT auth
      observe({
        jwt_profile <- decodeJWT(appConfig$auth_jwt)
        #D4Science JWT Auth - setting interaction with D4Science StorageHub
        if(startsWith(jwt_profile$iss, "https://accounts.d4science.org")){
          AUTH_API <- try(d4storagehub4R::StoragehubManager$new(token = jwt_profile$access$access_token, token_type = "jwt"))
          if (is(AUTH_API, "StoragehubManager")) {
            assign("AUTH_API", AUTH_API, envir = GEOFLOW_SHINY_ENV)
            auth_info(list(
              endpoint = list(auth_url = "https://api.d4science.org/workspace", auth_type = "d4science"),
              backend = NA, 
              service = NA, 
              user = AUTH_API$getUserProfile()$username, 
              token = jwt_profile$access$access_token
            ))
            if(!is.null(auth_info())){
              INFO("Set-up geoflow-shiny in auth mode (no UI, token based)")
              initAuthSessionVariables(session, auth_info())
              INFO("Load configuration editor module")
              config_editor_server("config_editor", auth_info, parent.session = session)
              INFO("Load configuration list module")
              config_list_server("config_list", auth_info, parent.session = session)
            }
          }
        }
      })
    }
    
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
      if(appConfig$auth_ui){
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
            menuSubItem(text = "List of configurations", tabName = "config_list"),
            startExpanded = TRUE
          )
        )
      }
      
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
      if(appConfig$auth_ui){
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