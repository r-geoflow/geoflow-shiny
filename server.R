# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  auth_info = reactiveVal(NULL)
  session_reloaded = reactiveVal(FALSE)
  geoflow_configs = reactiveVal(NULL)
  
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  # Read the session cookie
  cookie_observer = observe({
    req(!session_reloaded())
    print("Observing session cookie")
    session_cookie <- cookies::get_cookie("user_profile@geoflow-shiny")
    if (!is.null(session_cookie)) {
      session_data <- jsonlite::fromJSON(session_cookie)
      INFO(sprintf("Found a user profile stored as cookie - try reloading dashboard for user '%s'", session_data$user))
      stored_password <- tryCatch(
        appConfig$keyring_backend$get(service = "geoflow-shiny", username = session_data$user),
        error = function(e) NULL
      )
      if(!is.null(stored_password)){
        INFO(sprintf("Found a password stored in keyring for user '%s'", session_data$user))
        if(any(sapply(appConfig$auth_endpoints, function(x){x$auth_url == session_data$endpoint$auth_url}))){
          endpoint = appConfig$auth_endpoints[sapply(appConfig$auth_endpoints, function(x){x$auth_url == session_data$endpoint$auth_url})][[1]]
          switch(endpoint$auth_type,
                 "ocs" = {
                   #try to reconnect behind the scene
                   AUTH_API = try(ocs4R::ocsManager$new(
                     url = endpoint$auth_url,
                     user = session_data$user,
                     pwd = stored_password
                   ))
                   if(!is(AUTH_API, "try-error")) if(is(AUTH_API, "ocsManager") && !is.null(AUTH_API$getWebdavRoot())) {
                     assign("AUTH_API", AUTH_API, envir = GEOFLOW_SHINY_ENV)
                     shinyjs::show("main-content")
                     shinyjs::hide("login-wrapper")
                     auth_info(session_data)
                     geoflow_configs(getConfigurationFiles(config = appConfig, auth_api = AUTH_API, auth_info = auth_info()))
                   }
                 }       
          )
        }
      }else{
        WARN("No password stored in keyring")
      }
    }
    cookie_observer$destroy()
  })
  
  onStop(function(){
    resetAuthSessionVariables(session)
  })

  if(appConfig$auth){  
    
    if(appConfig$auth_ui){
      
      #auth with UI
      credentials <- authLoginServer(
        id = "login",
        config = appConfig,
        log_out = reactive(logout_init())
      )
      
      observe({
        if (credentials()$user_auth) auth_info(credentials()$auth_info)
      })
  
      # call the logout module with reactive trigger to hide/show
      logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(!is.null(auth_info()))
      )
      
      observe({
        
        if (!is.null(auth_info())) {
          
          if(!is.null(auth_info()) & !session_reloaded()){
            
            print(auth_info())
            initAuthSessionVariables(session, appConfig, auth_info())
            
            #load modules
            INFO("Load home module")
            home_server("home", auth_info, i18n, geoflow_configs, parent.session = session)
            INFO("Load configuration editor module")
            config_editor_server("config_editor", auth_info, i18n, geoflow_configs, parent.session = session)
            INFO("Load configuration list module")
            config_list_server("config_list", auth_info, i18n, geoflow_configs, parent.session = session)
            INFO("Load metadata editor module")
            metadata_editor_server("metadata_editor", auth_info, i18n, geoflow_configs, parent.session = session)
            
            AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
            geoflow_configs(getConfigurationFiles(config = appConfig, auth_api = AUTH_API, auth_info = auth_info()))
          }
          
          shinyjs::show("main-content")
          shinyjs::hide("login-wrapper")
          session_reloaded(TRUE)
          session_data = list(
            endpoint = auth_info()$endpoint,
            user = auth_info()$user
          )
          cookies::set_cookie("user_profile@geoflow-shiny", as.character(jsonlite::toJSON(session_data)), expiration = 7)
          
        } else {
          shinyjs::hide("main-content")
          shinyjs::show("login-wrapper")
        }
      }) 
      
      observeEvent(input$`logout-button`, {
        logout_init()
        shinyjs::hide("main-content")
        shinyjs::show("login-wrapper")
        cookies::remove_cookie("user_profile@geoflow-shiny")
        appConfig$keyring_backend$delete(service = "geoflow-shiny", username = auth_info()$user)
      })
      
    }else{
      #auth without UI
      shinyjs::show("main-content")
      shinyjs::hide("login-wrapper")
      
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
              user = AUTH_API$getUserProfile()$username, 
              token = jwt_profile$access$access_token
            ))
            if(!is.null(auth_info())){
              #load modules
              INFO("Set-up geoflow-shiny in auth mode (no UI, token based)")
              initAuthSessionVariables(session, auth_info())
              INFO("Load home module")
              home_server("home", auth_info, i18n, geoflow_configs, parent.session = session)
              INFO("Load configuration editor module")
              config_editor_server("config_editor", auth_info, i18n, geoflow_configs, parent.session = session)
              INFO("Load configuration list module")
              config_list_server("config_list", auth_info, i18n, geoflow_configs, parent.session = session)
              INFO("Load metadata editor module")
              metadata_editor_server("metadata_editor", auth_info, i18n, geoflow_configs, parent.session = session)
              
              AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
              geoflow_configs(getConfigurationFiles(config = appConfig, auth_api = AUTH_API, auth_info = auth_info()))
            }
          }
        }
      })
    }
    
  }else{
    #anonymous usage
    INFO("Set-up geoflow-shiny in anonymous mode")
    INFO("Load home module")
    home_server("home", i18n = i18n, geoflow_configs = geoflow_configs, parent.session = session)
    INFO("Load configuration editor module")
    config_editor_server("config_editor", i18n = i18n, geoflow_configs = geoflow_configs, parent.session = session)
    INFO("Load configuration list module")
    config_list_server("config_list", i18n = i18n, geoflow_configs = geoflow_configs, parent.session = session)
    INFO("Load metadata editor module")
    metadata_editor_server("metadata_editor", i18n = i18n, geoflow_configs = geoflow_configs, parent.session = session)
    geoflow_configs(getConfigurationFiles(config = appConfig, auth_api = NULL, auth_info = NULL))
    shinyjs::show("main-content")
    shinyjs::hide("login-wrapper")

  }
  
  
  renderSideUI = function(i18n){
    bs4Dash::sidebarMenu(
      id = "geoflow-tabs",
      bs4Dash::menuItem(
        text = i18n()$t("MENU_ITEM_HOME"),
        tabName = "home",
        selected = TRUE
      ),
      bs4Dash::menuItem(
        text = i18n()$t("MENU_ITEM_CREATE"),
        tabName = "config",
        bs4Dash::menuSubItem(text = i18n()$t("MENU_SUBITEM_CREATE_METADATA"), tabName = "metadata_editor"),
        bs4Dash::menuSubItem(text = i18n()$t("MENU_SUBITEM_CREATE_WORKFLOW"), tabName = "config_editor"),
        startExpanded = TRUE
      ),
      bs4Dash::menuItem(
        text = i18n()$t("MENU_ITEM_EXECUTE"),
        tabName = "exec",
        bs4Dash::menuSubItem(text = i18n()$t("MENU_SUBITEM_EXECUTE_WORKFLOW"), tabName = "config_list"),
        startExpanded = TRUE
      )
    )
  }
  
  output$side_ui <- renderUI({
    if(appConfig$auth){
      if(appConfig$auth_ui){
        req(!is.null(auth_info()))
        renderSideUI(i18n())
      }else{
        renderSideUI(i18n())
      }
      
    }else{
      renderSideUI(i18n())
    }
  })
  
  renderMainUI = function(){
    tabItems(
      home_ui("home"),
      config_editor_ui("config_editor"),
      config_list_ui("config_list"),
      metadata_editor_ui("metadata_editor")
    )
  }
  
  output$main_ui <- renderUI({
    if(appConfig$auth){
      if(appConfig$auth_ui){
        req(!is.null(auth_info()))
        renderMainUI()
      }else{
        renderMainUI()
      }
      
    }else{
      renderMainUI()
    }
  })
  
  output$user_avatar <- renderUI({
    tags$a(
      href = "#",
      class = "nav-link",
      style = "float:right;",
      tags$img(
        src = sprintf("%s/avatar/%s/64", auth_info()$endpoint$auth_url, auth_info()$user),
        class = "img-circle",
        style = "height: 32px; width: 32px; margin-top: -10px;"
      )
    )
  })
  
  output$app_logout <- renderUI({
    shiny::actionButton(
      inputId = "logout-button", 
      label = i18n()$t("LOGOUT"), icon = icon("right-from-bracket"), 
      class = "btn-danger"
    )
  })
  
  output$app_language <- renderUI({
    tags$div(
      selectInput(
        "selected_language", label = NULL,
        choices = setNames(
          translator$get_languages()[-1],
          c("English", "Español", "Français")
        ),
        selected = appConfig$lang,
        width = "110px",
        
      ),
      style = "float:right;margin-left:5px;padding-top:2px;"
    )
  })

  #module page management
  session$userData$module <- reactiveVal(NULL)
  
  observe({
    #Update moduleUrl
    moduleUrl <- gsub('_', '-', input$`geoflow-tabs`)
    session$userData$module(moduleUrl)
    updateModuleUrl(session, moduleUrl)
  })
  
  observeEvent(input$selected_language, {
    INFO(paste("Set app language:", input$selected_language))
    shiny.i18n::update_lang(input$selected_language)
    if(appConfig$auth){
      AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
      config_files = getConfigurationFiles(config = appConfig, auth_api = AUTH_API, auth_info = auth_info())
      attr(config_files, "lastModified") = Sys.time()
      geoflow_configs(config_files)
    }else{
      config_files = getConfigurationFiles(config = appConfig, auth_api = NULL, auth_info = NULL)
      attr(config_files, "lastModified") = Sys.time()
      geoflow_configs(config_files)
    }
  })
  
  
  
}
