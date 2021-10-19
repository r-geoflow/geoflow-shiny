# Define server logic required
#==========================================================================================
server <- function(input, output, session) {
  
  print(appConfig$auth)
  if(appConfig$auth){
    INFO("Set-up geoflow-shiny in auth mode")
    INFO(sprintf("Authentication type: %s", appConfig$auth_type))
    INFO(sprintf("Authentication provider: %s", appConfig$auth_url))
    #auth usage
    
    #auth providers
    #-------------------------------------------------------------------------------------------
    #check_credentials_ocs
    check_credentials_ocs <- function(){
      function(user, password){
        disable("auth-go_auth")
        AUTH_API <- try(ocs4R::ocsManager$new(
          url = appConfig$auth_url,
          user = user, pwd = password,
          logger = appConfig$logger
        ))
        if (is(AUTH_API, "ocsManager") && !is.null(AUTH_API$getWebdavRoot())) {
          credentials_df <- data.frame(user = user, password = password, stringsAsFactors = FALSE)
          assign("AUTH_API", AUTH_API, envir = GEOFLOW_SHINY_ENV)
        }else{
          credentials_df <- data.frame(user = character(0), password = character(0), stringsAsFactors = FALSE)
        }
        #enable("auth-go_auth")
        outsec = shinymanager:::check_credentials_df(user, password, credentials_df = credentials_df)
        return(outsec)
      }
    }

    #calling secured shiny server
    result_auth <- shinymanager::secure_server(
      check_credentials = switch(appConfig$auth_type,
        "ocs" = check_credentials_ocs(),
        {
          errMsg <- sprintf("No authentication provider for '%s'", appConfig$auth_type)
          ERROR(errMsg)
          stop(errMsg)
        }
      ),
      session = session,
      timeout = appConfig$auth_timeout
    )
    
    observe({
      user <- reactive({reactiveValuesToList(result_auth)$user})
      AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
      logged <- reactive({if(is(AUTH_API, "try-error")){FALSE}else{TRUE}})
      if(!is.null(user())){
        shiny::callModule(config_editor_server, "config_editor", user, logged, parent.session = session)
        shiny::callModule(config_list_server, "config_list", user, logged, parent.session = session)
      }
    })
    
  }else{
    #anonymous usage
    INFO("Set-up geoflow-shiny in anonymous mode")
    shiny::callModule(config_editor_server, "config_editor")
    shiny::callModule(config_list_server, "config_list")
  }
  
  #module page management
  session$userData$module <- reactiveVal(NULL)
  observe({
    currentModule <- NA
    #look if there is a module in URL, if yes use it
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$module)) {
      currentModule <- query$module
      cat(sprintf("Current module from URL %s\n", currentModule))
    }else{
      if (!is.null(session$userData$module())) {
        currentModule <- session$userData$module()
        cat(sprintf("Current module from userData %s\n", currentModule))
      }
    }
    
    if (!is.na(currentModule)) {
      isolate({updateTabItems(session, "geoflow-tabs", gsub("-", "_", currentModule))})
    } 
  })
  
   
}