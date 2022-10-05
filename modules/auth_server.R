#authLoginServer
authLoginServer <- function (id, config, log_out = shiny::reactiveVal(), reload_on_logout = FALSE) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    keyring_backend_name <- if(!is.null(config$auth_keyring_backend)) config$auth_keyring_backend else 'env'
    keyring_backend <- keyring:::known_backends[[keyring_backend_name]]$new()
    
    credentials <- shiny::reactiveValues(
      user_auth = FALSE, 
      user_info = NULL
    )
    shiny::observeEvent(log_out(), {
      if (reload_on_logout) {
        session$reload()
      }else{
        shiny::updateTextInput(session, "auth_password", value = "")
        credentials$user_auth <- FALSE
        credentials$user_info <- NULL
      }
    })
    shiny::observe({
      shinyjs::toggle(id = "panel", condition = !credentials$user_auth)
    })
    shiny::observeEvent(input$auth_button_login,{
      
      auth_endpoint <- config$auth_endpoints[sapply(config$auth_endpoints, function(x){x$auth_url == input$auth_provider})][[1]]
      
      switch(auth_endpoint$auth_type,
             #OCS auth
             "ocs" = {
               shinyjs::disable("auth_button_login")
               AUTH_API <- try(ocs4R::ocsManager$new(
                 url = auth_endpoint$auth_url,
                 user = input$auth_username, pwd = input$auth_password,
                 logger = appConfig$logger
               ))
               if (is(AUTH_API, "ocsManager") && !is.null(AUTH_API$getWebdavRoot())) {
                 assign("AUTH_API", AUTH_API, envir = GEOFLOW_SHINY_ENV)
                 if(!is.null(appConfig$data_dir_remote_user_root)) if(appConfig$data_dir_remote_user_root){
                   INFO(sprintf("Using user '%s' root directory", input$auth_username))
                   appConfig$data_dir_remote <<- paste0(input$auth_username, "/", appConfig$data_dir_remote)
                 }
                 keyring_service <- paste0("geoflow-shiny@", auth_endpoint$auth_url)
                 keyring_backend$set_with_value(keyring_service, username = input$auth_username, password = input$auth_password)
                 
                 credentials$user_auth <- TRUE
                 credentials$auth_info <- list(
                   endpoint = auth_endpoint, 
                   backend = keyring_backend, 
                   service = keyring_service, 
                   user = input$auth_username, 
                   stringsAsFactors = FALSE
                 )
               }else{
                 credentials$user_auth <- FALSE
                 credentials$auth_info <- list(
                   endpoint = character(0),
                   backend = character(0), 
                   service = character(0), 
                   user = character(0), 
                   stringsAsFactors = FALSE
                 )
               }
             },{
               errMsg <- sprintf("No authentication provider for '%s'", auth_endpoint$auth_type)
               ERROR(errMsg)
               stop(errMsg)
             }
      )
      shinyjs::enable("auth_button_login")
      
      if(!credentials$user_auth){
        shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
        shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
      }
    })
    shiny::reactive({
      shiny::reactiveValuesToList(credentials)
    })
  })
}