#config_editor_server
config_editor_server<- function(id, auth_info, i18n, geoflow_configs, parent.session){

 moduleServer(id, function(input, output, session){
  
  ns <- session$ns

  AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
  
  #contact handlers
  #---------------------------------------------------------------------------------------------
  addContactSource <- function(handler = character(0), source = character(0)){
    if(length(handler)>0) if(!handler %in% geoflow::list_contact_handlers()$id) handler <- ""
    data.frame(
      handler = handler,
      source = source
    )
  }
  
  #entity handlers
  #---------------------------------------------------------------------------------------------
  addEntitySource <- function(handler = character(0), source = character(0)){
    if(length(handler)>0) if(!handler %in% geoflow::list_entity_handlers()$id) handler <- ""
    data.frame(
      handler = handler, 
      source = source
    )
  }
  
  #dictionary handlers
  #---------------------------------------------------------------------------------------------
  addDictionarySource <- function(handler = character(0), source = character(0)){
    if(length(handler)>0) if(!handler %in% geoflow::list_dictionary_handlers()$id) handler <- ""
    data.frame(
      handler = handler, 
      source = source
    )
  }
  
  #configuration loader
  #---------------------------------------------------------------------------------------------
  #loadConfigurationUI
  loadConfigurationUI <- function(config){
    
    #load profile
    cfg_id = if(!is.null(config$profile$id)) config$profile$id else config$id
    ctrl_profile$id <- cfg_id
    cfg_mode = if(!is.null(config$profile$mode)) config$profile$mode else config$mode
    ctrl_profile$mode <- cfg_mode
    ctrl_profile$name <- config$profile$name
    ctrl_profile$project <- config$profile$project
    ctrl_profile$organization <- config$profile$organization
    ctrl_profile$logos <- unlist(config$profile$logos)
    cfg_options = if(!is.null(config$profile$options)) config$profile$options else config$options
    if(!is.null(cfg_options)){
      if(!is.null(cfg_options$line_separator)) ctrl_profile$options$line_separator <- cfg_options$line_separator
      if(!is.null(cfg_options$skipDataDownload)) ctrl_profile$options$skipDataDownload <- as.character(cfg_options$skipDataDownload)
      if(!is.null(cfg_options$skipEnrichWithData)) ctrl_profile$options$skipEnrichWithData <- as.character(cfg_options$skipEnrichWithData)
      if(!is.null(cfg_options$skipEnrichWithDatatypes)) ctrl_profile$options$skipEnrichWithDatatypes <- as.character(cfg_options$skipEnrichWithDatatypes)
      if(!is.null(cfg_options$skipDynamicBbox)) ctrl_profile$options$skipDynamicBbox <- as.character(cfg_options$skipDynamicBbox)
      if(!is.null(cfg_options$enrichDataStrategy)) ctrl_profile$options$enrichDataStrategy <- as.character(cfg_options$enrichDataStrategy)
    }
      
    #load metadata
    #contacts
    config_contacts <- config$metadata$contacts
    if(!is.null(names(config_contacts))) if(!is.list(config_contacts)) config_contacts <- list(config_contacts)
    ctrl_metadata$contacts = do.call("rbind", lapply(config_contacts, function(config_contact){
      addContactSource(handler = config_contact$handler, source = config_contact$source)
    }))
    #entities
    config_entities <- config$metadata$entities
    if(!is.null(names(config_entities))) if(!is.list(config_entities)) config_entities <- list(config_entities)
    ctrl_metadata$entities = do.call("rbind", lapply(config_entities, function(config_entity){
      addEntitySource(handler = config_entity$handler, source = config_entity$source)
    }))
    #dictionary
    config_dictionaries <- config$metadata$dictionary
    if(!is.null(names(config_dictionaries))) if(!is.list(config_dictionaries)) config_dictionaries <- list(config_dictionaries)
    ctrl_metadata$dictionary = do.call("rbind", lapply(config_dictionaries, function(config_dictionary){
      addDictionarySource(handler = config_dictionary$handler, source = config_dictionary$source)
    }))
    
    #load software
    ctrl_software$list <- config$software
    
    #load actions
    ctrl_actions$list <- config$actions
    
  }
  
  #controllers
  #------------------------------------------------------------------------------------
  cloud_overwriting_danger <- reactiveVal(FALSE)
  #ctrl_config
  ctrl_config_file <- reactiveVal(NULL)
  ctrl_config <- reactiveVal(NULL)
  
  #profile controller
  ctrl_profile <- reactiveValues(
    id = NULL,
    mode = "entity",
    name = NULL,
    project = NULL,
    organization = NULL,
    logos = list(),
    options = list(
      line_separator = geoflow::get_line_separator(),
      skipDataDownload = as.character(FALSE),
      skipEnrichWithData = as.character(FALSE),
      skipEnrichWithDatatypes = as.character(FALSE),
      skipDynamicBbox = as.character(FALSE),
      enrichDataStrategy = as.character(FALSE)
    )
  )
  #metadata controller
  ctrl_metadata <- reactiveValues(
    contacts = addContactSource(),
    entities = addEntitySource(),
    dictionary = addDictionarySource()
  )
  #software controller
  ctrl_software <- reactiveValues(
    list = list()
  )
  #actions controller
  ctrl_actions <- reactiveValues(
    list = list()
  )
  #ctrl_validation
  ctrl_validation <- reactiveValues(
    report = data.frame(
      row = integer(0),
      col = character(0),
      type = character(0),
      message = character(0)
    ),
    report_raw = list(),
    data = NULL
  )
  #------------------------------------------------------------------------------------
  #config editor choices
  output$config_editor_choices = renderUI({
    fluidRow(
      bs4Dash::bs4ValueBox(
        width = 6,
        value = h5(i18n()$t("CFG_EDITOR"), style = "font-weight:bold;"), 
        subtitle = i18n()$t("CFG_EDITOR_SUBTITLE"), 
        color = "primary",
        icon = icon("gears"),
        footer = shiny::tagList(
          bs4Dash::actionButton(inputId = ns("create_config"), label = i18n()$t("CFG_EDITOR_CONFIG_CREATE")),
          bs4Dash::actionButton(inputId = ns("load_config"), label = i18n()$t("CFG_EDITOR_CONFIG_LOAD")),
          bs4Dash::actionButton(inputId = ns("save_config"), label = i18n()$t("CFG_EDITOR_CONFIG_SAVE"))
        )
      )
    )
  })

  output$config_editor <- renderUI({
    req(!is.null(ctrl_config_file()))
    bs4Dash::tabsetPanel(
      id = "geoflow_config_blocks", 
      type = "pills",
      shiny::tabPanel(
        value = "profile",
        title = i18n()$t("CFG_EDITOR_PROFILE"),
        br(),
        uiOutput(ns("profile"))
      ),
      shiny::tabPanel(
        value = "metadata",
        title = i18n()$t("CFG_EDITOR_METADATA"),
        br(),
        tabBox(
          width = 12, id = "metadata_tabs",
          type = "tabs", solidHeader = FALSE, status = "teal",
          shiny::tabPanel(
            value = "contacts",
            title = i18n()$t("CFG_EDITOR_METADATA_C"),
            shiny::tagList(
              br(),
              shiny::actionButton(inputId = ns("add_contact"), label = i18n()$t("CFG_EDITOR_METADATA_C_ADD"), class = "btn-primary"),
              shiny::actionButton(inputId = ns("modify_contact"), label = i18n()$t("CFG_EDITOR_METADATA_C_MODIFY"), class = "btn-warning"),
              shiny::actionButton(inputId = ns("delete_contact"), label = i18n()$t("CFG_EDITOR_METADATA_C_DELETE"), class = "btn-danger"),
              DT::DTOutput(ns("tbl_contacts"))
            )
          ),
          shiny::tabPanel(
            value = "entities",
            title = i18n()$t("CFG_EDITOR_METADATA_E"),
            shiny::tagList(
              br(),
              shiny::actionButton(inputId = ns("add_entity"), label = i18n()$t("CFG_EDITOR_METADATA_E_ADD"), class = "btn-primary"),
              shiny::actionButton(inputId = ns("modify_entity"), label = i18n()$t("CFG_EDITOR_METADATA_E_MODIFY"), class = "btn-warning"),
              shiny::actionButton(inputId = ns("delete_entity"), label = i18n()$t("CFG_EDITOR_METADATA_E_DELETE"), class = "btn-danger"),
              DT::DTOutput(ns("tbl_entities"))
            )
          ),
          shiny::tabPanel(
            value = "dictionary",
            title = i18n()$t("CFG_EDITOR_METADATA_D"),
            shiny::tagList(
              br(),
              shiny::actionButton(inputId = ns("add_dictionary"), label = i18n()$t("CFG_EDITOR_METADATA_D_ADD"), class = "btn-primary"),
              shiny::actionButton(inputId = ns("modify_dictionary"), label = i18n()$t("CFG_EDITOR_METADATA_D_MODIFY"), class = "btn-warning"),
              shiny::actionButton(inputId = ns("delete_dictionary"), label = i18n()$t("CFG_EDITOR_METADATA_D_DELETE"), class = "btn-danger"),
              DT::DTOutput(ns("tbl_dictionary"))
            )
          )
        )
      ),
      shiny::tabPanel(
        value = "software",
        title = i18n()$t("CFG_EDITOR_SOFTWARE"),
        br(),
        box(id = "software", width = 12,
            shiny::actionButton(inputId = ns("add_software"), label = i18n()$t("CFG_EDITOR_SOFTWARE_ADD"), class = "btn-primary"),
            shiny::actionButton(inputId = ns("modify_software"), label = i18n()$t("CFG_EDITOR_SOFTWARE_MODIFY"), class = "btn-warning"),
            shiny::actionButton(inputId = ns("delete_software"), label = i18n()$t("CFG_EDITOR_SOFTWARE_DELETE"), class = "btn-danger"),
            DT::DTOutput(ns("tbl_software"))
        )
      ),
      shiny::tabPanel(
        value = "actions",
        title = i18n()$t("CFG_EDITOR_ACTIONS"),
        br(),
        box(id = "actions", width = 12,
            shiny::actionButton(inputId = ns("add_action"), label = i18n()$t("CFG_EDITOR_ACTION_ADD"), class = "btn-primary"),
            shiny::actionButton(inputId = ns("modify_action"), label = i18n()$t("CFG_EDITOR_ACTION_MODIFY"), class = "btn-warning"),
            shiny::actionButton(inputId = ns("delete_action"), label = i18n()$t("CFG_EDITOR_ACTION_DELETE"), class = "btn-danger"),
            DT::DTOutput(ns("tbl_actions"))
        )
      )
    )
  })
  
  #create config button event
  observeEvent(input$create_config,{
    tmpfile = tempfile(fileext = ".json")
    ctrl_config_file(tmpfile)
  })
  
  #load config button event
  observeEvent(input$load_config, {
    shiny::showModal(
      shiny::modalDialog(
        title = i18n()$t("CFG_EDITOR_CONFIG_LOAD"),
        if(appConfig$auth){
          tabsetPanel(
            id = "load_config_modes",
            tabPanel(i18n()$t("CFG_EDITOR_MODE_CLOUD"),
                     tagList(
                       jsTreeR::jstreeOutput(ns("config_load_tree_leavesonly")),
                       actionButton(ns("config_load_tree_leavesonly_select"), label = i18n()$t("CFG_EDITOR_SELECT"), status = "primary", style = "float:right"),
                       actionButton(ns("config_load_tree_leavesonly_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                     )
            ),
            tabPanel(i18n()$t("CFG_EDITOR_MODE_LOCAL"),
                     tagList(
                       fileInput(ns("config_local_file"), label = "File",multiple = FALSE,accept = c(".json",".yml",".yaml"),buttonLabel = i18n()$t("CFG_EDITOR_CHOOSEFILE")),
                       actionButton(ns("config_local_file_select"), label = i18n()$t("CFG_EDITOR_SELECT"), status = "primary", style = "float:right"),
                       actionButton(ns("config_local_file_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                     )
            )
          )
        }else{
          tabsetPanel(
            id = "load_contact_tables_modes",
            tabPanel(i18n()$t("CFG_EDITOR_MODE_LOCAL"),
                     tagList(
                       fileInput(ns("config_local_file"), label = "File",multiple = FALSE,accept = c(".json",".yml",".yaml"),buttonLabel = i18n()$t("CFG_EDITOR_CHOOSEFILE")),
                       actionButton(ns("config_local_file_select"), label = i18n()$t("CFG_EDITOR_SELECT"), status = "primary", style = "float:right"),
                       actionButton(ns("config_local_file_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                     )
            )
          )
        },
        easyClose = FALSE, footer = NULL 
      )
    )
  })
  
  loadCloudTree(id = "config_load_tree_leavesonly", config = appConfig, auth_api = AUTH_API, 
                mime_types = c(".json", ".yml", ".yaml"), leaves_only = TRUE, output = output)
  
  observeEvent(input$config_local_file_cancel, {
    shiny::removeModal()
  })
  observeEvent(input$config_load_tree_leavesonly_cancel,{
    shiny::removeModal()
  })
  observeEvent(input$config_load_tree_leavesonly_select,{
    selected_resource = input$config_load_tree_leavesonly_selected
    print(selected_resource)
    #OCS download selected resource and read it
    filepath <- AUTH_API$downloadFile(relPath = dirname(selected_resource[[1]]$data), filename = selected_resource[[1]]$text, outdir = tempdir())
    config <- try(switch(mime::guess_type(filepath),
                        "application/json" = jsonlite::read_json(filepath),
                        "application/yaml" = yaml::read_yaml(filepath)
    ))
    attr(config, "filepath") <- filepath
    print(filepath)
    
    #load configuration UI
    ctrl_config_file(attr(config, "filepath"))
    loadConfigurationUI(config)
    
    shiny::removeModal()
    loadCloudTree(id = "config_load_tree_leavesonly", config = appConfig, auth_api = AUTH_API, 
                  mime_types = c(".json", ".yml", ".yaml"), leaves_only = TRUE, output = output)
  })
  observeEvent(input$config_local_file_select,{
    req(!is.null(input$config_local_file))
    
    #read local resource
    filepath <- input$config_local_file$datapath
    config <- try(switch(mime::guess_type(filepath),
                         "application/json" = jsonlite::read_json(filepath),
                         "application/yaml" = yaml::read_yaml(filepath)
    ))
    attr(config, "filepath") <- filepath
    #load configuration UI
    ctrl_config_file(attr(config, "filepath"))
    loadConfigurationUI(config)
    
    shiny::removeModal()
    
  })
  
  
  #SAVE
  #save config button event
  observeEvent(input$save_config, {
    cloud_overwriting_danger(FALSE)
    shiny::showModal(
      shiny::modalDialog(
        title = i18n()$t("CFG_EDITOR_CONFIG_SAVE"),
        if(appConfig$auth){
          tabsetPanel(
            id = "save_config_modes",
            tabPanel(i18n()$t("CFG_EDITOR_MODE_CLOUD"),
                     tagList(
                       selectizeInput(inputId = ns("config_filemimetype"), label = i18n()$t("CFG_EDITOR_FORMAT"), choices = c("YAML" = "yaml", "JSON" = "json"), selected = "yaml"),
                       textInput(ns("config_filename"), label = i18n()$t("CFG_EDITOR_FILENAME"), value = sprintf("new_config.%s", input$config_filemimetype), width = NULL),
                       hr(),
                       jsTreeR::jstreeOutput(ns("config_load_tree")),
                       uiOutput(ns("config_load_tree_upload_action")),
                       actionButton(ns("config_load_tree_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                     )
            ),
            tabPanel(i18n()$t("CFG_EDITOR_MODE_LOCAL"),
                     tagList(
                       selectizeInput(inputId = ns("config_local_filemimetype"), label = i18n()$t("CFG_EDITOR_FORMAT"), choices = c("YAML" = "yaml", "JSON" = "json"), selected = "yaml"),
                       textInput(ns("config_local_filename"), label = i18n()$t("CFG_EDITOR_FILENAME"), value = sprintf("new_config.%s", input$config_local_filemimetype), width = NULL),
                       hr(),
                       uiOutput(ns("config_local_save_action")),
                       actionButton(ns("config_local_save_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                     )
            )
          )
        }else{
          tabsetPanel(
            id = "save_config_modes",
            tabPanel(i18n()$t("CFG_EDITOR_MODE_LOCAL"),
                     tagList(
                       selectizeInput(inputId = ns("config_local_filemimetype"), label = i18n()$t("CFG_EDITOR_FORMAT"), choices = c("YAML" = "yaml", "JSON" = "json"), selected = "yaml"),
                       textInput(ns("config_local_filename"), label = i18n()$t("CFG_EDITOR_FILENAME"), value = sprintf("new_config.%s", input$config_local_filemimetype), width = NULL),
                       hr(),
                       uiOutput(ns("config_local_save_action")),
                       actionButton(ns("config_local_save_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                     )
            )
          )
        },
        easyClose = FALSE, footer = uiOutput(ns("overwriting_file_danger")) 
      )
    )
  })
  
  observeEvent(input$config_filemimetype,{
    updateTextInput(inputId = "config_filename", value = sprintf("new_config.%s", input$config_filemimetype))
  })
  
  loadCloudTree(id = "config_load_tree", config = appConfig, auth_api = AUTH_API, 
                mime_types = c(".json", ".yml", ".yaml"), leaves_only = FALSE, output = output)
  
  output$config_load_tree_upload_action <- renderUI({
    if(length(input$config_load_tree_selected)>0){
      actionButton(ns("config_load_tree_upload"), label = i18n()$t("CFG_EDITOR_UPLOAD"), status = "primary", style = "float:right")
    }else{
      disabled(actionButton(ns("config_load_tree_upload"), label = i18n()$t("CFG_EDITOR_UPLOAD"), 
                            title = i18n()$t("CFG_EDITOR_UPLOAD_TOOLTIP"),
                            status = "primary", style = "float:right"))
    }
  })
  observe({
    if(length(input$config_load_tree_selected)>0){
      selected_resource = input$config_load_tree_selected[[1]]
      if(selected_resource$type == "file"){
        shiny::updateTextInput(inputId = "config_filename", value = basename(selected_resource$data))
        cloud_overwriting_danger(TRUE)
      }else if(selected_resource$type == "folder"){
        files = AUTH_API$listFiles(relPath = selected_resource$data)
        if(input$config_filename %in% files$name){
          cloud_overwriting_danger(TRUE)
        }else{
          cloud_overwriting_danger(FALSE)
        }
      }
    }
  })
  observeEvent(input$config_load_tree_cancel,{
    shiny::removeModal()
    cloud_overwriting_danger(FALSE)
  })
  observeEvent(input$config_load_tree_upload,{
    req(length(input$config_load_tree_selected)>0)
    selected_resource = input$config_load_tree_selected[[1]]
    
    cfg = getConfiguration()
    file <- file.path(tempdir(), input$config_filename)
    switch(mime::guess_type(input$config_filename),
           "application/json" = {
             jsonlite::write_json(cfg, file, auto_unbox = TRUE, pretty = TRUE)
           },
           "application/yaml" = {
             yaml::write_yaml(cfg, file)
           }
    )
    uploaded = AUTH_API$uploadFile(
      filename = file,
      relPath = if(selected_resource$type == "folder"){
        selected_resource$data
      }else if(selected_resource$type == "file"){
        dirname(selected_resource$data)
      }
    )
    shiny::removeModal()
    loadCloudTree(id = "config_load_tree", config = appConfig, auth_api = AUTH_API, leaves_only = FALSE, output = output)
    cloud_overwriting_danger(FALSE)
  })
  
  observeEvent(input$config_local_filemimetype,{
    updateTextInput(inputId = "config_local_filename", value = sprintf("new_config.%s", input$config_local_filemimetype))
  })
  output$config_local_save_action <- renderUI({
    req(input$config_local_filename)
    downloadButtonCustom(
      outputId = ns("config_local_save"), 
      label = i18n()$t("CFG_EDITOR_SAVE"), icon = NULL,
      class = "btn btn-primary", style = "float:right"
    )
  })
  output$config_local_save <- downloadHandler(
    filename = function(){ 
      print("PIPO")
      print(input$config_local_filename)
      input$config_local_filename
    },
    content = function(con){
      cfg = getConfiguration()
      switch(mime::guess_type(con),
             "application/json" = {
               jsonlite::write_json(cfg, con, auto_unbox = TRUE, pretty = TRUE)
             },
             "application/yaml" = {
               yaml::write_yaml(cfg, con)
             }
      )
    }
  )
  
  observeEvent(input$config_local_save_cancel,{
    shiny::removeModal()
    cloud_overwriting_danger(FALSE)
  })
  
  
  #PROFILE
  #=====================================================================================
  output$profile <- renderUI({
    fluidRow(
      tabBox(
        width = 6, id = "profile_settings",
        type = "tabs", solidHeader = FALSE, status = "teal",
        shiny::tabPanel(
          id = "profile_settings_main",
          title = i18n()$t("CFG_EDITOR_PROFILE_MAIN_SETTINGS"),
          textInput(inputId = ns("profile_id"), label = i18n()$t("CFG_EDITOR_PROFILE_WORKFLOW_IDENTIFIER"), value = ctrl_profile$id),
          selectizeInput(inputId = ns("profile_mode"), label = i18n()$t("CFG_EDITOR_PROFILE_WORKFLOW_MODE"), choices = c("raw", "entity"), selected = ctrl_profile$mode)
        ),
        shiny::tabPanel(
          id = "profile_settings_optional",
          title = i18n()$t("CFG_EDITOR_PROFILE_OPTIONAL_SETTINGS"),
          textInput(inputId = ns("profile_option_line_separator"), label = i18n()$t("CFG_EDITOR_PROFILE_MD_LINE_SEPARATOR"), value = ctrl_profile$options$line_separator),
          selectizeInput(inputId = ns("profile_option_skipDataDownload"), label = i18n()$t("CFG_EDITOR_PROFILE_SKIP_DATA_DOWNLOAD"), choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipDataDownload),
          selectizeInput(inputId = ns("profile_option_skipEnrichWithData"), label = i18n()$t("CFG_EDITOR_PROFILE_SKIP_ENRICH_WITH_DATA"), choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipEnrichWithData),
          selectizeInput(inputId = ns("profile_option_skipEnrichWithDatatypes"), label = i18n()$t("CFG_EDITOR_PROFILE_SKIP_ENRICH_WITH_DATATYPES"), choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipEnrichWithDatatypes),
          selectizeInput(inputId = ns("profile_option_skipDynamicBbox"), label = i18n()$t("CFG_EDITOR_PROFILE_SKIP_DYNAMIC_BBOX"), choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipDynamicBbox),
          selectizeInput(inputId = ns("profile_option_enrichDataStrategy"), label = i18n()$t("CFG_EDITOR_PROFILE_ENRICH_DATA_STRATEGY"), choices = c("first", "union"), selected = ctrl_profile$options$enrichDataStrategy)
        )
      ),
      tabBox(
        width = 6, id = "profile_additional_information", 
        type = "tabs", solidHeader = FALSE, status = "teal",
        shiny::tabPanel(
          id = "profile_additional_information",
          title = i18n()$t("CFG_EDITOR_PROFILE_ADDITIONAL_INFO"),
          textInput(inputId = ns("profile_name"), label = i18n()$t("CFG_EDITOR_PROFILE_ADDITIONAL_INFO_NAME"), value = ctrl_profile$name),
          textInput(inputId = ns("profile_project"), label = i18n()$t("CFG_EDITOR_PROFILE_ADDITIONAL_INFO_PROJECT"), value = ctrl_profile$project),
          textInput(inputId = ns("profile_organization"), label = i18n()$t("CFG_EDITOR_PROFILE_ADDITIONAL_INFO_ORG"), value = ctrl_profile$organization),
          selectizeInput(
            inputId = ns("profile_logos"), 
            label = i18n()$t("CFG_EDITOR_PROFILE_ADDITIONAL_INFO_LOGOS"), 
            choices = ctrl_profile$logos, 
            selected = ctrl_profile$logos,
            multiple = TRUE, 
            options = list(
              create = TRUE,
              render = I("{
                    item: function(item, escape) {
                      return '<div><img src=\"'+item.value+'\" height=25 /> ' + item.value + '</div>'; 
                    }
                  }"
              )
            )
          )
        )
      )
    )
  })
  
  #CONFIG VALIDATION/TEST
  #=====================================================================================  
  loadConfiguration <- function(){
    geoflow_config <- try(geoflow::initWorkflow(ctrl_config_file(), handleMetadata = FALSE, session = parent.session, dir = tempdir()))
    if(!is(geoflow_config, "try-error")){
      ctrl_config(geoflow_config)
    }
  }
  
  #METADATA
  #=====================================================================================
  output$validation_report_DT = DT::renderDataTable(
    DT::datatable(
      ctrl_validation$report,
      extensions = 'RowGroup',
      options = list(rowGroup = list(dataSrc = 0)),
      selection = 'none',
      rownames= FALSE
    ) %>% formatStyle('type', target = 'row', backgroundColor = styleEqual(c("WARNING", "ERROR"), c('#fff3cd', '#f8d7da'))),
    options = list(
      lengthChange = FALSE
    )
  )
  output$validation_report_HT <- rhandsontable::renderRHandsontable({
    #check if any warning
    rows_with_warning <- c()
    cols_with_warning <- c()
    report_with_warning <- ctrl_validation$report[ctrl_validation$report$type == "WARNING",]
    if(nrow(report_with_warning)>0){
      report_with_warning <- unique(report_with_warning[,c("row", "col")])
      report_with_warning$row <- sapply(report_with_warning$row, function(x){as.integer(gsub("Row ", "", x))})
      report_with_warning$col <- sapply(report_with_warning$col, function(x){which(colnames(ctrl_validation$report_data)==x)})
      row.names(report_with_warning) <- NULL
      report_with_warning <- as.matrix(report_with_warning)
      rows_with_warning <- report_with_warning[,1]
      cols_with_warning <- report_with_warning[,2]
    }
    #check if any error
    rows_with_error <- c()
    cols_with_error <- c()
    report_with_error <- ctrl_validation$report[ctrl_validation$report$type == "ERROR",]
    if(nrow(report_with_error)>0){
      print(report_with_error)
      report_with_error <- unique(report_with_error[,c("row", "col")])
      report_with_error$row <- sapply(report_with_error$row, function(x){as.integer(gsub("Row ", "", x))})
      report_with_error$col <- sapply(report_with_error$col, function(x){which(colnames(ctrl_validation$report_data)==x)})
      row.names(report_with_error) <- NULL
      report_with_error <- as.matrix(report_with_error)
      rows_with_error <- report_with_error[,1]
      cols_with_error <- report_with_error[,2]
      print(report_with_error)
    }
    #create handsontable
    out_tbl <- rhandsontable::rhandsontable(
      ctrl_validation$report_data, 
      readOnly = TRUE,
      rows_with_warning = rows_with_warning-1,
      cols_with_warning = cols_with_warning-1,
      rows_with_error = rows_with_error-1,
      cols_with_error = cols_with_error-1
    ) %>%
      hot_cols(
        fixedColumnsLeft = 1,
        colWidths = 200,
        manualColumnResize = TRUE,
        renderer = "
                function (instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (instance.params) {
                        //manage cells that are valid
                        var cell_valid = true;
                        
                        //manage cells with warnings
                        console.log('Warnings');
                        console.log('Warning rows');
                        row_warning_to_highlight = instance.params.rows_with_warning
                        row_warning_to_highlight = row_warning_to_highlight instanceof Array ? row_warning_to_highlight : [row_warning_to_highlight]
                        console.log(row_warning_to_highlight);
                        console.log('Warning cols');
                        col_warning_to_highlight = instance.params.cols_with_warning
                        col_warning_to_highlight = col_warning_to_highlight instanceof Array ? col_warning_to_highlight : [col_warning_to_highlight]
                        console.log(col_warning_to_highlight);
                        for (var i=0; i < row_warning_to_highlight.length; i++) {
                            for(var j=0; j < col_warning_to_highlight.length; j++){
                              var warning_row = row_warning_to_highlight.length == 1? row_warning_to_highlight[i].row : row_warning_to_highlight[i];
                              var warning_col = col_warning_to_highlight.length == 1? col_warning_to_highlight[i].col: col_warning_to_highlight[i];
                              if (warning_row == row && warning_col == col) {
                                  $(td).addClass('cell_with_warning');
                                  cell_valid = false;
                                  break; break;
                              }
                            }
                        }
                        
                        //manage cells with errors
                        console.log('Errors');
                        console.log('Error rows');
                        row_error_to_highlight = instance.params.rows_with_error
                        row_error_to_highlight = row_error_to_highlight instanceof Array ? row_error_to_highlight : [row_error_to_highlight]
                        console.log(row_error_to_highlight);
                        console.log('Error cols');
                        col_error_to_highlight = instance.params.cols_with_error
                        col_error_to_highlight = col_error_to_highlight instanceof Array ? col_error_to_highlight : [col_error_to_highlight]
                        console.log(col_error_to_highlight);
                        for (var i = 0; i < row_error_to_highlight.length; i++) {
                            for(var j = 0; j < col_error_to_highlight.length; j++){
                              var error_row = row_error_to_highlight.length == 1? row_error_to_highlight[i].row : row_error_to_highlight[i];
                              var error_col = col_error_to_highlight.length == 1? col_error_to_highlight[i].col: col_error_to_highlight[i];
                              if (error_row == row && error_col == col) {
                                  $(td).addClass('cell_with_error');
                                  cell_valid = false;
                                  break; break;
                              }
                            }
                        }
                        
                        if(cell_valid) $(td).addClass('cell_valid');
                    }
                }") 
    for(i in 1:nrow(ctrl_validation$report_data)){
      for(j in 1:ncol(ctrl_validation$report_data)){
        cell_validator <- ctrl_validation$report_raw[sapply(ctrl_validation$report_raw, function(x){x$i == i && x$j == j})][[1]]
        cell_validation_report <- cell_validator$validate()
        if(nrow(cell_validation_report)==0){
          cell_validation_report <- NULL
        }else{
          cell_validation_report <- paste0(sapply(1:nrow(cell_validation_report), function(idx){
            paste0("- ", cell_validation_report[idx, "type"], ": ", cell_validation_report[idx, "message"])
          }), collapse="\n")
        }
        out_tbl <- out_tbl %>% 
          hot_cell(i, j, comment = cell_validation_report)
      }
    }

    out_tbl
  })
  #showValidationModal
  showValidationModal <- function(type, handler, source){
    INFO(sprintf("Validation for '%s' (%s)", source, type))
    #load configuration
    INFO("Reload configuration")
    loadConfiguration()
    #get metadata handler
    INFO("Load metadata handler")
    md_handler <- geoflow::loadMetadataHandler(config = ctrl_config(), type = type, element = list(handler = handler, source = source))
    #get source data only (no handling of geoflow objects)
    INFO("Load metadata objects")
    md_data <- md_handler$fun(handler = md_handler, config = ctrl_config(), source = source, handle = FALSE)
    #get metadata validator
    INFO("Load metadata validator")
    md_validator <- switch(type,
      "contacts" = geoflow::geoflow_validator_contacts$new(source = md_data),
      "entities" = geoflow::geoflow_validator_entities$new(source = md_data)
    )
    #validate
    hasReport <- FALSE
    md_validation_message <- NULL
    INFO("Validate data structure")
    md_structure_status <- md_validator$validate_structure()
    if(!md_structure_status){
      md_validation_message <- tags$span(unlist(strsplit(attr(md_structure_status, "message"),": "))[2], style="color:red;font-weight:bold;")
    }else{
      INFO("Validate data content")
      md_content_report <- md_validator$validate_content()
      if(nrow(md_content_report)==0){
        md_validation_message <- tags$span(i18n()$t("CFG_EDITOR_METADATA_VALIDATION_OK"), style = "color:green;font-weight:bold;")
      }else{
        md_content_report$row <- paste(i18n()$t("CFG_EDITOR_METADATA_VALIDATION_ROW"), md_content_report$row)
        ctrl_validation$report <- md_content_report
        ctrl_validation$report_raw <- md_validator$validate_content(raw = TRUE)
        ctrl_validation$report_data <- md_data
        hasReport <- TRUE
      }
    }
    
    showModal(
      modalDialog(
        title = sprintf(i18n()$t("CFG_EDITOR_METADATA_VALIDATION_REPORT"), type),
        tags$b(paste0(i18n()$t("CFG_EDITOR_SOURCE"),": ")), tags$b(tags$a(href = source, source)),hr(),
        if(hasReport){
          bs4Dash::tabsetPanel(
            type = "pills",
            shiny::tabPanel(
              title = i18n()$t("CFG_EDITOR_METADATA_VALIDATION_SMARTVIEW"), hr(),
              rhandsontable::rHandsontableOutput(ns("validation_report_HT"))
            ),
            shiny::tabPanel(
              title = i18n()$t("CFG_EDITOR_METADATA_VALIDATION_RAWVIEW"), hr(),
              DT::dataTableOutput(ns("validation_report_DT"))
            )
          )
          
        }else{
          md_validation_message
        },
        easyClose = TRUE, footer = NULL, size = "l"
      )
    )
  }
  #manage button handlers
  manageButtonValidateEvents <- function(data, type, uuids){
    prefix <- paste0("button_validate_", type,"_")
    if(nrow(data)>0) lapply(1:nrow(data),function(i){
      x <- data[i,]
      button_id <- paste0(prefix,uuids[i])
      observeEvent(input[[button_id]],{
        showValidationModal(type, x$handler, x$source)
      })
    })
  }
  
  #metadata table handler
  metadataTableHandler <- function(data, type, uuids, validate = TRUE){
    
    #DT::datatable({
      colnames(data) <- c(i18n()$t("CFG_EDITOR_HANDLER"), i18n()$t("CFG_EDITOR_SOURCE"))
      if(validate) if(nrow(data)>0){
        data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              Handler = data[i, i18n()$t("CFG_EDITOR_HANDLER")],
              Source = data[i, i18n()$t("CFG_EDITOR_SOURCE")],
              Actions = as(actionButton(inputId = ns(paste0('button_validate_',type,'_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                             title = i18n()$t("CFG_EDITOR_METADATA_VALIDATION_CHECK"), label = "", icon = icon("tasks")),"character")
            )
            return(out_tib)
          }
        ))
      }
      return(data)
  }
  
  #renderMetadataTable
  renderMetadataTable <- function(data, type, validate){
    
    uuids <- NULL
    if(!is.null(data)) if(nrow(data)>0) for(i in 1:nrow(data)){
      one_uuid = uuid::UUIDgenerate() 
      uuids <- c(uuids, one_uuid)
    }
    
    output[[paste0("tbl_", type)]] <- DT::renderDT(
      metadataTableHandler(data, type, uuids, validate),
      selection='single', escape=FALSE,rownames=FALSE,
      options=list(
        lengthChange = FALSE,
        paging = FALSE,
        searching = FALSE,
        preDrawCallback = JS(
          'function() {
                  Shiny.unbindAll(this.api().table().node()); }'
        ),
        drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); }'
        ),
        autoWidth = FALSE,
        columnDefs = Filter(Negate(is.null),list(
          list(width = '100px', targets = c(0)),
          list(width = '400px', targets = c(1),
               render = JS("function(data, type, full, meta) {
                           var html = data;
                           if(data.startsWith(\"http://\") | data.startsWith(\"https://\")){
                              html = '<a href=\"' + data + '\" target=\"_blank\">'+data+'</a>';
                           }
                           var gsheet_handler = full[0] == \"gsheet\";
                           if(gsheet_handler) if(!data.startsWith(\"https://docs.google.com/spreadsheets\")){
                              html += '<br><div style=\"color:red;padding:2px;\" role=\"alert\">Invalid Google spreadsheets link</div>';
                           }
                           return html;
                        }")),
          if(validate) list(width = '50px', targets = c(2)) else NULL
        ))
      )
    )
    if(validate) manageButtonValidateEvents(data, type, uuids)
  }
  
  #render tables
  observe({
    renderMetadataTable(ctrl_metadata$contacts, "contacts", TRUE)
    renderMetadataTable(ctrl_metadata$entities, "entities", TRUE)
    renderMetadataTable(ctrl_metadata$dictionary, "dictionary", FALSE)
  })
  
  #contacts
  #-----------------------------------------------------------------------------------------------------
  #contact form
  showContactModal <- function(new = TRUE, handler = "", source = ""){
    title = ifelse(new,i18n()$t("CFG_EDITOR_ADD"),i18n()$t("CFG_EDITOR_MODIFY"))
    i18n_key_suffix <- ifelse(new, "ADD", "MODIFY")
    form_action <- tolower(title)
    showModal(modalDialog(title = i18n()$t(sprintf("CFG_EDITOR_METADATA_C_%s", i18n_key_suffix)),
                          selectInput(ns("contact_form_handler"), i18n()$t("CFG_EDITOR_HANDLER"),choices=geoflow::list_contact_handlers()$id, selected = handler),
                          textInput(ns("contact_form_source"), i18n()$t("CFG_EDITOR_SOURCE"), value = source),
                          actionButton(ns("contact_form_cancel"), i18n()$t("CFG_EDITOR_CANCEL")),
                          actionButton(ns(sprintf("contact_%s_go", form_action)), title, style = "float:right"),
                          easyClose = FALSE, footer = NULL ))
  }
  observeEvent(input$contact_form_cancel, {
    removeModal()
  })
  #contact/add
  observeEvent(input$add_contact,{
    showContactModal(new = TRUE)
  })
  observeEvent(input$contact_add_go, {
    new_contact <- addContactSource(handler = input$contact_form_handler, source = input$contact_form_source)
    ctrl_metadata$contacts <- rbind(ctrl_metadata$contacts, new_contact)
    removeModal()
  })
  #contact/modify
  observeEvent(input$modify_contact,{
    if(length(input$tbl_contacts_rows_selected)>=1 ){
      contact_sel <- ctrl_metadata$contacts[input$tbl_contacts_rows_selected,]
      showContactModal(new = FALSE, handler = contact_sel$handler, contact_sel$source)
    }else{
      modalDialog(
        title = i18n$t("CFG_EDITOR_WARNING"),
        i18n()$t("CFG_EDITOR_WARNING_ROW"),easyClose = TRUE
      )
    }
  })
  observeEvent(input$contact_modify_go, {
    mod_contact <- addContactSource(handler = input$contact_form_handler, source = input$contact_form_source)
    ctrl_metadata$contacts[input$tbl_contacts_rows_selected,"handler"] <- mod_contact$handler
    ctrl_metadata$contacts[input$tbl_contacts_rows_selected,"source"] <- mod_contact$source
    removeModal()
  })
  #contact/delete
  observeEvent(input$delete_contact,{
    showModal(
      if(length(input$tbl_contacts_rows_selected)>=1 ){
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_CONFIRMATION"),
          footer = tagList(
            modalButton(i18n()$t("CFG_EDITOR_CANCEL")),
            actionButton(ns("contact_delete_go"), "Yes", style = "float:right")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_SELECTION"),easyClose = TRUE
        )
      }
    )
  })
  observeEvent(input$contact_delete_go, {
    ctrl_metadata$contacts=ctrl_metadata$contacts[-input$tbl_contacts_rows_selected, ]
    removeModal()
  })
  
  
  #entities
  #----------------------------------------------------------------------------------------------------
  #entity form
  showEntityModal <- function(new = TRUE, handler = "", source = ""){
    title = ifelse(new,i18n()$t("CFG_EDITOR_ADD"),i18n()$t("CFG_EDITOR_MODIFY"))
    i18n_key_suffix <- ifelse(new, "ADD", "MODIFY")
    form_action <- tolower(title)
    showModal(modalDialog(title = i18n()$t(sprintf("CFG_EDITOR_METADATA_E_%s", i18n_key_suffix)),
                          selectInput(ns("entity_form_handler"), i18n()$t("CFG_EDITOR_HANDLER"),choices=geoflow::list_entity_handlers()$id, selected = handler),
                          textInput(ns("entity_form_source"), i18n()$t("CFG_EDITOR_SOURCE"), value = source),
                          actionButton(ns("entity_form_cancel"), i18n()$t("CFG_EDITOR_CANCEL")),
                          actionButton(ns(sprintf("entity_%s_go", form_action)), title, style = "float:right"),
                          easyClose = FALSE, footer = NULL ))
  }
  observeEvent(input$entity_form_cancel, {
    removeModal()
  })
  #entity/add
  observeEvent(input$add_entity,{
    showEntityModal(new = TRUE)
  })
  observeEvent(input$entity_add_go, {
    new_entity <- addEntitySource(handler = input$entity_form_handler, source = input$entity_form_source)
    ctrl_metadata$entities <- rbind(ctrl_metadata$entities, new_entity)
    removeModal()
  })
  #entity/modify
  observeEvent(input$modify_entity,{
    if(length(input$tbl_entities_rows_selected)>=1 ){
      entity_sel <- ctrl_metadata$entities[input$tbl_entities_rows_selected,]
      showEntityModal(new = FALSE, handler = entity_sel$handler, entity_sel$source)
    }else{
      modalDialog(
        title = i18n()$t("CFG_EDITOR_WARNING"),
        i18n()$t("CFG_EDITOR_WARNING_ROW"),
        easyClose = TRUE
      )
    }
  })
  observeEvent(input$entity_modify_go, {
    mod_entity <- addEntitySource(handler = input$entity_form_handler, source = input$entity_form_source)
    ctrl_metadata$entities[input$tbl_entities_rows_selected,"handler"] <- mod_entity$handler
    ctrl_metadata$entities[input$tbl_entities_rows_selected,"source"] <- mod_entity$source
    removeModal()
  })
  #entity/delete
  observeEvent(input$delete_entity,{
    showModal(
      if(length(input$tbl_entities_rows_selected)>=1 ){
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_CONFIRMATION"),
          footer = tagList(
            modalButton(i18n()$t("CFG_EDITOR_CANCEL")),
            actionButton(ns("entity_delete_go"), "Yes", style = "float:right")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_SELECTION"),easyClose = TRUE
        )
      }
    )
  })
  observeEvent(input$entity_delete_go, {
    ctrl_metadata$entities=ctrl_metadata$entities[-input$tbl_entities_rows_selected, ]
    removeModal()
  })
  
  #dictionary
  #-----------------------------------------------------------------------------------------------------
  #dictionary form
  showDictionaryModal <- function(new = TRUE, handler = "", source = ""){
    title = ifelse(new,i18n()$t("CFG_EDITOR_ADD"),i18n()$t("CFG_EDITOR_MODIFY"))
    i18n_key_suffix <- ifelse(new, "ADD", "MODIFY")
    form_action <- tolower(title)
    showModal(modalDialog(title = i18n()$t(sprintf("CFG_EDITOR_METADATA_D_%s", i18n_key_suffix)),
                          selectInput(ns("dictionary_form_handler"), i18n()$t("CFG_EDITOR_HANDLER"),choices=geoflow::list_dictionary_handlers()$id, selected = handler),
                          textInput(ns("dictionary_form_source"), i18n()$t("CFG_EDITOR_SOURCE"), value = source),
                          actionButton(ns("dictionary_form_cancel"), i18n()$t("CFG_EDITOR_CANCEL")),
                          actionButton(ns(sprintf("dictionary_%s_go", form_action)), title, style = "float:right"),
                          easyClose = FALSE, footer = NULL ))
  }
  observeEvent(input$dictionary_form_cancel,{
    removeModal()
  })
  #dictionary/add
  observeEvent(input$add_dictionary,{
    showDictionaryModal(new = TRUE)
  })
  observeEvent(input$dictionary_add_go, {
    new_dictionary <- addDictionarySource(handler = input$dictionary_form_handler, source = input$dictionary_form_source)
    ctrl_metadata$dictionary <- rbind(ctrl_metadata$dictionary, new_dictionary)
    removeModal()
  })
  #dictionary/modify
  observeEvent(input$modify_dictionary,{
    if(length(input$tbl_dictionary_rows_selected)>=1 ){
      dictionary_sel <- ctrl_metadata$dictionary[input$tbl_dictionary_rows_selected,]
      showDictionaryModal(new = FALSE, handler = dictionary_sel$handler, dictionary_sel$source)
    }else{
      modalDialog(
        title = i18n()$t("CFG_EDITOR_WARNING"),
        i18n()$t("CFG_EDITOR_WARNING_ROW"),easyClose = TRUE
      )
    }
  })
  observeEvent(input$dictionary_modify_go, {
    mod_dictionary <- addDictionarySource(handler = input$dictionary_form_handler, source = input$dictionary_form_source)
    ctrl_metadata$dictionary[input$tbl_dictionary_rows_selected,"handler"] <- mod_dictionary$handler
    ctrl_metadata$dictionary[input$tbl_dictionary_rows_selected,"source"] <- mod_dictionary$source
    removeModal()
  })
  #dictionary/delete
  observeEvent(input$delete_dictionary,{
    showModal(
      if(length(input$tbl_dictionary_rows_selected)>=1 ){
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_CONFIRMATION"),
          footer = tagList(
            modalButton(i18n()$t("CFG_EDITOR_CANCEL")),
            actionButton(ns("dictionary_delete_go"), "Yes", style = "float:right")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_SELECTION"),easyClose = TRUE
        )
      }
    )
  })
  observeEvent(input$dictionary_delete_go, {
    ctrl_metadata$dictionary=ctrl_metadata$dictionary[-input$tbl_dictionary_rows_selected, ]
    removeModal()
  })
  
  #SOFTWARE
  #=====================================================================================
  #software summary table
  #----------------------------------------------------------------------------------------------------
  output$tbl_software = DT::renderDT({
      DT::datatable(
        {
          in_software <- cbind(
            ' ' = if(length(ctrl_software$list)>0) '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>' else character(0),
            'json' = sapply(ctrl_software$list, function(x){ as.character(jsonlite::toJSON(x, auto_unbox = TRUE)) }),
            data.frame(
              id = if(length(ctrl_software$list)>0) sapply(ctrl_software$list, function(x){x$id}) else character(0),
              type = if(length(ctrl_software$list)>0) sapply(ctrl_software$list, function(x){x$type}) else character(0),
              software_type = if(length(ctrl_software$list)>0) sapply(ctrl_software$list, function(x){x$software_type}) else character(0)
            )
          )
          in_software$def = if(length(ctrl_software$list)>0) geoflow::list_software()[sapply(in_software$software_type, function(x){which(x ==geoflow::list_software()$software_type)}),]$definition else character(0)
          colnames(in_software)[colnames(in_software) %in% c("id","type","software_type","def")] <- c(i18n()$t("CFG_EDITOR_SOFTWARE_IDENTIFIER"), i18n()$t("CFG_EDITOR_SOFTWARE_TYPE"), i18n()$t("CFG_EDITOR_SOFTWARE_SOFTWARETYPE"), i18n()$t("CFG_EDITOR_SOFTWARE_DEFINITION"))
          in_software
        },
        selection='single', escape=FALSE,rownames=FALSE,
        options=list(
          paging = FALSE,
          searching = FALSE,
          preDrawCallback = JS('function() {
                                Shiny.unbindAll(this.api().table().node()); }'
          ),
          drawCallback = JS('function() {
                             Shiny.bindAll(this.api().table().node()); }'
          ),
          columnDefs = list(
            list(orderable = FALSE, className = 'details-control', targets = 0),
            list(visible = FALSE, targets = 1)
          )
        ),
        callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                  var json = JSON.parse(d[1]);
                  var html = '<div style=\"padding: .5em;\" class=\"row\">';
                  if(json.parameters){
                    var params = Object.keys(json.parameters);
                    html += '<div class=\"col-md-6\">';
                    html += '<h4><b>Parameters</b></h4><hr style=\"margin-top:0px;margin-bottom:4px;border:1px solid #000;\">';
                    html += '<ul>';
                    for(var i=0;i<params.length;i++){
                      var param = params[i]
                      html += '<li><b>'+param+'</b>: '+json.parameters[param] + '</li>';
                    }
                     html += '</ul>';
                    html += '</div>';
                  }
                  if(json.properties){
                    var props = Object.keys(json.properties);
                    html += '<div class=\"col-md-6\">';
                    html += '<h4><b>Properties</b></h4><hr style=\"margin-top:0px;margin-bottom:4px;border:1px solid #000;\">';
                    html += '<ul>';
                    for(var i=0;i<props.length;i++){
                      var prop = props[i]
                      html += '<li><b>'+prop+'</b>: '+json.properties[prop] + '</li>';
                    }
                     html += '</ul>';
                    html += '</div>';
                  }
                  html += '</div>';
                  return html;
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                } else {
                row.child(format(row.data())).show();
                td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                }
                });"
        )
      )
    },
    options = list(lengthChange = FALSE)
  )
  #software form
  showSoftwareModal <- function(new = TRUE, software = NULL){
    title = ifelse(new,i18n()$t("CFG_EDITOR_ADD"),i18n()$t("CFG_EDITOR_MODIFY"))
    i18n_key_suffix <- ifelse(new, "ADD", "MODIFY")
    form_action <- tolower(title)
    showModal(modalDialog(title = i18n()$t(sprintf("CFG_EDITOR_METADATA_C_%s", i18n_key_suffix)), size = "l",
                          fluidRow(
                            column(width=6,
                              textInput(ns("software_form_id"), paste0(i18n()$t("CFG_EDITOR_SOFTWARE_IDENTIFIER"),":"), value = software$id),
                              selectInput(ns("software_form_type"), paste0(i18n()$t("CFG_EDITOR_SOFTWARE_TYPE"),":"),choices=c("input", "output"), selected = software$type),
                              selectInput(ns("software_form_software_type"), paste0(i18n()$t("CFG_EDITOR_SOFTWARE_SOFTWARETYPE"),":"),choices=geoflow::list_software()$software_type, selected = software$software_type)
                            ),
                            column(width=6,
                              uiOutput(ns("software_form_details"))
                            )
                          ),
                          actionButton(ns("software_form_cancel"), i18n()$t("CFG_EDITOR_CANCEL")),
                          actionButton(ns(sprintf("software_%s_go", form_action)), title, style = "float:right"),
                          easyClose = FALSE, footer = NULL ))
  }
  #software
  getSoftwareFromModal <- function(){
    software <- list(
      id = input$software_form_id,
      type = input$software_form_type,
      software_type = input$software_form_software_type,
      parameters = list(),
      properties = list()
    )
    params <- geoflow::list_software_parameters(software$software_type, raw = TRUE)
    paramNames <- names(params)
    if(length(paramNames)>0){
      software$parameters <- lapply(paramNames, function(paramName){
        input[[sprintf("software_form_parameters_%s", paramName)]]
      })
      names(software$parameters) <- paramNames
    }
    props <- geoflow::list_software_properties(software$software_type, raw = TRUE)
    propNames <- names(props)
    if(length(propNames)>0){
      software$properties <- lapply(propNames, function(propName){
        prop <- input[[sprintf("software_form_properties_%s", propName)]]
        if(prop == "") prop <- NULL
        return(prop)
      })
      names(software$properties) <- propNames
      software$properties <- software$properties[!sapply(software$properties, is.null)]
    }
    return(software)
  }
  observeEvent(input$software_form_cancel,{
    removeModal()
  })
  #software/add
  observeEvent(input$add_software,{
    showSoftwareModal(new = TRUE)
  })
  #observer software_type selection in modal
  observeEvent(input$software_form_software_type, {
    output$software_form_details <- renderUI({
      
      software <- NULL
      if(!is.null(input$tbl_software_rows_selected)){
        print(sprintf("Row selected: %s", input$tbl_software_rows_selected))
        software <- ctrl_software$list[[input$tbl_software_rows_selected]]
        print(software)
      }
      
      software_details <- list(
        parameters = geoflow::list_software_parameters(input$software_form_software_type, raw = TRUE),
        properties = geoflow::list_software_properties(input$software_form_software_type, raw = TRUE)
      )
      items <- list()
      if(length(software_details$parameters)>0) items <- c(items, i18n()$t("CFG_EDITOR_SOFTWARE_PARAMETERS"))
      if(length(software_details$properties)>0) items <- c(items, i18n()$t("CFG_EDITOR_SOFTWARE_PROPERTIES"))
      
      if(length(items)>0){
        do.call(bs4Dash::tabsetPanel, c(
          id = "software_form_details_tabs", 
          type = "pills",
          lapply(items, function(item){
            shiny::tabPanel(
              title = item,
              value = tolower(item),
              br(),
              do.call("tagList", lapply(names(software_details[[tolower(item)]]), function(name){
                software_param <- software_details[[tolower(item)]][[name]]
                if(!is.null(software_param$choices)){
                  selectizeInput(
                    inputId = ns(sprintf("software_form_%s_%s", tolower(item), name)),
                    label = c(software_param$label, list(tags$span(class = "glyphicon glyphicon-info-sign software-parameter-info", title = software_param$def))),
                    selected = software[[tolower(item)]][[name]],
                    choices = software_param$choices
                  )
                }else{
                  clazz <- software_param$class
                  if(length(clazz)==0) clazz = ""
                  switch(clazz,
                    "character" = textInput(
                      inputId = ns(sprintf("software_form_%s_%s", tolower(item), name)),
                      label = c(software_param$label, list(tags$span(class = "glyphicon glyphicon-info-sign software-parameter-info", title = software_param$def))),
                      value = if(!is.null(software[[tolower(item)]][[name]])){
                        software[[tolower(item)]][[name]]
                      }else{
                        if(!is.null(software_param$default)){software_param$default}else{
                          if(input$software_form_software_type == "ocs" & name %in% c("url","user","pwd")){
                            sprintf("{{GEOFLOW_SHINY_AUTH_%s}}", toupper(name))
                          }else{
                            NULL
                          }
                        }
                      }
                    ),
                    "integer" = numericInput(
                      inputId = ns(sprintf("software_form_%s_%s", tolower(item), name)),
                      label = c(software_param$label, list(tags$span(class = "glyphicon glyphicon-info-sign software-parameter-info", title = software_param$def))),
                      value = if(!is.null(software[[tolower(item)]][[name]])){
                        software[[tolower(item)]][[name]]
                      }else{
                        if(!is.null(software_param$default)){software_param$default}else{NULL}
                      }
                    ),
                    "numeric" = numericInput(
                      inputId = ns(sprintf("software_form_%s_%s", tolower(item), name)),
                      label = c(software_param$label, list(tags$span(class = "glyphicon glyphicon-info-sign software-parameter-info", title = software_param$def))),
                      value = if(!is.null(software[[tolower(item)]][[name]])){
                        software[[tolower(item)]][[name]]
                      }else{
                        if(!is.null(software_param$default)){software_param$default}else{NULL}
                      }
                    ),
                    textInput(
                      inputId = ns(sprintf("software_form_%s_%s", tolower(item), name)),
                      label = c(software_param$label, list(tags$span(class = "glyphicon glyphicon-info-sign software-parameter-info", title = software_param$def))),
                      value = if(!is.null(software[[tolower(item)]][[name]])){
                        software[[tolower(item)]][[name]]
                      }else{
                        if(!is.null(software_param$default)){software_param$default}else{NULL}
                      }
                    )
                  )
                }
              }))       
            )
          })
        ))
      }else{
        tags$div()
      }
    })
  })
  observeEvent(input$software_add_go, {
    ctrl_software$list[[length(ctrl_software$list)+1]] <- getSoftwareFromModal()
    removeModal()
  })
  #entity/modify
  observeEvent(input$modify_software,{
    if(length(input$tbl_software_rows_selected)>=1 ){
      software_sel <- ctrl_software$list[[input$tbl_software_rows_selected]]
      showSoftwareModal(new = FALSE, software_sel)
    }else{
      modalDialog(
        title = i18n()$t("CFG_EDITOR_WARNING"),
        i18n()$t("CFG_EDITOR_WARNING_ROW"),
        easyClose = TRUE
      )
    }
  })
  observeEvent(input$software_modify_go, {
    ctrl_software$list[[input$tbl_software_rows_selected]] <- getSoftwareFromModal()
    removeModal()
  })
  #software/delete
  observeEvent(input$delete_software,{
    showModal(
      if(length(input$tbl_software_rows_selected)>=1 ){
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_CONFIRMATION"),
          footer = tagList(
            modalButton(i18n()$t("CFG_EDITOR_CANCEL")),
            actionButton(ns("software_delete_go"), "Yes", style = "float:right")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_SELECTION"),easyClose = TRUE
        )
      }
    )
  })
  observeEvent(input$software_delete_go, {
    ctrl_software$list[[input$tbl_software_rows_selected]] <- NULL
    ctrl_software$list <- ctrl_software$list[!sapply(ctrl_software$list, is.null)]
    removeModal()
  })
  
  #ACTIONS
  #=====================================================================================
  #actions summary table
  #----------------------------------------------------------------------------------------------------
  output$tbl_actions = DT::renderDT({
    DT::datatable(
      {
        in_action <- cbind(
          ' ' = if(length(ctrl_actions$list)>0) '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>' else character(0),
          'json' = sapply(ctrl_actions$list, function(x){ as.character(jsonlite::toJSON(x, auto_unbox = TRUE)) }),
          data.frame(
            id = if(length(ctrl_actions$list)>0) sapply(ctrl_actions$list, function(x){x$id}) else character(0),
            run = if(length(ctrl_actions$list)>0) sapply(ctrl_actions$list, function(x){as.logical(x$run)}) else logical(0)
          )
        )
        in_action$type = if(length(ctrl_actions$list)>0) {
          sapply(in_action$id, function(x){
            idx = which(x==geoflow::list_actions()$id)
            if(length(idx)>0){
              geoflow::list_actions()[idx,]$type
            }else{
              character(0)
            }
          })
        }
        in_action$def = if(length(ctrl_actions$list)>0){
          sapply(in_action$id, function(x){
            idx = which(x==geoflow::list_actions()$id)
            if(length(idx)>0){
              geoflow::list_actions()[idx,]$definition
            }else{
              character(0)
            }
          })
        }
        colnames(in_action)[colnames(in_action) %in% c("id","run","type","def")] <- c(i18n()$t("CFG_EDITOR_ACTION_IDENTIFIER"), i18n()$t("CFG_EDITOR_ACTION_RUN"), i18n()$t("CFG_EDITOR_ACTION_TYPE"), i18n()$t("CFG_EDITOR_ACTION_DEFINITION"))
        in_action
      },
      selection='single', escape=FALSE,rownames=FALSE,
      options=list(
        paging = FALSE,
        searching = FALSE,
        preDrawCallback = JS('function() {
                                Shiny.unbindAll(this.api().table().node()); }'
        ),
        drawCallback = JS('function() {
                             Shiny.bindAll(this.api().table().node()); }'
        ),
        columnDefs = list(
          list(orderable = FALSE, className = 'details-control', targets = 0),
          list(visible = FALSE, targets = 1)
        )
      ),
      callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                  var json = JSON.parse(d[1]);
                  var html = '<div style=\"background-color:#eee; padding: .5em;\" class=\"row\">';
                  if(json.options){
                    var options = Object.keys(json.options);
                    html += '<div class=\"col-md-6\">';
                    html += '<h4><b>Options</b></h4><hr style=\"margin-top:0px;margin-bottom:4px;border:1px solid;\">';
                    html += '<ul>';
                    for(var i=0;i<options.length;i++){
                      var option = options[i]
                      html += '<li><b>'+option+'</b>: '+json.options[option] + '</li>';
                    }
                     html += '</ul>';
                    html += '</div>';
                  }
                  html += '</div>';
                  return html;
                };
                table.on('click', 'td.details-control', function() {
                var td = $(this), row = table.row(td.closest('tr'));
                if (row.child.isShown()) {
                row.child.hide();
                td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                } else {
                row.child(format(row.data())).show();
                td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                }
                });"
      )
    )
  },
  options = list(lengthChange = FALSE)
  )
  #action form
  showActionModal <- function(new = TRUE, action = NULL){
    title = ifelse(new,i18n()$t("CFG_EDITOR_ADD"),i18n()$t("CFG_EDITOR_MODIFY"))
    i18n_key_suffix <- ifelse(new, "ADD", "MODIFY")
    form_action <- tolower(title)
    showModal(modalDialog(title = i18n()$t(sprintf("CFG_EDITOR_SOFTWARE_%s", i18n_key_suffix)),
                          selectInput(ns("action_form_id"), paste0(i18n()$t("CFG_EDITOR_ACTION_IDENTIFIER"),":"),choices=geoflow::list_actions()$id, selected = action$id),
                          selectInput(ns("action_form_run"), paste0(i18n()$t("CFG_EDITOR_ACTION_RUN"),":"),choices=c(TRUE,FALSE), selected = action$run),
                          uiOutput(ns("action_form_details")),
                          actionButton(ns("action_form_cancel"), i18n()$t("CFG_EDITOR_CANCEL")),
                          actionButton(ns(sprintf("action_%s_go", form_action)), title, style = "float:right"),
                          easyClose = FALSE, footer = NULL ))
  }
  #action
  getActionFromModal <- function(){
    action <- list(
      id = input$action_form_id,
      run = input$action_form_run,
      options = list()
    )
    act_options <- geoflow::list_action_options(action$id, raw = TRUE)
    act_optionNames <- names(act_options)
    if(length(act_optionNames)>0){
      action$options <- lapply(act_optionNames, function(optionName){
        input[[sprintf("action_form_options_%s", optionName)]]
      })
      names(action$options) <- act_optionNames
    }
    return(action)
  }
  observeEvent(input$action_form_cancel, {
    removeModal()
  })
  #action/add
  observeEvent(input$add_action,{
    showActionModal(new = TRUE)
  })
  #observer action id selection in modal
  observeEvent(input$action_form_id, {
    output$action_form_details <- renderUI({
      
      action <- NULL
      if(!is.null(input$tbl_actions_rows_selected)){
        print(sprintf("Row selected: %s", input$tbl_actions_rows_selected))
        action <- ctrl_actions$list[[input$tbl_actions_rows_selected]]
        print(action)
      }
      
      act_options = geoflow::list_action_options(input$action_form_id, raw = TRUE)
      print("Action options")
      print(act_options)
      if(length(act_options)>0){
        tags$div(
            h5(tags$b(i18n()$t("CFG_EDITOR_ACTION_OPTIONS"))),hr(),
            do.call("tagList", lapply(names(act_options), function(name){
              act_option = act_options[[name]]
              multiple <- if(!is.null(act_option$multiple)) act_option$multiple else FALSE
              add_choices <- if(!is.null(act_option$add_choices)) act_option$add_choices else FALSE
              if(!is.null(act_option$choices)){
                selectizeInput(
                  inputId = ns(sprintf("action_form_options_%s", name)),
                  label = act_option$def, selected = if(!is.null(action$options[[name]])) unlist(action$options[[name]]) else unlist(act_option$default),
                  choices = act_option$choices, multiple = multiple,
                  options = list(create = add_choices)
                ) 
              }else{
                clazz <- act_option$class
                if(length(clazz)==0) clazz = ""
                
                switch(clazz,
                       "character" = textInput(
                         inputId = ns(sprintf("action_form_options_%s", name)),
                         label = act_option$def, value = ifelse(!is.null(action$options[[name]]), action$options[[name]], act_option$default)
                       ),
                       "integer" = numericInput(
                         inputId = ns(sprintf("action_form_options_%s", name)),
                         label = act_option$def, value = ifelse(!is.null(action$options[[name]]), action$options[[name]], act_option$default)
                       ),
                       "numeric" = numericInput(
                         inputId = ns(sprintf("action_form_options_%s", name)),
                         label = act_option$def, value = ifelse(!is.null(action$options[[name]]), action$options[[name]], ifelse(act_option$default!=Inf, act_option$default, 1))
                       ),
                       "logical" = selectizeInput(
                         inputId = ns(sprintf("action_form_options_%s", name)),
                         label = act_option$def, selected = ifelse(!is.null(action$options[[name]]), action$options[[name]], act_option$default),
                         choices = if(!is.null(act_option$default)){as.character(c(act_option$default,!act_option$default))}else {c("FALSE","TRUE")},
                         multiple = multiple
                       ),
                       textInput(
                         inputId = ns(sprintf("action_form_options_%s", name)),
                         label = act_option$def, value = as.character(ifelse(!is.null(action$options[[name]]), action$options[[name]], act_option$default))
                       )
                )
              }
            }))       
          )
      }else{
        tags$div()
      }
    })
  })
  observeEvent(input$action_add_go, {
    ctrl_actions$list[[length(ctrl_actions$list)+1]] <- getActionFromModal()
    removeModal()
  })
  #action/modify
  observeEvent(input$modify_action,{
    if(length(input$tbl_actions_rows_selected)>=1 ){
      action_sel <- ctrl_actions$list[[input$tbl_actions_rows_selected]]
      showActionModal(new = FALSE, action_sel)
    }else{
      modalDialog(
        title = i18n()$t("CFG_EDITOR_WARNING"),
        i18n()$t("CFG_EDITOR_WARNING_ROW"),
        easyClose = TRUE
      )
    }
  })
  observeEvent(input$action_modify_go, {
    ctrl_actions$list[[input$tbl_actions_rows_selected]] <- getActionFromModal()
    removeModal()
  })
  #action/delete
  observeEvent(input$delete_action,{
    showModal(
      if(length(input$tbl_actions_rows_selected)>=1 ){
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_CONFIRMATION"),
          footer = tagList(
            modalButton(i18n()$t("CFG_EDITOR_CANCEL")),
            actionButton(ns("action_delete_go"), "Yes", style = "float:right")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = i18n()$t("CFG_EDITOR_WARNING"),
          i18n()$t("CFG_EDITOR_DELETE_SELECTION"),easyClose = TRUE
        )
      }
    )
  })
  observeEvent(input$action_delete_go, {
    ctrl_actions$list[[input$tbl_actions_rows_selected]] <- NULL
    ctrl_actions$list <- ctrl_actions$list[!sapply(ctrl_actions$list, is.null)]
    removeModal()
  })
  
  #CONFIGURATION LOAD
  #=====================================================================================
  #on config url load
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(length(query)>0){
      filename <- query[["file"]]
      if(!is.null(filename)) {
        cat(sprintf("Selecting configuration file '%s'\n", filename))
        config <- loadConfigurationFileFromUrl(filename)
        ctrl_config_file(attr(config, "filepath"))
        loadConfigurationUI(config)
      }
    }
  })
  
  #getProfileFromInput
  getProfileFromInput <- function(){
    ctrl_profile$id <- input$profile_id
    ctrl_profile$mode <- input$profile_mode
    
    #metadata
    ctrl_profile$name <- input$profile_name
    ctrl_profile$project <- input$profile_project
    ctrl_profile$organization <- input$profile_organization
    ctrl_profile$logos <- input$profile_logos
    
    #options
    line_separator <- geoflow::get_line_separator()
    if(length(input$profile_option_line_separator)>0){
      line_separator <- input$profile_option_line_separator
      if(!endsWith(line_separator, "\n")) line_separator = paste0(line_separator, "\n")
    }
    ctrl_profile$options <- list(
      line_separator = line_separator,
      skipDataDownload = input$profile_option_skipDataDownload,
      skipEnrichWithData = input$profile_option_skipEnrichWithData,
      skipEnrichWithDatatypes = input$profile_option_skipEnrichWithDatatypes,
      skipDynamicBbox = input$profile_option_skipDynamicBbox,
      enrichDataStrategy = input$profile_option_enrichDataStrategy
      
    )
    return(reactiveValuesToList(ctrl_profile))
  }
  
  #save configuration
  getConfiguration <- function(){
    
    out_json <- list(
      profile = getProfileFromInput(),
      metadata = reactiveValuesToList(ctrl_metadata),
      software = ctrl_software$list,
      actions = ctrl_actions$list,
      registers = list()
    )
    
    for(md_domain in c("contacts", "entities", "dictionary")){
      out_json$metadata[[md_domain]] = lapply(seq_len(nrow(out_json$metadata[[md_domain]])), function(i) {
        as.list(out_json$metadata[[md_domain]][i, , drop = FALSE])
      })
    }
    
    out_json <- rapply(out_json, function(x){
      if(x[1] %in% c("TRUE","FALSE")) x <- as.logical(x); 
      return(x)
    }, classes = "character", deflt = NA_integer_, how = "replace")
    return(out_json)
  }
  
 })
  
}