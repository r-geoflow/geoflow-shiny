#config_editor_server
config_editor_server<- function(id, auth_info, parent.session){

 moduleServer(id, function(input, output, session){
  
  ns <- session$ns
  
  output$config_editor_info <- renderText({
    session$userData$module("configuration-editor")
    updateModuleUrl(session, "configuration-editor")
    text <- "<h2><b>geoflow</b> configuration editor <small>Create, edit and save a <b>geoflow</b> configuration</small></h2><hr>"
    text
  })
  
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
  #loadConfigurationFile
  loadConfigurationFile <- function(){
    config <- try(jsonlite::read_json(input$jsonfile$datapath))
    attr(config, "filepath") <- input$jsonfile$datapath
    return(config)
  }
  #loadConfigurationFileFromUrl
  loadConfigurationFileFromUrl <- function(file){
    filepath <- if(appConfig$auth){
      AUTH_API$downloadFile(relPath = appConfig$data_dir_remote, filename = file, outdir = tempdir())
    }else{
      file
    }
    config <- try(jsonlite::read_json(filepath))
    attr(config, "filepath") <- filepath
    print(filepath)
    return(config)
  }
  
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
    if(!is.null(names(config_contacts))) config_contacts <- list(config_contacts)
    ctrl_metadata$contacts = do.call("rbind", lapply(config_contacts, function(config_contact){
      addContactSource(handler = config_contact$handler, source = config_contact$source)
    }))
    #entities
    config_entities <- config$metadata$entities
    if(!is.null(names(config_entities))) config_entities <- list(config_entities)
    ctrl_metadata$entities = do.call("rbind", lapply(config_entities, function(config_entity){
      addEntitySource(handler = config_entity$handler, source = config_entity$source)
    }))
    
    #load software
    ctrl_software$list <- config$software
    
    #load actions
    ctrl_actions$list <- config$actions
    
  }
  
  #controllers
  #------------------------------------------------------------------------------------
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
  
  output$geoflow_config_tabpanel <- renderUI({
    bs4Dash::tabsetPanel(
      id = "geoflow_config_blocks", 
      type = "pills",
      shiny::tabPanel(
        value = "profile",
        title = "Profile",
        tags$div(id = "profile",
                 br(),
                 uiOutput(ns("profile"))
        )
      ),
      shiny::tabPanel(
        value = "metadata",
        title = "Metadata",
        br(),
        box(id = "metadata", width = 12,
            bs4Dash::tabsetPanel(
              id = "metadata_tabs", 
              type = "tabs",
              shiny::tabPanel(
                value = "contacts",
                title = "Contacts",
                shiny::tagList(
                  br(),
                  shiny::actionButton(inputId = ns("add_contact"), label = "Add a new contact source", class = "btn-primary"),
                  shiny::actionButton(inputId = ns("modify_contact"), label = "Modify a contact source", class = "btn-warning"),
                  shiny::actionButton(inputId = ns("delete_contact"), label = "Delete a contact source", class = "btn-danger"),
                  DT::DTOutput(ns("tbl_contacts"))
                )
              ),
              shiny::tabPanel(
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
              shiny::tabPanel(
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
      shiny::tabPanel(
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
      shiny::tabPanel(
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
  })
  
  #PROFILE
  #=====================================================================================
  output$profile <- renderUI({
    tagList(
      box(
        width = 6,
        bs4Dash::tabsetPanel(
          id = "profile_execution_main", 
          type = "tabs",
          shiny::tabPanel(
            title = "Execution parameters",
            textInput(inputId = ns("profile_id"), label = "Workflow identifier", value = ctrl_profile$id),
            selectizeInput(inputId = ns("profile_mode"), label = "Workflow mode", choices = c("raw", "entity"), selected = ctrl_profile$mode)
          ),
          shiny::tabPanel(
            title = "Execution options",
            textInput(inputId = ns("profile_option_line_separator"), label = "Metadata line separator", value = ctrl_profile$options$line_separator),
            selectizeInput(inputId = ns("profile_option_skipDataDownload"), label = "Skip data download", choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipDataDownload),
            selectizeInput(inputId = ns("profile_option_skipEnrichWithData"), label = "Skip enrich with data", choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipEnrichWithData),
            selectizeInput(inputId = ns("profile_option_skipEnrichWithDatatypes"), label = "Skip enrich with data types", choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipEnrichWithDatatypes),
            selectizeInput(inputId = ns("profile_option_skipDynamicBbox"), label = "Skip dynamic BBOX", choices = c("FALSE", "TRUE"), selected = ctrl_profile$options$skipDynamicBbox),
            selectizeInput(inputId = ns("profile_option_enrichDataStrategy"), label = "Enrich data strategy", choices = c("first", "union"), selected = ctrl_profile$options$enrichDataStrategy)
          )
        )
      ),
      box(
        width = 6,
        title = "Additional information",
        textInput(inputId = ns("profile_name"), label = "Name", value = ctrl_profile$name),
        textInput(inputId = ns("profile_project"), label = "Project", value = ctrl_profile$project),
        textInput(inputId = ns("profile_organization"), label = "Organization", value = ctrl_profile$organization),
        selectizeInput(inputId = ns("profile_logos"), label = "Logos", choices = ctrl_profile$logos, selected = ctrl_profile$logos,
                      multiple = TRUE, options = list(create = TRUE))
      )
    )
  })
  
  #CONFIG VALIDATION/TEST
  #=====================================================================================  
  loadConfiguration <- function(){
    geoflow_config <- try(geoflow::initWorkflow(ctrl_config_file(), handleMetadata = FALSE, session = parent.session))
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
        md_validation_message <- tags$span("No validation issue detected!", style = "color:green;font-weight:bold;")
      }else{
        md_content_report$row <- paste("Row", md_content_report$row)
        ctrl_validation$report <- md_content_report
        ctrl_validation$report_raw <- md_validator$validate_content(raw = TRUE)
        ctrl_validation$report_data <- md_data
        hasReport <- TRUE
      }
    }
    
    showModal(
      modalDialog(
        title = sprintf("Metadata (%s) validation report", type),
        tags$b("Source: "), tags$b(tags$a(href = source, source)),hr(),
        if(hasReport){
          bs4Dash::tabsetPanel(
            type = "pills",
            shiny::tabPanel(
              title = "Smart view", hr(),
              rhandsontable::rHandsontableOutput(ns("validation_report_HT"))
            ),
            shiny::tabPanel(
              title = "Raw report", hr(),
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
      colnames(data) <- c("Handler", "Source")
      if(validate){
        data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              Handler = data[i, "Handler"],
              Source = data[i, "Source"],
              Actions = as(actionButton(inputId = ns(paste0('button_validate_',type,'_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                             title = "Check metadata", label = "", icon = icon("tasks")),"character")
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
        columnDefs = list(
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
        )
      )
    )
    if(validate) manageButtonValidateEvents(data, type, uuids)
  }
  
  #render tables
  observe({
    renderMetadataTable(ctrl_metadata$contacts, "contacts", TRUE)
    renderMetadataTable(ctrl_metadata$entities, "entities", TRUE)
    renderMetadataTable(ctrl_metadata$dictionnary, "dictionnary", FALSE)
  })
  
  #contacts
  #-----------------------------------------------------------------------------------------------------
  #contact form
  showContactModal <- function(new = TRUE, handler = "", source = ""){
    title_prefix <- ifelse(new, "Add", "Modify")
    form_action <- tolower(title_prefix)
    showModal(modalDialog(title = sprintf("%s contact source", title_prefix),
                          selectInput(ns("contact_form_handler"), "Handler:",choices=geoflow::list_contact_handlers()$id, selected = handler),
                          textInput(ns("contact_form_source"), "Source", value = source), 
                          actionButton(ns(sprintf("contact_%s_go", form_action)), title_prefix),
                          easyClose = TRUE, footer = NULL ))
  }
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
        title = "Warning",
        paste("Please select the row that you want to edit!" ),easyClose = TRUE
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
          title = "Warning",
          paste("Are you sure to delete",length(input$tbl_contacts_rows_selected),"contact source(s)?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("contact_delete_go"), "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select contact source(s) that you want to delete!" ),easyClose = TRUE
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
    title_prefix <- ifelse(new, "Add", "Modify")
    form_action <- tolower(title_prefix)
    showModal(modalDialog(title = sprintf("%s entity source", title_prefix),
                          selectInput(ns("entity_form_handler"), "Handler:",choices=geoflow::list_entity_handlers()$id, selected = handler),
                          textInput(ns("entity_form_source"), "Source", value = source), 
                          actionButton(ns(sprintf("entity_%s_go", form_action)), title_prefix),
                          easyClose = TRUE, footer = NULL ))
  }
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
        title = "Warning",
        paste("Please select the row that you want to edit!" ),
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
          title = "Warning",
          paste("Are you sure to delete",length(input$tbl_entities_rows_selected),"entity source(s)?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("entity_delete_go"), "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select entity source(s) that you want to delete!" ),easyClose = TRUE
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
    title_prefix <- ifelse(new, "Add", "Modify")
    form_action <- tolower(title_prefix)
    showModal(modalDialog(title = sprintf("%s dictionary source", title_prefix),
                          selectInput(ns("dictionary_form_handler"), "Handler:",choices=geoflow::list_dictionary_handlers()$id, selected = handler),
                          textInput(ns("dictionary_form_source"), "Source", value = source), 
                          actionButton(ns(sprintf("dictionary_%s_go", form_action)), title_prefix),
                          easyClose = TRUE, footer = NULL ))
  }
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
        title = "Warning",
        paste("Please select the row that you want to edit!" ),easyClose = TRUE
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
          title = "Warning",
          paste("Are you sure to delete",length(input$tbl_dictionary_rows_selected),"dictionary source(s)?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("dictionary_delete_go"), "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select dictionary source(s) that you want to delete!" ),easyClose = TRUE
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
          colnames(in_software)[colnames(in_software) %in% c("id","type","software_type","def")] <- c("Identifier", "Type (input/output)", "Software Type", "Definition")
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
    title_prefix <- ifelse(new, "Add", "Modify")
    form_action <- tolower(title_prefix)
    showModal(modalDialog(title = sprintf("%s software", title_prefix),
                          textInput(ns("software_form_id"), "Id:", value = software$id),
                          selectInput(ns("software_form_type"), "Type:",choices=c("input", "output"), selected = software$type),
                          selectInput(ns("software_form_software_type"), "Type:",choices=geoflow::list_software()$software_type, selected = software$software_type),
                          uiOutput(ns("software_form_details")),
                          actionButton(ns(sprintf("software_%s_go", form_action)), title_prefix),
                          easyClose = TRUE, footer = NULL ))
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
      if(length(software_details$parameters)>0) items <- c(items, "Parameters")
      if(length(software_details$properties)>0) items <- c(items, "Properties")
      
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
        title = "Warning",
        paste("Please select the row that you want to edit!" ),
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
          title = "Warning",
          paste("Are you sure to delete",length(input$tbl_software_rows_selected),"software?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("software_delete_go"), "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select software that you want to delete!" ),easyClose = TRUE
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
        in_action$type = if(length(ctrl_actions$list)>0) geoflow::list_actions()[sapply(in_action$id, function(x){which(x==geoflow::list_actions()$id)}),]$type else character(0)
        in_action$def = if(length(ctrl_actions$list)>0) geoflow::list_actions()[sapply(in_action$id, function(x){which(x==geoflow::list_actions()$id)}),]$definition else character(0)
        colnames(in_action)[colnames(in_action) %in% c("id","run","type","def")] <- c("Identifier", "Run?", "Action Type", "Definition")
        print(in_action)
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
    title_prefix <- ifelse(new, "Add", "Modify")
    form_action <- tolower(title_prefix)
    showModal(modalDialog(title = sprintf("%s action", title_prefix),
                          selectInput(ns("action_form_id"), "Type:",choices=geoflow::list_actions()$id, selected = action$id),
                          selectInput(ns("action_form_run"), "Run:",choices=c(TRUE,FALSE), selected = action$run),
                          uiOutput(ns("action_form_details")),
                          actionButton(ns(sprintf("action_%s_go", form_action)), title_prefix),
                          easyClose = TRUE, footer = NULL ))
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
            h5(tags$b("Action options")),hr(),
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
        title = "Warning",
        paste("Please select the row that you want to edit!" ),
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
          title = "Warning",
          paste("Are you sure to delete",length(input$tbl_actions_rows_selected),"action(s)?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("action_delete_go"), "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select action that you want to delete!" ),easyClose = TRUE
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
  #on config load
  observeEvent(input$load_configuration,{
    if(!is.null(input$jsonfile)){
      config <- loadConfigurationFile()
      output$jsonfile_msg <- renderUI({
        if(is(config, "try-error")){
          tags$span("Please provide a valid JSON file!", style = "color:red;font-weight:bold;float:left;margin-top: 8px;margin-left: 5px;")
        }else{
          ctrl_config_file(attr(config, "filepath"))
          loadConfigurationUI(config)
          tags$span("Valid JSON", style = "color:green;font-weight:bold;float:left;margin-top: 8px;margin-left: 5px;")
        }
      })
    }else{
      output$jsonfile_msg <- renderUI({
        tags$span("No file specified!", style = "color:red;font-weight:bold;float:left;margin-top: 8px;margin-left: 5px;")
      })
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
    out_json <- rapply(out_json, function(x){
      if(x[1] %in% c("TRUE","FALSE")) x <- as.logical(x); 
      return(x)
    }, classes = "character", deflt = NA_integer_, how = "replace")
    return(out_json)
  }
  
  #saveConfiguration
  observeEvent(input$saveConfiguration,{
    shinyjs::disable("saveConfiguration")
    progress <- Progress$new(session, min=0, max=100)
    on.exit(progress$close())
    config_json <- getConfiguration()
    progress$set(value = 25, message = "Preparing geoflow configuration file...")
    filename <- paste0(config_json$profile$id, ".json")
    if(appConfig$auth){
      switch(auth_info()$endpoint$auth_type,
        "ocs" = {
          #use of ocs4R::ocsManager
          if(!paste0(appConfig$data_dir_remote,"/") %in% AUTH_API$listFiles()$name){
            AUTH_API$makeCollection(appConfig$data_dir_remote)
          }
          file <- file.path(tempdir(), filename)
          jsonlite::write_json(config_json, 
                               file, 
                               auto_unbox = TRUE, pretty = TRUE)
          AUTH_API$uploadFile(relPath = appConfig$data_dir_remote, filename = file)
          unlink(file)
        },
        "d4science" = {
          #use of d4storagehub4R::StoragehubManager
          folderPath <- dir(appConfig$data_dir_remote)
          if(length(folderPath)==0) folderPath <- NULL
          folderName <- basename(appConfig$data_dir_remote)
          if(is.null(folderPath)){
            if(!folderName %in% AUTH_API$listWSItems()$name){
              AUTH_API$createFolder(name = folderName)
            }
          }else{
            AUTH_API$createFolder(folderPath = folderPath, name = folderName)
          }
          
          file <- file.path(tempdir(), filename)
          jsonlite::write_json(config_json, 
                               file, 
                               auto_unbox = TRUE, pretty = TRUE)
          AUTH_API$uploadFile(folderPath = appConfig$data_dir_remote, file = file) 
        }
      )
    }else{
      if(!dir.exists(GEOFLOW_DATA_DIR)) dir.create(GEOFLOW_DATA_DIR, recursive = TRUE)
      jsonlite::write_json(config_json, 
                           file.path(GEOFLOW_DATA_DIR, filename), 
                           auto_unbox = TRUE, pretty = TRUE)
      unlink(file)
    }
    progress$set(value = 99, message = "geoflow configuration file successfully saved!")
    Sys.sleep(1)
    progress$set(value = 100)
    shinyjs::enable("saveConfiguration")
  })
  
  #downloadConfiguration
  output$downloadConfiguration <- downloadHandler(
    filename = function(){ paste0("geoflow_config_", ctrl_profile$id, ".json")  },
    content = function(con){
      disable("downloadConfiguration")
      config_json <- getConfiguration()
      jsonlite::write_json(config_json, con, auto_unbox = TRUE, pretty = TRUE)
      enable("downloadConfiguration")
    }
  )
  
 })
  
}