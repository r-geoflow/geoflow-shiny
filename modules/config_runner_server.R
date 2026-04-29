#config_runner_server
config_runner_server<- function(id, auth_info = NULL, auth_api = NULL, i18n, geoflow_configs, parent.session){
  
 moduleServer(id, function(input, output, session){
  
  ns <- session$ns
  
  ipc.queue <- ipc::shinyQueue()
  ipc.queue$consumer$start()
  
  #auth API (if existing)
  #AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
  
  #reactives
  reactive_workflow <- reactiveVal(NULL)
  reactive_job <- reactiveVal(NULL)
  empty_status = ""; attr(empty_status, "status_code") = ""
  reactive_job_status <- reactiveVal(empty_status)
  reactive_job_progress <- reactiveVal(0)
  
  #event on language change
  observeEvent(i18n(),{
    #update job_status (handled as reactive)
    status_code = attr(reactive_job_status(),"status_code")
    translated_status = if(status_code != ""){ i18n()$t(paste0("EXEC_JOB_STATUS_", toupper(status_code))) }else{ "" }
    attr(translated_status, "status_code") = status_code
    reactive_job_status(translated_status)
  })
  

  #UIs
  #-----------------------------------------------------------------------------
  
  #workflow manager
  output$workflow_manager <- renderUI({
    shiny::fluidRow(
      box(
        inputId = "config_loader", 
        title = i18n()$t("EXEC_WORKFLOW"), status = "primary", width = 6,
        br(),
        uiOutput(ns("config_file_loader")),
        br(),
        hr(),
        uiOutput(ns("config_actions"))
      ),
      box(
        inputId = "config_runner",
        title = i18n()$t("EXEC_CONSOLE"), status = "primary", width = 6,
        uiOutput(ns("config_job_status")),
        uiOutput(ns("config_job_download")),br(),br(),
        uiOutput(ns("config_job_interactive_log"))
      )
    )
  })
  
  #Loader
  output$config_file_loader <- renderUI({
    if(appConfig$auth){
      tabsetPanel(
        id = "config_runner_load_config_modes",
        tabPanel(i18n()$t("CFG_EDITOR_MODE_CLOUD"),
                 tagList(
                   jsTreeR::jstreeOutput(ns("config_load_tree_leavesonly")),
                   actionButton(ns("config_load_tree_leavesonly_select"), label = i18n()$t("CFG_EDITOR_SELECT"), status = "primary", style = "float:right"),
                   actionButton(ns("config_load_tree_leavesonly_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                 )
        ),
        tabPanel(i18n()$t("CFG_EDITOR_MODE_LOCAL"),
                 tagList(
                   fileInput(ns("config_local_file"), label = i18n()$t("FILE"),multiple = FALSE,accept = c(".json",".yml",".yaml"),buttonLabel = i18n()$t("CFG_EDITOR_CHOOSEFILE")),
                   actionButton(ns("config_local_file_select"), label = i18n()$t("CFG_EDITOR_SELECT"), status = "primary", style = "float:right"),
                   actionButton(ns("config_local_file_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                 )
        )
      )
    }else{
      tabsetPanel(
        id = "config_runner_load_config_modes",
        tabPanel(i18n()$t("CFG_EDITOR_MODE_LOCAL"),
                 tagList(
                   fileInput(ns("config_local_file"), label = i18n()$t("FILE"),multiple = FALSE,accept = c(".json",".yml",".yaml"),buttonLabel = i18n()$t("CFG_EDITOR_CHOOSEFILE")),
                   actionButton(ns("config_local_file_select"), label = i18n()$t("CFG_EDITOR_SELECT"), status = "primary", style = "float:right"),
                   actionButton(ns("config_local_file_cancel"), label = i18n()$t("CFG_EDITOR_CANCEL"), style = "float:right")
                 )
        )
      )
    }
  })
  
  loadCloudTree(id = "config_load_tree_leavesonly", config = appConfig, auth_api = AUTH_API, 
                mime_types = c(".json", ".yml", ".yaml"), leaves_only = TRUE, output = output)
  
  #actions
  output$config_actions <- renderUI({
    if(!is.null(reactive_workflow())){
      tags$div(
        style = "float:right;", 
        tagList(
          actionButton(inputId = ns("workflow_edit"), class="btn btn-info", style = "margin-right: 2px;",
                       title = i18n()$t("EXEC_BUTTON_EDIT"), label = "", icon = icon("edit")),
          actionButton(inputId = ns("workflow_run"), class="btn btn-primary",
                       title = i18n()$t("EXEC_BUTTON_EXECUTE"), label = "", icon = icon("play"))
        )
      )
    }else{
      tags$div()
    }
  })
  
  #Console
  #Interactive job status
  output$config_job_status <- renderUI({ 
    div(
      tags$span(
        switch(attr(reactive_job_status(), "status_code"),
               "inprogress" = icon("gears"),
               "success" = icon("check"),
               "error" = icon("xmark"),
               "aborted" = icon("exclamation"),
               ""
        ),
        if(attr(reactive_job_status(), "status_code") == "inprogress"){
          paste(reactive_job_status(), paste0("(", reactive_job_progress(),"%)"))
        }else{
          reactive_job_status()
        },
        class = switch(attr(reactive_job_status(), "status_code"),
                       "started" = "badge badge-info",
                       "inprogress" = "badge badge-info",
                       "success" = "badge badge-success",
                       "error" = "badge badge-danger",
                       "aborted" = "badge badge-secondary",
                       ""
        ), style="margin-left:7.5px;"),
      style = "float:left;"
    )
  })
  
  output$config_job_download <- renderUI({
    if(attr(reactive_job_status(), "status_code") %in% c("success", "error")){
      div(
        downloadButtonCustom(
          ns("download_job_archive"),
          i18n()$t("EXEC_BUTTON_JOB_ARCHIVE"),
          icon = icon("download")
        ),
        downloadButtonCustom(
          ns("download_job_log"),
          i18n()$t("EXEC_BUTTON_JOB_LOG"),
          icon = icon("list")
        ),
        style = "float:right;margin-right:7.5px;"
      )
    }else{
      ""
    }
  })
  output$download_job_archive <- downloadHandler(
    filename = function(){ paste0(basename(reactive_job()), ".zip") },
    content = function(con){
      disable("download_job_archive")
      setwd(reactive_job())
      zip::zip(zipfile = con, files = list.files())
      enable("download_job_archive")
    },
    contentType = "application/zip"
  )
  
  output$download_job_log <- downloadHandler(
    filename = function(){ paste0(basename(reactive_job()),"_log", ".txt") },
    content = function(con){
      disable("download_job_log")
      file.copy(
        from = file.path(reactive_job(), "job-logs.txt"),
        to = con
      )
      enable("download_job_log")
    },
    contentType = "text/plain"
  )
  
  #Interactive job console log
  config_job_interactive_log <- reactivePoll(100, session,
                                             checkFunc = function(){
                                               if(!is.null(reactive_job())){
                                                 logfile <- file.path(reactive_job(), "job-logs.txt")
                                                 if(file.exists(logfile)){
                                                   file.info(logfile)$mtime[1]
                                                 }else{
                                                   ""
                                                 }
                                               }else{
                                                 ""
                                               }
                                             },
                                             valueFunc = function(){
                                               out_txt <- i18n()$t("EXEC_JOB_NOJOBSTARTED")
                                               if(!is.null(reactive_job())){
                                                 if(out_txt == ""){
                                                   out_txt <- i18n()$t("EXEC_JOB_STARTING")
                                                 }else{
                                                   logfile <- file.path(reactive_job(), "job-logs.txt")
                                                   if(file.exists(logfile)){
                                                     out_txt = paste(readLines(logfile),collapse="\n")
                                                   }
                                                 }
                                               }
                                               out_txt
                                             }
  )
  output$config_job_interactive_log <- renderUI({
    sep <- paste0(paste0(rep("=",100),collapse=""),"\n")
    parts <- unlist(strsplit(config_job_interactive_log(), sep))
    if(length(parts)==1){
      tags$div(
        config_job_interactive_log(),
        style = "max-height:500px;font-size:80%;color:white;background-color:black;overflow-y:auto;white-space: pre-line;padding:2px;"
      )
    }else{
      nb_parts <- length(parts)/2
      if((length(parts) %% 2) != 0) parts <- c(parts, "")
      nb_parts <- length(parts)/2
      do.call("accordion", c(id="job-steps", lapply(1:nb_parts, function(i){
        bs4Dash::accordionItem(
          title = {
            the_title <- i18n()$t(paste0("EXEC_", gsub("\n", "", gsub(" ", "_", toupper(parts[(i*2)-1])))))
            if(i==1){
              tags$span(fa("r-project", fill = "steelblue"), the_title, style = "font-weight:bold;")
            }else{
              if(i<nb_parts){
                tags$span(fa("far fa-circle-check"), the_title, style = "font-weight:bold;")
              }else{
                switch(attr(reactive_job_status(), "status_code"),
                       "started" = tags$span(fa("gears"), the_title, style = "font-weight:bold;"),
                       "inprogress" = tags$span(fa("gears"), the_title, style = "font-weight:bold;"),
                       "success" = tags$span(fa("far fa-circle-check"), the_title, style = "font-weight:bold;"),
                       "error" = tags$span(fa("far fa-circle-xmark"), the_title, style = "font-weight:bold;")
                )
              }
            }
          },
          status = {
            if(i==1){
              "gray-dark"
            }else{
              if(i<nb_parts){
                "success"
              }else{
                switch(attr(reactive_job_status(), "status_code"),
                       "started" = "info",
                       "inprogress" = "info",
                       "success" = "success",
                       "error" = "danger"
                )
              }
            }
          },
          collapsed = if(i<nb_parts) TRUE else FALSE,
          tags$div(
            parts[i*2],
            style = "max-height:500px;font-size:80%;color:white;background-color:black;overflow-y:auto;white-space: pre-line;padding:2px;"
          )
        )
      })))
    }
    
  })
  
  #EVENTS
  #-----------------------------------------------------------------------------
  #cancel buttons (local / cloud)
  #local
  observeEvent(input$config_local_file_cancel, {
    shiny::removeModal()
  })
  #cloud
  observeEvent(input$config_load_tree_leavesonly_cancel,{
    shiny::removeModal()
  })
  #select buttons (local / cloud)
  #local
  observeEvent(input$config_local_file_select,{
    req(!is.null(input$config_local_file))
    
    #read local resource
    filepath <- input$config_local_file$datapath
    config <- try(switch(mime::guess_type(filepath),
                         "application/json" = jsonlite::read_json(filepath),
                         "application/yaml" = yaml::read_yaml(filepath)
    ))
    if(is(config, "try-error")){
      postMessage(msg = i18n()$t("CFG_EDITOR_CONFIG_LOAD_ERROR1"), type = "error")
      return(NULL)
    }
    
    attr(config, "filepath") <- filepath
    #load configuration UI
    reactive_workflow(attr(config, "filepath"))
    
    shiny::removeModal()
    postMessage(msg = i18n()$t("CFG_EDITOR_CONFIG_LOAD_SUCCESS"), type = "success")
  })
  #cloud
  observeEvent(input$config_load_tree_leavesonly_select,{
    selected_resource = input$config_load_tree_leavesonly_selected
    #OCS download selected resource and read it
    filepath <- auth_api()$downloadFile(relPath = dirname(selected_resource[[1]]$data), filename = selected_resource[[1]]$text, outdir = tempdir())
    config <- try(switch(mime::guess_type(filepath),
                         "application/json" = jsonlite::read_json(filepath),
                         "application/yaml" = yaml::read_yaml(filepath)
    ))
    if(is(config, "try-error")){
      postMessage(msg = i18n()$t("CFG_EDITOR_CONFIG_LOAD_ERROR1"), type = "error")
      return(NULL)
    }
    
    attr(config, "filepath") <- filepath
    
    #load configuration UI
    reactive_workflow(attr(config, "filepath"))
    
    shiny::removeModal()
    loadCloudTree(id = "config_load_tree_leavesonly", config = appConfig, auth_api = auth_api(), 
                  mime_types = c(".json", ".yml", ".yaml"), leaves_only = TRUE, output = output)
    postMessage(msg = i18n()$t("CFG_EDITOR_CONFIG_LOAD_SUCCESS"), type = "success")
  })
  
  
  #edit workflow config
  observeEvent(input$workflow_edit,{
    shinyjs::disable("workflow_edit")
    parent.session$userData$workflow(reactive_workflow())
    updateModuleUrl(session, "configuration-editor")
    updateTabItems(session = parent.session, "geoflow-tabs", "config_editor")
    shinyjs::enable("workflow_edit")
  })
  
  #execute workflow
  observeEvent(input$workflow_run,{
    
    shinyjs::disable("workflow_run")
    
    reactive_job("")
    started = i18n()$t("EXEC_JOB_STATUS_STARTED");attr(started, "status_code") = "started"
    reactive_job_status(started)
    
    filepath <- reactive_workflow()
    outconfig <- try(switch(mime::guess_type(filepath),
     "application/json" = jsonlite::read_json(filepath),
     "application/yaml" = yaml::read_yaml(filepath)
    ))
    if(is.null(outconfig$profile$id)) outconfig$profile$id <- outconfig$id
    
    targetdir <- if(appConfig$auth){
      tempdir()
    }else{
      GEOFLOW_DATA_DIR
    }
    
    #geoflow execution
    ipc.progress <- ipc::AsyncProgress$new(
      value = 0, min = 0, max = 100,
      message = sprintf("%s '%s'", i18n()$translate("EXEC_WORKFLOW"), outconfig$profile$id),
      detail = as.character(i18n()$translate("EXEC_JOB_INITIALIZATION"))
    )
    
    #i18n messages (to be shared within future)
    workflow_msg_started = as.character(translator$translate("EXEC_JOB_STATUS_STARTED")); attr(workflow_msg_started, "status_code") = "started"
    workflow_msg_inprogress = as.character(translator$translate("EXEC_JOB_STATUS_INPROGRESS")); attr(workflow_msg_inprogress, "status_code") = "inprogress"
    workflow_msg_exec = as.character(translator$translate("EXEC_WORKFLOW"))
    workflow_msg_exec_detail = as.character(translator$translate("EXEC_WORKFLOW_EXECUTION_ACTION_DETAIL"))
    
    #exec
    future::future({
      ipc.queue$producer$fireEval(print("Process started for geoflow job"))
      ipc.queue$producer$fireAssignReactive("reactive_job_status", workflow_msg_started)
      
      geoflow::executeWorkflow(
        file = filepath,
        dir = targetdir,
        queue = ipc.queue,
        on_initWorkflowJob = function(config, queue){
          queue$producer$fireEval(print("Successful job directory creation"))
          queue$producer$fireAssignReactive("reactive_job", config$job)
        },
        on_initWorkflow = function(config, queue){
          queue$producer$fireEval(print("Successful workflow initialization"))
          ipc.queue$producer$fireAssignReactive("reactive_job_status", workflow_msg_inprogress)
        },
        on_closeWorkflow = function(config, queue){
          ipc.progress$close()
        },
        monitor = function(step, config, entity, action, queue){
          queue$producer$fireAssignReactive("reactive_job_progress", step)
          ipc.progress$set(
            value = step,
            message = sprintf("%s [%s] :", workflow_msg_exec, config$profile$id),
            detail = sprintf(workflow_msg_exec_detail,action$id, entity$identifiers[["id"]], step)
          )
        },
        session = parent.session
      )
    }) %...>% 
      (function(result){
        success = as.character(translator$translate("EXEC_JOB_STATUS_SUCCESS")); attr(success, "status_code") = "success"
        reactive_job_status(success)
        shinyjs::enable("workflow_run")
      }) %...T!%
      (function(error){
        print(error)
        ipc.progress$close()
        error_status = as.character(translator$translate("EXEC_JOB_STATUS_ERROR")); attr(error_status, "status_code") = "error"
        reactive_job_status(error_status)
        showModal(modalDialog(title = "Error",
                              p(sprintf(as.character(translator$translate("EXEC_JOB_ERROR_MESSAGE")), outconfig$profile$id)),
                              p(as.character(error)),
                              easyClose = TRUE, footer = NULL ))
        shinyjs::enable("workflow_run")
      })
    
    #Return something other than the future so we don't block the UI
    NULL
  })

 })
  
}
