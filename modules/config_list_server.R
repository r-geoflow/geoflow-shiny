#config_list_server
config_list_server<- function(id, auth_info, i18n, geoflow_configs, parent.session){
  
 moduleServer(id, function(input, output, session){
  
  ns <- session$ns
  
  ipc.queue <- ipc::shinyQueue()
  ipc.queue$consumer$start()
  
  #reactives
  pageLoaded <- reactiveVal(FALSE)
  reactive_job <- reactiveVal(NULL)
  empty_status = ""; attr(empty_status, "status_code") = ""
  reactive_job_status <- reactiveVal(empty_status)
  reactive_job_progress <- reactiveVal(0)
  
  #event on language change
  observeEvent(i18n(),{
    
    #update job_status (handled as reactive)
    status_code = attr(reactive_job_status(),"status_code")
    translated_status = if(status_code != ""){ translator$t(paste0("EXEC_JOB_STATUS_", toupper(status_code))) }else{ "" }
    attr(translated_status, "status_code") = status_code
    reactive_job_status(translated_status)
    
  })
  
  output$config_list_info <- renderText({
    session$userData$module("configuration-list")
    updateModuleUrl(session, "configuration-list")
    text <- i18n()$t("EXEC_TITLE")
    pageLoaded(TRUE)
    text
  })
  
  AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
  
  #getConfigurations
  getConfigurations <- function(uuids = NULL){
    outlist <- geoflow_configs()
    out <- NULL
    if(length(outlist)==0){
       out <- tibble::tibble(
         Name = character(0),
         LastModified = character(0),
         Actions = character(0)
       ) 
    }else{
      out <- do.call("rbind", lapply(1:length(outlist),function(i){
        x <- outlist[[i]]
        
        out_tib <- tibble::tibble(
          Name = if(appConfig$auth){ x$name }else{ x },
          LastModified = if(appConfig$auth){
            switch(auth_info()$endpoint$auth_type,
              "ocs" = {
                as.POSIXct(x$lastModified)  
              },
              "d4science" = {
                as.POSIXct(x$lastModificationTime/1000, origin = "1970-01-01")
              }
            )
          } else {
            file.info(x)$mtime
          },
          
          #onclick = paste0("window.open(window.location.href.split('?')[0] + '?module=configuration-editor&file=", if(appConfig$auth){ x$name }else{ x }, "','_self')")
          Actions = paste0(
            actionButton(inputId = ns(paste0('button_edit_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                         title = i18n()$t("EXEC_BUTTON_EDIT"), label = "", icon = icon("edit")),
            actionButton(inputId = ns(paste0('button_execute_', uuids[i])), class="btn btn-primary",
                         title = i18n()$t("EXEC_BUTTON_EXECUTE"), label = "", icon = icon("play"))
          )
        )
        return(out_tib)
      }))
    }
    colnames(out) <- c(i18n()$t("TABLE_NAME"), i18n()$t("TABLE_LASTMODIFIED"), i18n()$t("TABLE_ACTIONS"))
    return(out)
  }
  
  #functions to manage button events
  #edit
  manageButtonEditEvents <- function(uuids){
    prefix <- "button_edit_"
    outlist <- geoflow_configs()
    if(length(outlist)>0) lapply(1:length(outlist),function(i){
      x <- outlist[[i]]
      if(appConfig$auth) x <- x$name
      button_id <- paste0(prefix,uuids[i])
      observeEvent(input[[button_id]],{
        shinyjs::disable(button_id)
        updateModuleUrl(session, "configuration-editor", file = x)
        updateTabItems(session = parent.session, "geoflow-tabs", "config_editor")
        shinyjs::enable(button_id)
      })
    })
  }
  
  #execute
  manageButtonExecuteEvents <- function(uuids){
    prefix <- "button_execute_"
    outlist <- geoflow_configs()
    if(length(outlist)>0) lapply(1:length(outlist),function(i){
      x <- outlist[[i]]
      if(appConfig$auth) x <- x$name
      button_id <- paste0(prefix,uuids[i])
      observeEvent(input[[button_id]],{
        
        shinyjs::disable(button_id)
        
        reactive_job("")
        started = i18n()$t("EXEC_JOB_STATUS_STARTED");attr(started, "status_code") = "started"
        reactive_job_status(started)
        
        filepath <- if(appConfig$auth){
          switch(auth_info()$endpoint$auth_type,
            "ocs" = AUTH_API$downloadFile(relPath = appConfig$data_dir_remote, filename = x, outdir = tempdir()),
            "d4science" = AUTH_API$downloadItemByPath(path = file.path(appConfig$data_dir_remote, x), wd = tempdir())
          )
        }else{
          x
        }
        outconfig <- jsonlite::read_json(filepath)
        if(is.null(outconfig$profile$id)) outconfig$profile$id <- outconfig$id
        
        targetdir <- if(appConfig$auth){
          tempdir()
        }else{
          GEOFLOW_DATA_DIR
        }
        
        #geoflow execution
        ipc.progress <- ipc::AsyncProgress$new(
          value = 0, min = 0, max = 100,
          message = sprintf("%s '%s'", translator$t("EXEC_WORKFLOW"), outconfig$profile$id),
          detail = translator$t("EXEC_JOB_INITIALIZATION")
        )
        
        future::future({
          ipc.queue$producer$fireEval(print("Process started for geoflow job"))
          started = translator$t("EXEC_JOB_STATUS_STARTED"); attr(started, "status_code") = "started"
          ipc.queue$producer$fireAssignReactive("reactive_job_status", started)
          
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
              inprogress = translator$t("EXEC_JOB_STATUS_INPROGRESS"); attr(inprogress, "status_code") = "inprogress"
              ipc.queue$producer$fireAssignReactive("reactive_job_status", inprogress)
            },
            on_closeWorkflow = function(config, queue){
              ipc.progress$close()
            },
            monitor = function(step, config, entity, action, queue){
              queue$producer$fireAssignReactive("reactive_job_progress", step)
              ipc.progress$set(
                value = step, 
                message = sprintf("%s [%s] :", translator$t("EXEC_WORKFLOW"), config$profile$id),
                detail = sprintf(translator$t("EXEC_ACTION_EXEC_DETAIL"),action$id,entity$identifiers[["id"]],step)
              )
            },
            session = parent.session
          )
        }) %...>% 
          (function(result){
            success = translator$t("EXEC_JOB_STATUS_SUCCESS"); attr(success, "status_code") = "success"
            reactive_job_status(success)
            shinyjs::enable(button_id)
          }) %...T!%
          (function(error){
            ipc.progress$close()
            error_status = translator$t("EXEC_JOB_STATUS_ERROR"); attr(error_status, "status_code") = "error"
            reactive_job_status(error_status)
            showModal(modalDialog(title = "Error",
                                  p(sprintf(translator$t("EXEC_JOB_ERROR_MESSAGE"), outconfig$profile$id)),
                                  p(as.character(error)),
                                  easyClose = TRUE, footer = NULL ))
            shinyjs::enable(button_id)
          })
        
        #Return something other than the future so we don't block the UI
        NULL
      })
    })
  }
  
  #loadConfigurationFiles
  loadConfigurationFiles <- function(force = FALSE){
    print(i18n()$t("TABLE_LANGUAGE"))
    config_files <- geoflow_configs()
    if(force) config_files <- getConfigurationFiles(config = appConfig, auth_api = AUTH_API, auth_info = auth_info())
    uuids <- NULL
    if(length(config_files)>0) for(i in 1:length(config_files)){
      one_uuid = uuid::UUIDgenerate() 
      uuids <- c(uuids, one_uuid)
    }
    configs <- getConfigurations(uuids = uuids)
    
    output$config_list_table <- DT::renderDT(
      configs,
      server = FALSE,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        deferRender = TRUE,
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        language = list(url = i18n()$t("TABLE_LANGUAGE"))
      )
    )
    manageButtonEditEvents(uuids)
    manageButtonExecuteEvents(uuids)
  }
  
  observeEvent(geoflow_configs(),{
    INFO("TRIGGERED GEOFLOW CONFIGS!!!!")
    loadConfigurationFiles()
  })
  
  observeEvent(pageLoaded(), {
    loadConfigurationFiles()
  })
  
  observeEvent(input$config_list_refresh,{
    loadConfigurationFiles(force = TRUE)
  })
  
  #workflow manager
  output$workflow_manager <- renderUI({
    shiny::fluidRow(
      box(
        inputId = "config_list_wrapper", 
        title = tags$span(i18n()$t("EXEC_WORKFLOWS"), tags$small(actionLink(ns("config_list_refresh"), label = NULL, icon = icon("fas fa-sync")))), status = "primary", width = 6,
        tags$div(shinycssloaders::withSpinner(DT::DTOutput(ns("config_list_table"))), style = "font-size:80%;")
      ),
      box(
        inputId = "config_log_wrapper",
        title = i18n()$t("EXEC_CONSOLE"), status = "primary", width = 6,
        uiOutput(ns("config_job_status")),
        uiOutput(ns("config_job_download")),br(),br(),
        uiOutput(ns("config_job_interactive_log"))
      )
    )
  })
  
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
          ns("downloadJob"),
          i18n()$t("EXEC_BUTTON_JOB_ARCHIVE"),
          icon = icon("download")
        ),
        style = "float:right;margin-right:7.5px;"
      )
    }else{
      ""
    }
  })
  output$downloadJob <- downloadHandler(
    filename = function(){ paste0(basename(reactive_job()), ".zip") },
    content = function(con){
      disable("downloadJob")
      setwd(reactive_job())
      zip::zip(zipfile = con, files = list.files())
      enable("downloadJob")
    },
    contentType = "application/zip"
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
  
 })
  
}
