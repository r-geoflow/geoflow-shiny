#config_list_server
config_list_server<- function(input, output, session, user, logged, parent.session){
  
  ns <- session$ns
  
  ipc.queue <- ipc::shinyQueue()
  ipc.queue$consumer$start()
  
  pageLoaded <- reactiveVal(FALSE)
  reactive_job <- reactiveVal(NULL)
  reactive_job_status <- reactiveVal("")
  reactive_job_progress <- reactiveVal(0)
  
  output$config_list_info <- renderText({
    session$userData$module("configuration-list")
    updateModuleUrl("configuration-list", session)
    text <- "<h2>List of <b>geoflow</b> configurations <small>Access your configuration, and execute them</small></h2><hr>"
    pageLoaded(TRUE)
    text
  })
  
  AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
  
  #FUNCTIONS
  #getConfigurationFiles
  getConfigurationFiles <- function() {
    if(appConfig$auth){
      INFO(sprintf("Listing configuration files in '%s' at '%s'", appConfig$data_dir_remote, appConfig$auth_url))
      switch(appConfig$auth_type,
        "ocs" = {
          outfiles <- list()
          data_dir_remote_exists <- TRUE
          folders <- unlist(strsplit(appConfig$data_dir_remote,"/"))
          folders <- folders[folders!=""]
          path <- "/"
          for(folder in folders){
            folder_path <- paste0(folder, "/")
            data_dir_remote_exists <- folder_path %in% AUTH_API$listFiles(relPath = path)$name
            if(!data_dir_remote_exists) break
            path <- paste0(path, folder_path)
          }
          if(data_dir_remote_exists){
            files <- AUTH_API$listFiles(relPath = appConfig$data_dir_remote)
            files <- files[files$contentType == "application/json",]
            if(nrow(files)>0) outfiles <- lapply(1:nrow(files), function(i){files[i,]})
          }
          outfiles
        },
        list())
    }else{
      list.files(GEOFLOW_DATA_DIR, pattern = ".json", full.names = TRUE)
    }
  }
  
  #getConfigurations
  getConfigurations <- function(uuids = NULL){
    outlist <- getConfigurationFiles()
    out <- NULL
    if(length(outlist)==0){
       out <- tibble::tibble(
         Name = character(0),
         Actions = character(0)
       ) 
    }else{
      out <- do.call("rbind", lapply(1:length(outlist),function(i){
        x <- outlist[[i]]
        
        out_tib <- tibble::tibble(
          Name = if(appConfig$auth){ x$name }else{ x },
          LastModified = if(appConfig$auth){
            switch(appConfig$auth_type,
              "ocs" = {
                as.POSIXct(x$lastModified)  
              }      
            )
          } else {
            file.info(x)$mtime
          },
          
          #onclick = paste0("window.open(window.location.href.split('?')[0] + '?module=configuration-editor&file=", if(appConfig$auth){ x$name }else{ x }, "','_self')")
          Actions = paste0(
            actionButton(inputId = ns(paste0('button_edit_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                         title = "Edit configuration", label = "", icon = icon("edit")),
            actionButton(inputId = ns(paste0('button_execute_', uuids[i])), class="btn btn-primary",
                         title = "Execute configuration", label = "", icon = icon("play"))
          )
        )
        return(out_tib)
      }))
    }
    return(out)
  }
  
  #functions to manage button events
  #edit
  manageButtonEditEvents <- function(uuids){
    prefix <- "button_edit_"
    outlist <- getConfigurationFiles()
    if(length(outlist)>0) lapply(1:length(outlist),function(i){
      x <- outlist[[i]]
      if(appConfig$auth) x <- x$name
      button_id <- paste0(prefix,uuids[i])
      observeEvent(input[[button_id]],{
        
        shinyjs::disable(button_id)
        
        filepath <- if(appConfig$auth){
          AUTH_API$downloadFile(relPath = appConfig$data_dir_remote, filename = x, outdir = tempdir())
        }else{
          x
        }
        print(filepath)
        updateTabItems(session = parent.session, "config", "config_editor")
        
      })
    })
  }
  #execute
  manageButtonExecuteEvents <- function(uuids){
    prefix <- "button_execute_"
    outlist <- getConfigurationFiles()
    if(length(outlist)>0) lapply(1:length(outlist),function(i){
      x <- outlist[[i]]
      if(appConfig$auth) x <- x$name
      button_id <- paste0(prefix,uuids[i])
      observeEvent(input[[button_id]],{
        
        shinyjs::disable(button_id)
        
        reactive_job("")
        reactive_job_status("Started")
        
        filepath <- if(appConfig$auth){
          AUTH_API$downloadFile(relPath = appConfig$data_dir_remote, filename = x, outdir = tempdir())
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
          message = sprintf("Workflow '%s'", outconfig$profile$id),
          detail = "Initializing workflow ... 0%"
        )
        future::future({
          ipc.queue$producer$fireEval(print("Process started for geoflow job"))
          ipc.queue$producer$fireAssignReactive("reactive_job_status", "Started")
           
          geoflow::executeWorkflow(
            file = filepath,
            dir = targetdir,
            queue = ipc.queue,
            on_initWorkflow = function(config, queue){
              queue$producer$fireEval(print("Successful workflow initialization"))
              ipc.queue$producer$fireAssignReactive("reactive_job_status", "In progress")
            },
            on_initWorkflowJob = function(config, queue){
              queue$producer$fireEval(print("Successful workflow job initialization"))
              queue$producer$fireAssignReactive("reactive_job", config$job)
            },
            on_closeWorkflow = function(config, queue){
              ipc.progress$close()
            },
            monitor = function(step, config, entity, action, queue){
              queue$producer$fireAssignReactive("reactive_job_progress", step)
              ipc.progress$set(
                value = step, 
                message = sprintf("Worflow [%s] running :",config$profile$id),
                detail = sprintf("Executing action: '%s' of entity: '%s' ... %s %%",action$id,entity$identifiers[["id"]],step)
              )
            }
          )
           
        }) %...>% 
          (function(result){
            reactive_job_status("Passed")
            showModal(modalDialog(title = "Success",
                                    p(sprintf("Workflow '%s' has been successfully executed!", outconfig$profile$id)),
                                    p(sprintf("See results at: %s", result)),
                                    easyClose = TRUE, footer = NULL))
              shinyjs::enable(button_id)
          }) %...T!%
          (function(error){
            ipc.progress$close()
            reactive_job_status("Failed")
            showModal(modalDialog(title = "Error",
                                  p(sprintf("Workflow '%s' has thrown an error!", outconfig$profile$id)),
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
  loadConfigurationFiles <- function(){
    config_files <- getConfigurationFiles()
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
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    )
    manageButtonEditEvents(uuids)
    manageButtonExecuteEvents(uuids)
  }
  
  observeEvent(pageLoaded(), {
    loadConfigurationFiles()
  })
  
  observeEvent(input$config_list_refresh,{
    loadConfigurationFiles()
  })
  
  #Interactive job status
  output$config_job_status <- renderUI({ 
    tags$span(
      if(reactive_job_status() == "In progress"){
        paste(reactive_job_status(), paste0("(", reactive_job_progress(),"%)"))
      }else{
        reactive_job_status()
      },
      class = switch(reactive_job_status(),
         "Started" = "label label-info",
         "In progress" = "label label-info",
         "Passed" = "label label-success",
         "Failed" = "label label-danger",
         "Aborted" = "label label-secondary",
         ""
      )) 
  })
  
  #Interactive job console log
  config_job_interactive_log <- reactivePoll(500, session,
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
     out_txt <- "No geoflow job started!"
     if(!is.null(reactive_job())){
       if(out_txt == ""){
         out_txt <- "Starting geoflow job..."
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
    tags$div(
      config_job_interactive_log(),
      style = "max-height:500px;font-size:80%;color:white;background-color:black;overflow-y:auto;white-space: pre-line;padding:2px;"
    )
  })
  
}