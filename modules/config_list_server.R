#config_list_server
config_list_server<- function(id, auth_info, parent.session){
  
 moduleServer(id, function(input, output, session){
  
  ns <- session$ns
  
  ipc.queue <- ipc::shinyQueue()
  ipc.queue$consumer$start()
  
  pageLoaded <- reactiveVal(FALSE)
  reactive_job <- reactiveVal(NULL)
  reactive_job_status <- reactiveVal("")
  reactive_job_progress <- reactiveVal(0)
  
  output$config_list_info <- renderText({
    session$userData$module("configuration-list")
    updateModuleUrl(session, "configuration-list")
    text <- "<h2>List of <b>geoflow</b> configurations <small>Access your configuration, and execute them</small></h2><hr>"
    pageLoaded(TRUE)
    text
  })
  
  AUTH_API <- try(get("AUTH_API", envir = GEOFLOW_SHINY_ENV), silent = TRUE)
  
  #FUNCTIONS
  #getConfigurationFiles
  getConfigurationFiles <- function() {
    if(appConfig$auth){
      INFO(sprintf("Listing configuration files in '%s' at '%s'", appConfig$data_dir_remote, auth_info()$endpoint$auth_url))
      switch(auth_info()$endpoint$auth_type,
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
            if(nrow(files)>0){
              files <- files[endsWith(files$name, ".json"),]
              outfiles <- lapply(1:nrow(files), function(i){files[i,]})
            }
          }
          outfiles
        },
        "d4science" = {
          outfiles <- list()
          files <- AUTH_API$listWSItemsByPath(appConfig$data_dir_remote)
          if(length(files)>0) if(nrow(files)>0){
            files <- files[endsWith(files$name, ".json"),]
            outfiles <- lapply(1:nrow(files), function(i){files[i,]})
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
    print(outlist)
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
    print(outlist)
    if(length(outlist)>0) lapply(1:length(outlist),function(i){
      x <- outlist[[i]]
      print(x)
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
            on_initWorkflowJob = function(config, queue){
              queue$producer$fireEval(print("Successful job directory creation"))
              queue$producer$fireAssignReactive("reactive_job", config$job)
            },
            on_initWorkflow = function(config, queue){
              queue$producer$fireEval(print("Successful workflow initialization"))
              ipc.queue$producer$fireAssignReactive("reactive_job_status", "In progress")
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
            },
            session = parent.session
          )
        }) %...>% 
          (function(result){
            reactive_job_status("Passed")
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
    div(
    tags$span(
      switch(reactive_job_status(),
       "In progress" = icon("gears"),
       "Passed" = icon("check"),
       "Failed" = icon("xmark"),
       "Aborted" = icon("exclamation"),
        ""
      ),
      if(reactive_job_status() == "In progress"){
        paste(reactive_job_status(), paste0("(", reactive_job_progress(),"%)"))
      }else{
        reactive_job_status()
      },
      class = switch(reactive_job_status(),
         "Started" = "badge badge-info",
         "In progress" = "badge badge-info",
         "Passed" = "badge badge-success",
         "Failed" = "badge badge-danger",
         "Aborted" = "badge badge-secondary",
         ""
      ), style="margin-left:7.5px;"),
      style = "float:left;"
    )
  })
  
  output$config_job_download <- renderUI({
    if(reactive_job_status() %in% c("Passed", "Failed")){
      div(
        downloadButtonCustom(
          ns("downloadJob"),
          'Download job archive',
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
            the_title <- parts[(i*2)-1]
            if(i==1){
             tags$span(fa("r-project", fill = "steelblue"), the_title, style = "font-weight:bold;")
            }else{
              if(i<nb_parts){
                tags$span(fa("far fa-circle-check"), the_title, style = "font-weight:bold;")
              }else{
                switch(reactive_job_status(),
                 "Started" = tags$span(fa("gears"), the_title, style = "font-weight:bold;"),
                 "In progress" = tags$span(fa("gears"), the_title, style = "font-weight:bold;"),
                 "Passed" = tags$span(fa("far fa-circle-check"), the_title, style = "font-weight:bold;"),
                 "Failed" = tags$span(fa("far fa-circle-xmark"), the_title, style = "font-weight:bold;")
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
                switch(reactive_job_status(),
                "Started" = "info",
                "In progress" = "info",
                "Passed" = "success",
                "Failed" = "danger"
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