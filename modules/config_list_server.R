#config_list_server
config_list_server<- function(input, output, session){
  
  ns <- session$ns
  print(ns)

  
  getConfigurationFiles <- function() {
    list.files(GEOFLOW_DATA_DIR, pattern = ".json", full.names = TRUE)
  }
  
  config_react <- reactivePoll(10, session,
                               checkFunc = function(){
                                 length(getConfigurationFiles())
                               },
                               valueFunc = function(){
                                 getConfigurationFiles()
                               })
  
  #getConfigurations
  getConfigurations <- function(uuids = NULL){
    outlist <- getConfigurationFiles()
    out <- NULL
    if(length(outlist)==0){
       out <- tibble::tibble(
         Identifier = character(0),
         Name = character(0),
         Project = character(0),
         Organization = character(0),
         Mode = character(0),
         Actions = character(0)
       ) 
    }else{
      out <- do.call("rbind", lapply(1:length(outlist),function(i){
        x <- outlist[[i]]
        outconfig <- jsonlite::read_json(x)
        if(is.null(outconfig$profile$id)) outconfig$profile$id = outconfig$id
        if(is.null(outconfig$profile$mode)) outconfig$profile$mode = outconfig$mode
        out_tib <- tibble::tibble(
          Identifier = outconfig$profile$id,
          Name = outconfig$profile$name,
          Project = outconfig$profile$project,
          Organization = outconfig$profile$organization,
          Mode = outconfig$profile$mode,
          Actions = paste0(
            actionButton(inputId = ns(paste0('button_execute_', uuids[i])), class="btn btn-primary",
                         title = "Execute configuration", label = "", icon = icon("play"),
                         onclick = sprintf("Shiny.setInputValue('%s', this.id)",ns("select_button")))
          )
        )
        return(out_tib)
      }))
    }
    return(out)
  }
  
  #ShinyMonitor function
  
  shinyMonitor = function(step,config, entity,action){
      shiny::setProgress(value = step, 
                         message = sprintf("Worflow [%s] running :",config$profile$id),
                         detail = sprintf("Executing action: '%s' of entity: '%s' ... %s %%",action$id,entity$identifiers[["id"]],step))
  }
  
  #function to manage BUtton events
  manageButtonEvents <- function(prefix, uuids){
    outlist <- getConfigurationFiles()
    if(length(outlist)>0) lapply(1:length(outlist),function(i){
      x <- outlist[[i]]
      outconfig <- jsonlite::read_json(x)
      button_id <- paste0(prefix,uuids[i])
      observeEvent(input[[button_id]],{
        shinyjs::disable(button_id)
        out <- try(shiny::withProgress(value = 0,
                                       min=0,
                                       max=100,
                                       message = "Workflow initialization :",
                                       detail = "Connecting to softwares ... 0%" , 
                                       {geoflow::executeWorkflow(file = file.path(GEOFLOW_DATA_DIR, paste0(outconfig$profile$id, ".json")), 
                                                                 dir = GEOFLOW_DATA_DIR,
                                                                 monitor = shinyMonitor)}
                                   ))
        if(!is(out, "try-error")){
          showModal(modalDialog(title = "Success",
                      p(sprintf("Workflow '%s' has been successfully executed!", outconfig$profile$id)),
                      p(sprintf("See results at: %s", out)),
                      easyClose = TRUE, footer = NULL ))
        }else{
          print(as.character(out))
          showModal(modalDialog(title = "Error",
                      p(sprintf("Workflow '%s' has thrown an error!", outconfig$profile$id)),
                      p(unlist(strsplit(as.character(out)," : "))[2]),
                      easyClose = TRUE, footer = NULL ))
        }
        shinyjs::enable(button_id)
      })
    })
  }
  
  observeEvent(config_react(),{
    
    config_files <- config_react()
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
        paging = FALSE,
        searching = FALSE,
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    )
    manageButtonEvents("button_execute_", uuids)
  })
  
}