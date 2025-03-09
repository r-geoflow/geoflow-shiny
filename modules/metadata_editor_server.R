#metadata_editor_server
metadata_editor_server<- function(id, auth_info, i18n, geoflow_configs, parent.session){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    #reactives
    pageLoaded <- reactiveVal(FALSE)
    
    output$metadata_editor_info <- renderText({
      session$userData$module("metadata-editor")
      updateModuleUrl(session, "metadata-editor")
      text <- i18n()$t("METADATA_EDITOR_TITLE")
      pageLoaded(TRUE)
      text
    })
    
  
    output$meta_editor <- renderUI({
      tags$span("COMING SOON")
    })
    
  })
  
}