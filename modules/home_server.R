#home_ui
home_server <- function(id, auth_info, i18n, geoflow_configs, parent.session){
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(i18n(),{
      #update i18n to module session
      #shiny.i18n::update_lang(i18n()$get_translation_language(), session = session)
    })
    
    output$home_info <- renderUI({
      session$userData$module("home")
      updateModuleUrl(session, "home")
      tags$h2("geoflow-shiny",tags$small(HTML(paste(translator$t("HOME_TITLE"),"<a href='https://github.com/r-geoflow/geoflow' target='_blank'>geoflow</a>"))))
    })
    
    output$boxes = renderUI({
      fluidRow(
        valueBox(
          elevation = 4,
          value = length(geoflow_configs()),
          width = 3,
          gradient = TRUE,
          subtitle = i18n()$t("HOME_WORKFLOWS"),
          color = "primary",
          icon = icon("gears")
        )
      )
    })
    
  })
}
