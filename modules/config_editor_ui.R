#config_editor_ui
config_editor_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "config_editor",
    tags$script(HTML(sprintf("
      $(document).on('click', '.select-config-template', function() {
        Shiny.setInputValue(
          '%s',
          $(this).data('url'),
          {priority: 'event'}
        );
      });
    ", ns("config_template_selected")))),
    uiOutput(ns("config_editor_choices")),
    uiOutput(ns("config_editor"))
  )
}