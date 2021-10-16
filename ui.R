# Define UI for application that draws a histogram
#==========================================================================================
ui_base <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      ifelse(
        !is.null(appConfig$logo) && !is.null(appConfig$url),
        tags$a(
          href = appConfig$url,
          tags$img(src= appConfig$logo, height='30', width='50')
        ),
        ""
      ),
      tags$span(appConfig$title, style = "font-size:80%;")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Configuration",
        tabName = "config",
        menuSubItem(text = "Configuration editor", tabName = "config_editor"),
        menuSubItem(text = "List of configurations", tabName = "config_list")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "geoflow-shiny.css")
    ),
    tabItems(
      config_editor_ui("config_editor"),
      config_list_ui("config_list")
    ),
    useShinyjs()
  )
)
ui <- if(appConfig$auth){
  secure_app(
    ui = ui_base,
    background = appConfig$auth_background,
    language = appConfig$lang
  )
}else{
  ui_base
}