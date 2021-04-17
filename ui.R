# Define UI for application that draws a histogram
#==========================================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$span("Geoflow UI", style = "font-size:80%;")
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