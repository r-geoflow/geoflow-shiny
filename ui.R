# Define UI for application that draws a histogram
#==========================================================================================
ui <- shiny::tagList(
 dashboardPage(
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
    ),
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout", icon = icon("sign-out", lib = "font-awesome"))
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    withSpinner(uiOutput("side_ui"))
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "geoflow-shiny.css")
    ),
    if(appConfig$auth) authLoginUI(
      id = "login",
      config = appConfig,
      title = appConfig$auth_title,
      cookie_expiry = if(!is.null(appConfig$auth_cookie_expiry)){appConfig$auth_cookie_expiry}else{7}, 
      additional_ui = tags$div(
        HTML(appConfig$auth_footer),
        HTML("<p style='font-size:80%;'>Powered by <a href='https://www.r-project.org/' target='_blank'>R</a> and <a href='https://github.com/eblondel/geoflow' target='_blank'>geoflow</a></p>")
      )
    ),
    withSpinner(uiOutput("main_ui")),
    useShinyjs()
  )
 ),
 tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)
