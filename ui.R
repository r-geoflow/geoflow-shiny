# Define UI for application that draws a histogram
#==========================================================================================
ui <- fluidPage(
  shinyjs::useShinyjs(),
  cookies::cookie_dependency(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "geoflow-shiny.css")
  ),
  div(id = "login-wrapper", authLoginUI(
    id = "login",
    config = appConfig,
    title = appConfig$auth_title,
    cookie_expiry = if(!is.null(appConfig$auth_cookie_expiry)){appConfig$auth_cookie_expiry}else{7},
    additional_ui = tags$div(
      HTML(appConfig$auth_footer),
      HTML("<p style='font-size:80%;'>Powered by <a href='https://www.r-project.org/' target='_blank'><img src='https://www.r-project.org/Rlogo.png' height='50' style='margin-right:10px'></a><a href='https://github.com/r-geoflow' target='_blank'><img src='https://github.com/r-geoflow/geoflow-logos/blob/main/geoflow_square.png?raw=true' height='60' /></a></p>")
    )
  )),
  shinyjs::hidden(
    div(id = "main-content",
        bs4Dash::bs4DashPage(
          header = bs4Dash::dashboardHeader(
            title = bs4Dash::dashboardBrand(
              title = appConfig$title,
              color = "primary",
              href = appConfig$url,
              image = appConfig$logo
            ),
            rightUi = if(appConfig$auth) tags$li(
              class = "dropdown",
              shinyauthr::logoutUI("logout", icon = icon("right-from-bracket"), style = NULL),
              uiOutput("user_avatar", inline = T)
            )
            
          ),
          sidebar = bs4Dash::dashboardSidebar(
            withSpinner(uiOutput("side_ui"))
          ),
          body = bs4Dash::dashboardBody(
            withSpinner(uiOutput("main_ui"))
          )
        )
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
 )

