#authLoginUI
authLoginUI <- function (id, config, title = "Please log in", cloud_title = "Cloud provider", user_title = "User Name", 
                         pass_title = "Password", login_title = "Log in", login_btn_class = "btn-primary", 
                         error_message = "Invalid username or password!", additional_ui = NULL, 
                         cookie_expiry = 7) 
{
  
  ns <- shiny::NS(id)
  shinyjs::hidden(
    shiny::div(
      id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;", 
      shiny::wellPanel(
        shinyjs::useShinyjs(), 
        shinyauthr:::jscookie_script(),
        shinyjs::extendShinyjs(
          text = shinyauthr:::js_cookie_to_r_code(ns("jscookie"), expire_days = cookie_expiry), 
          functions = c("getcookie","setcookie", "rmcookie")
        ), 
        shinyjs::extendShinyjs(text = shinyauthr:::js_return_click(ns("auth_password"), ns("auth_button_login")), functions = c()),
        shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
        shiny::selectizeInput(
          inputId = ns("auth_provider"), label = cloud_title, 
          choices = NULL,
          width = "100%", options = list(
            placeholder = "Select a cloud platform",
            onInitialize = I('function() { this.setValue(""); }'),
            render = I('{
              option: function(item, escape) {
                console.log(item);
                html = escape(item.label);
                if(item.logo != "") html = "<img src=\'" + item.logo + "\' height=\'30px\'/> " + html;
                html = "<div>" + html + "</div>";
                return html;
              },
              item: function(item, escape) {
                html = escape(item.label);
                if(item.logo != "") html = "<img src=\'" + item.logo + "\' height=\'30px\'/> " + html;
                html = "<div>" + html + "</div>";
                return html;
              }
            }')
          )
        ),
        shiny::textInput(ns("auth_username"), shiny::tagList(shiny::icon("user"), user_title)),
        shiny::passwordInput(ns("auth_password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title)), 
        shiny::div(style = "text-align: center;", shiny::actionButton(inputId = ns("auth_button_login"), class = login_btn_class, label = login_title)),
        additional_ui,
        shinyjs::hidden(shiny::div(id = ns("error"), shiny::tags$p(error_message, style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center")))
      )
    )
  )
}