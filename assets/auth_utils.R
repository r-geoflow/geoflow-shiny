#initAuthEnvironmentVariables
initAuthEnvironmentVariables <- function(auth_info){
  print(auth_info)
  Sys.setenv("GEOFLOW_SHINY_AUTH_URL" = auth_info$endpoint$auth_url)
  Sys.setenv("GEOFLOW_SHINY_AUTH_USER" = auth_info$user)
  Sys.setenv("GEOFLOW_SHINY_AUTH_PWD" = auth_info$backend$get(service = auth_info$service, username = auth_info$user))
}

#resetAuthEnvironmentVariables
resetAuthEnvironmentVariables <- function(){
  Sys.setenv("GEOFLOW_SHINY_AUTH_URL" = "")
  Sys.setenv("GEOFLOW_SHINY_AUTH_USER" = "")
  Sys.setenv("GEOFLOW_SHINY_AUTH_PWD" = "")
}