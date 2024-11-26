#initAuthSessionVariables
initAuthSessionVariables <- function(session, auth_info){
  
  session$userData$GEOFLOW_SHINY_AUTH_URL = auth_info$endpoint$auth_url
  session$userData$GEOFLOW_SHINY_AUTH_USER = auth_info$user
  if(!is.null(auth_info$backend) && !is.null(auth_info$service)){
    session$userData$GEOFLOW_SHINY_AUTH_PWD = auth_info$backend$get(service = auth_info$service, username = auth_info$user)
  }
  if(!is.na(auth_info$token)) session$userData$GEOFLOW_SHINY_AUTH_TOKEN = auth_info$token
}

#resetAuthSessionVariables
resetAuthSessionVariables <- function(session){
  session$userData$GEOFLOW_SHINY_AUTH_URL = NULL
  session$userData$GEOFLOW_SHINY_AUTH_USER = NULL
  session$userData$GEOFLOW_SHINY_AUTH_PWD = NULL
  session$userData$GEOFLOW_SHINY_AUTH_TOKEN = NULL
}

#decodeJWT
decodeJWT <- function(jwt){
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])
  out_jwt <- jsonlite::parse_json(rawToChar(jose::base64url_decode(strings[2])))
  out_jwt$expired <- as(Sys.time(), "numeric") > out_jwt$exp
  out_jwt$jwt <- jwt
  
  #TODO how to know the current VRE context?
  vre_contexts <- names(out_jwt$resource_access)[startsWith(names(out_jwt$resource_access), "%2Fd4science.research-infrastructures.eu")]
  if(length(vre_contexts)==0) stop("No VRE context available!")
  out_jwt$vre_context <- vre_contexts[[1]]
  
  out_jwt$vre_resource_access <- out_jwt$resource_access[[out_jwt$vre_context]]
  out_jwt$shiny_resource_access <- out_jwt$resource_access[["geoflow-shiny"]]
  if(out_jwt$expired) stop("JWT token is expired")
  if(!out_jwt$expired){
    req <- httr::with_verbose(httr::POST(
      sprintf("%s/protocol/openid-connect/token",out_jwt$iss),
      encode = "form",
      httr::add_headers("Authorization" = paste("Bearer", jwt)),
      body = list(
        grant_type = I("urn:ietf:params:oauth:grant-type:uma-ticket"),
        audience = URLencode(out_jwt$vre_context, reserved = TRUE)
      )
    ))
    if(httr::status_code(req)==200){
      out_jwt$access <- content(req)
    }
  }
  
  return(out_jwt)
}