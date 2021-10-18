#logger
logger = function(type, text){cat(sprintf("[SRN-SHINY][%s] - %s \n", type, text))}
INFO = function(text){logger("INFO", text)}
WARN = function(text){logger("WARN", text)}
ERROR = function(text){logger("ERROR", text)}

#shinyInput
shinyInput <- function(FUN, len, indexes = NULL, id, ns, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    idx <- i
    if(!is.null(indexes)) idx <- indexes[i]
    inputs[i] <- as.character(FUN(paste0(ns(id), idx), ...))
  }
  inputs
}

#downloadButtonCustom
downloadButtonCustom <- function (outputId, label = "Download", class = NULL, href = "", icon = icon("download"), ...) {
  aTab <- tags$a(
    id = outputId, 
    class = paste("btn btn-default shiny-download-link", class),
    href = href,
    target = "_blank", 
    download = NA, 
    icon, 
    label, 
    ...
  )
}

#updateModuleUrl
updateModuleUrl <- function(module, session){
  updateQueryString(
    queryString = sprintf("?module=%s", module), 
    mode = "push", session
  )
}
