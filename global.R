#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#packages
#---------------------------------------------------------------------------------------
list_of_packages <- c(
  "shiny", "shinydashboard", "shinyjs",
  "geoflow", "jsonlite", "DT", "tibble"
)
invisible(lapply(list_of_packages, function(x) {
  if(!require(x,character.only = TRUE, quietly = TRUE)){
    install.packages(x,repos = "https://cran.rstudio.com/")
    require(x,character.only = TRUE, quietly = TRUE)
  }
}))

#Github packages
if(!require(shinyvalidate)){
  remotes::install_github("rstudio/shinyvalidate")
  require(shinyvalidate)
}

#global variables / environment
#---------------------------------------------------------------------------------------
GEOFLOW_DATA_DIR <- Sys.getenv("GEOFLOW_DATA_DIR")
if(GEOFLOW_DATA_DIR=="") GEOFLOW_DATA_DIR <- getwd()

#utilities
#---------------------------------------------------------------------------------------
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


#local datasets
#---------------------------------------------------------------------------------------

#modules
#---------------------------------------------------------------------------------------
source("modules/config_editor_server.R")
source("modules/config_editor_ui.R")
source("modules/config_list_server.R")
source("modules/config_list_ui.R")

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

#onStop
#---------------------------------------------------------------------------------------
