#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#config
#---------------------------------------------------------------------------------------
if(!require("yaml")){
  install.packages("yaml", repos = "https://cloud.r-project.org")
  require(yaml)
}

config_file = "D:/Documents/DEV/Packages/geoflow-shiny/resources/config.yml"
#config_file <- "/etc/geoflow-shiny/config.yml"
if(!nzchar(config_file)) stop("No configuration file at '/etc/geoflow-shiny/config.yml'")
appConfig <- suppressWarnings(yaml::read_yaml(config_file))
print(appConfig)

#packages
#---------------------------------------------------------------------------------------
list_of_packages <- c(
  "shiny", "shinymanager", "shinydashboard", "shinyjs",
  "geoflow", "jsonlite", "DT", "tibble",
  "ocs4R"
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
if(GEOFLOW_DATA_DIR=="") GEOFLOW_DATA_DIR <- appConfig$data_dir_local

GEOFLOW_SHINY_ENV <- new.env()

#scripts
#---------------------------------------------------------------------------------------
source("scripts/commons.R")

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
