#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#scripts
#---------------------------------------------------------------------------------------
source("assets/ui_utils.R")
source("assets/package_utils.R")
source("assets/commons.R")


config_file = "D:/Documents/DEV/Packages/geoflow-shiny_config_inrae.yml"
#config_file <- "/etc/geoflow-shiny/config.yml"

if(!file.exists(config_file)) stop(sprintf("No configuration file at '%s'", config_file))
print(sprintf("Reading configuration file '%s'", config_file))
appConfig <- suppressWarnings(yaml::read_yaml(config_file))
print(appConfig)

#packages
#---------------------------------------------------------------------------------------
loadAppPackages()

#global settings
#---------------------------------------------------------------------------------------
future::plan(multisession)
GEOFLOW_DATA_DIR <- appConfig$data_dir_local
GEOFLOW_SHINY_ENV <- new.env()

#modules
#---------------------------------------------------------------------------------------
source("modules/auth_server.R")
source("modules/auth_ui.R")
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
onStop(function(){
  
})
