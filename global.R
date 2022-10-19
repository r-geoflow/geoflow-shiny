#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#scripts
#---------------------------------------------------------------------------------------
source("assets/ui_utils.R")
source("assets/package_utils.R")
source("assets/auth_utils.R")
source("assets/commons.R")

#config
#---------------------------------------------------------------------------------------
#config_file = "D:/Documents/DEV/Packages/geoflow-shiny_config_inrae.yml"
config_file <- "resources/config.yml"
#test shiny server resource file existence (if mount through docker container)
shiny_server_config_file <- "/etc/geoflow-shiny/config.yml"
if(file.exists(shiny_server_config_file)) config_file <- shiny_server_config_file
if(!file.exists(config_file)) stop(sprintf("No configuration file at '%s'", config_file))
appConfig <- read_config(config_file)

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
  resetAuthEnvironmentVariables()
})
