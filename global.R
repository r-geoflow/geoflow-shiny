#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)
options(shiny.reactlog=TRUE) 

#scripts
#---------------------------------------------------------------------------------------
source("assets/ui_utils.R")
source("assets/package_utils.R")
source("assets/auth_utils.R")
source("assets/geoflow_utils.R")
source("assets/commons.R")

#config
#---------------------------------------------------------------------------------------
config_file <- "resources/config.yml"
#test shiny server resource file existence (if mount through docker container)
shiny_server_config_file <- "/etc/geoflow-shiny/config.yml"
if(file.exists(shiny_server_config_file)) config_file <- shiny_server_config_file
#test local environment variable (for local use)
if(nzchar(Sys.getenv("GEOFLOW_CONFIG"))){
  if(file.exists(Sys.getenv("GEOFLOW_CONFIG"))) config_file <- Sys.getenv("GEOFLOW_CONFIG")
}
if(!file.exists(config_file)) stop(sprintf("No configuration file at '%s'", config_file))
appConfig <- read_config(config_file)

#packages
#---------------------------------------------------------------------------------------
loadAppPackages()

#global settings
#---------------------------------------------------------------------------------------
if(Sys.info()[["sysname"]] == "Windows"){
  future::plan(multisession)
}else{
  future::plan(multicore)
}
GEOFLOW_DATA_DIR <- appConfig$data_dir_local
GEOFLOW_SHINY_ENV <- new.env()

#keyring
#---------------------------------------------------------------------------------------
#keyring?
keyring_backend_name <- if(!is.null(appConfig$auth_keyring_backend)) appConfig$auth_keyring_backend else 'env'
keyring_backend <- keyring:::known_backends[[keyring_backend_name]]$new()
appConfig$keyring_backend = keyring_backend

#language/i18n
#---------------------------------------------------------------------------------------
if(is.null(appConfig$lang)) appConfig$lang <- "en"
translator <- shiny.i18n::Translator$new(translation_csvs_path = "./i18n")
translator$set_translation_language(appConfig$lang)


#modules
#---------------------------------------------------------------------------------------
source("modules/auth_server.R")
source("modules/auth_ui.R")
source("modules/home_server.R")
source("modules/home_ui.R")
source("modules/metadata_editor_server.R")
source("modules/metadata_editor_ui.R")
source("modules/config_editor_server.R")
source("modules/config_editor_ui.R")
source("modules/config_list_server.R")
source("modules/config_list_ui.R")

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")
