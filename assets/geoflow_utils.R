#getConfigurationFiles
getConfigurationFiles <- function(config, auth_api = NULL, auth_info = NULL) {
  if(config$auth){
    INFO(sprintf("Listing configuration files in '%s' at '%s'", config$data_dir_remote, auth_info$endpoint$auth_url))
    switch(auth_info$endpoint$auth_type,
           "ocs" = {
             files = get_cloud_files(auth_api, root = config$data_dir_remote, mime_types = c(".json", ".yaml", ".yml"), recursive = TRUE)
             outfiles <- list()
             if(nrow(files)>0){
               outfiles <- lapply(1:nrow(files), function(i){files[i,]})
             }
             outfiles
           },
           "d4science" = {
             #unmaintained
             outfiles <- list()
             files <- auth_api$listWSItemsByPath(config$data_dir_remote)
             if(length(files)>0) if(nrow(files)>0){
               files <- files[endsWith(files$name, ".json"),]
               outfiles <- lapply(1:nrow(files), function(i){files[i,]})
             }
             outfiles
           },
           list())
  }else{
    list.files(GEOFLOW_DATA_DIR, pattern = ".json", full.names = TRUE)
  }
}