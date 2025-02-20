#getConfigurationFiles
getConfigurationFiles <- function(config, auth_api = NULL, auth_info = NULL) {
  if(config$auth){
    INFO(sprintf("Listing configuration files in '%s' at '%s'", config$data_dir_remote, auth_info$endpoint$auth_url))
    switch(auth_info$endpoint$auth_type,
           "ocs" = {
             outfiles <- list()
             data_dir_remote_exists <- TRUE
             folders <- unlist(strsplit(config$data_dir_remote,"/"))
             folders <- folders[folders!=""]
             path <- "/"
             for(folder in folders){
               folder_path <- paste0(folder, "/")
               data_dir_remote_exists <- folder_path %in% auth_api$listFiles(relPath = path)$name
               if(!data_dir_remote_exists) break
               path <- paste0(path, folder_path)
             }
             if(data_dir_remote_exists){
               files <- auth_api$listFiles(relPath = config$data_dir_remote)
               files <- files[files$contentType == "application/json",]
               if(nrow(files)>0){
                 files <- files[!is.na(files$name) & endsWith(files$name, ".json"),]
                 outfiles <- lapply(1:nrow(files), function(i){files[i,]})
               }
             }
             outfiles
           },
           "d4science" = {
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