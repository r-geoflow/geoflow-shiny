#get_cloud_files
get_cloud_files = function(auth_api, root, mime_types, recursive = TRUE){
  resources = auth_api$listFiles(root)
  resources = resources[order(resources$resourceType),]
  cloud_resources = resources[!is.na(resources$contentType) & resources$contentType %in% sapply(mime_types, mime::guess_type),]
  cloud_resources = cbind(
    path = if(nrow(cloud_resources)>0) file.path(root, cloud_resources$name) else character(0),
    cloud_resources
  )
  dir_resources = resources[resources$resourceType == "collection",]
  if(recursive){
    if(nrow(dir_resources)>0){
      child_resources = do.call("rbind", lapply(1:nrow(dir_resources), function(i){
        dir_resource_name = sub("/", "", basename(resources[i,]$name))
        get_cloud_files(auth_api, root = file.path(root, dir_resource_name), mime_types = mime_types, recursive = recursive)
      }))
      cloud_resources = rbind(cloud_resources, child_resources)
    }
  }
  return(cloud_resources)
}

#build_tree_data_dir
build_tree_data_dir = function(auth_api, root, mime_types, folder_basename = FALSE){
  resources = auth_api$listFiles(root)
  resources = resources[order(resources$resourceType),]
  resources = resources[is.na(resources$contentType)| resources$contentType %in% sapply(mime_types, mime::guess_type),]
  tree_data = list(
    text = ifelse(folder_basename, basename(root), root),
    children = if(nrow(resources)>0){ 
      lapply(1:nrow(resources), function(i){
        resource_name = sub("/", "", basename(resources[i,]$name))
        switch(resources[i,]$resourceType,
               "collection" = build_tree_data_dir(auth_api = auth_api, root = file.path(root, resource_name), mime_types = mime_types, folder_basename = TRUE),
               "file" = list(
                 text = resource_name,
                 data = file.path(root, resource_name),
                 type = "file"
               )
        )
      })
    }else{
      list()
    },
    data = root,
    type = "folder"
  )
}

#loadCloudTree
loadCloudTree = function(id, config, auth_api, leaves_only = FALSE, mime_types = c(".csv", ".xlsx", ".xls"), output){
  output[[id]] <- jsTreeR::renderJstree({
    req(!is.null(auth_api))
    jsTreeR::jstree(
      nodes = list(
        build_tree_data_dir(
          auth_api = auth_api, 
          root = config$data_dir_remote,
          mime_types = mime_types
        )
      ),
      types = list(
        file = list(icon = "jstree-file"),
        folder = list(icon = "jstree-folder")
      ),
      selectLeavesOnly = leaves_only,
      checkboxes = FALSE,
      multiple = FALSE,
      search = TRUE
    )
  })
}