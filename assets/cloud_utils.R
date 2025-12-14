#build_tree_data_dir
build_tree_data_dir = function(auth_api, root, mime_types, folder_basename = FALSE){
  resources = auth_api$listFiles(root)
  resources = resources[order(resources$resourceType),]
  resources = resources[is.na(resources$contentType)| resources$contentType %in% sapply(mime_types, mime::guess_type),]
  tree_data = list(
    text = ifelse(folder_basename, basename(root), root),
    children = if(nrow(resources)>0){ 
      lapply(1:nrow(resources), function(i){
        switch(resources[i,]$resourceType,
               "collection" = build_tree_data_dir(auth_api = auth_api, root = file.path(root, resources[i,]$name), mime_types = mime_types, folder_basename = TRUE),
               "file" = list(
                 text = resources[i,]$name,
                 data = file.path(root, resources[i,]$name),
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