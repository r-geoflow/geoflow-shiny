footer <- function(id, version, date){
  tags$div(
    tags$p(sprintf("%s - v%s (%s)", id, version, date), style = "float:left;color:white;"),
    tags$p(paste(sessionInfo()$R.version$version.string,"– Resources: RAM:", round(benchmarkme::get_ram()/1e9,1),"GB", "–","CPU:", benchmarkme::get_cpu()$no_of_cores, "cores"), style = "float:right;")
  )
}

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
    type = "folder"
  )
}