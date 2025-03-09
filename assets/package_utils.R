#getAppPackage
getAppPackage <- function(){
  package <- jsonlite::read_json('./package.json')
  return(package)
}

#getAppId
getAppId <- function(){
  package <- getAppPackage()
  return(package$id)
}

#getAppVersion
getAppVersion <- function(){
  package <- getAppPackage()
  return(package$version)
}

#getAppDate
getAppDate <- function(){
  package <- getAppPackage()
  return(package$date)
}

#loadAppPackages
loadAppPackages <- function(){
  # package <- getAppPackage()
  # list_of_packages <- sapply(package$dependencies, function(x){
  #   pkg <- x$package
  #   if(is.null(pkg)) if("repo" %in% names(x)){
  #     parts <- unlist(strsplit(x$repo,"/"))
  #     if(length(parts)>1) pkg <- parts[2]
  #   }
  #   return(pkg)
  # })
  # invisible(lapply(list_of_packages, function(x) {
  #   require(x,character.only = TRUE, quietly = TRUE)
  # }))
  #core libraries
  require(dotenv)
  require(keyring)
  require(httr)
  require(dplyr)
  require(yaml)
  require(jsonlite)
  require(uuid)
  require(fastmap)
  require(magrittr)
  require(promises)
  require(future)
  require(ipc)
  require(benchmarkme)
  require(atom4R)
  require(rmarkdown)
  
  #shiny stuff
  require(fontawesome)
  require(shiny)
  require(shinyauthr)
  require(cookies)
  require(shinyjs)
  require(shinycssloaders)
  require(bs4Dash)
  require(shiny.i18n)
  
  #tables
  require(DT)
  require(tibble)
  require(rhandsontable)
  require(readxl)
  require(gsheet)
  
  #dbi
  require(DBI)
  require(RMariaDB)
  require(RPostgres)
  require(RPostgreSQL)
  require(RSQLite)
  
  #geospatial
  require(geometa)
  require(geoflow)
  require(geonapi)
  require(ows4R)
  
  #apis
  require(googledrive)
  require(d4storagehub4R)
  require(zen4R)
  require(ocs4R)
  require(dataverse)
  require(blastula)
  
  #ncdf
  require(ncdf4)
  require(thredds)
  
  #eml
  require(EML)
  require(emld)
  require(datapack)
  require(dataone)
  
}
