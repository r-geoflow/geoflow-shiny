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
  package <- getAppPackage()
  list_of_packages <- sapply(package$dependencies, function(x){
    pkg <- x$package
    if(is.null(pkg)) if("repo" %in% names(x)){
      parts <- unlist(strsplit(x$repo,"/"))
      if(length(parts)>1) pkg <- parts[2]
    }
    return(pkg)
  })
  invisible(lapply(list_of_packages, function(x) {
    require(x,character.only = TRUE, quietly = TRUE)
  }))
}
