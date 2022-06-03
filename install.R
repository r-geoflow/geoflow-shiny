package <- jsonlite::read_json('./srv/dcf-shiny/package.json')
invisible(lapply(package$dependencies, function(pkg){
  from <- 'cran'
  pkg_installer <- remotes::install_version
  if(!is.null(pkg$from)){
    from <- pkg$from
    pkg_installer <- try(eval(parse(text=paste0("remotes::install_",from))))
  }
  if(class(pkg_installer)[1] == "try-error") return(NULL)
  version <- ""
  if(!is.null(pkg$version)) version <- paste0("[",pkg$version,"]")
  cat(sprintf("Install package '%s' %s from '%s'\n", pkg$package, version, from))
  pkg_args <- pkg[names(pkg)!="from"]
  do.call(pkg_installer, pkg_args)
}))