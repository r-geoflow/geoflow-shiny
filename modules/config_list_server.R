#config_list_server
config_list_server<- function(input, output, session){
  
  ns <- session$ns

  getConfigurations <- function(){
    outlist <- list.files("out/configs", pattern = ".json", full.names = TRUE)
    out <- NULL
    if(length(outlist)==0){
       out <- data.frame(
         Identifier = character(0),
         Name = character(0),
         Project = character(0),
         Organization = character(0),
         Mode = character(0),
         Actions = character(0),
         stringsAsFactors = FALSE
       ) 
    }else{
      out <- do.call("rbind", lapply(outlist,function(x){
        outconfig <- jsonlite::read_json(x)
        if(is.null(outconfig$profile$id)) outconfig$profile$id = outconfig$id
        if(is.null(outconfig$profile$mode)) outconfig$profile$mode = outconfig$mode
        return(data.frame(
          Identifier = outconfig$profile$id,
          Name = outconfig$profile$name,
          Project = outconfig$profile$project,
          Organization = outconfig$profile$organization,
          Mode = outconfig$profile$mode,
          Actions = "",
          stringsAsFactors = FALSE
        ))
      }))
    }
    return(out)
  }
  
  output$config_list_table <- DT::renderDT(
    getConfigurations(),
    options = list(lengthChange = FALSE)
  )
  
}