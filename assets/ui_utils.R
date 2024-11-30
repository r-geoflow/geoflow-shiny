footer <- function(id, version, date){
  tags$div(
    tags$p(sprintf("%s - v%s (%s)", id, version, date), style = "float:left;color:white;"),
    tags$p(paste(sessionInfo()$R.version$version.string,"– Resources: RAM:", round(benchmarkme::get_ram()/1e9,1),"GB", "–","CPU:", benchmarkme::get_cpu()$no_of_cores, "cores"), style = "float:right;")
  )
}