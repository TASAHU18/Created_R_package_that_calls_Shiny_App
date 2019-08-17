
runClusterApplication <- function(){
library(shiny)
runApp(system.file("/server.R", package = "shinyClusters"))
}
