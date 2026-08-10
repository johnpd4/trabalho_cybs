if(!require(pacman)){install.packages("pacman")}
pacman::p_load(plotly, purrr, shiny, scales, patchwork,
               ggplot2, forcats, shinyWidgets, combinat)

source("funcs.R")
source("tabs/main_BDP.R")

server = function(input, output, session){
  
  # in tabs/main_BDP.R
  main_BDP_server(input, output, session)
  
}