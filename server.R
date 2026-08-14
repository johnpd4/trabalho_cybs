if(!require(pacman)){install.packages("pacman")}
pacman::p_load(plotly, purrr, shiny, scales, patchwork,
               ggplot2, forcats, shinyWidgets, combinat)

source("funcs.R")
source("tabs/main_BDP.R")
source("tabs/sufficient_statistics.R")
source("tabs/pred_vs_real.R")

server = function(input, output, session){
  
  # in tabs/main_BDP.R
  process_list = main_BDP_server(input, output, session)
  
  # in tabs/sufficient_statistics.R
  sufficient_statistics_server(input, output, session, process_list)
  
  # in tabs/pred_vs_real.R
  pred_vs_real_server(input, output, session, process_list)
  
}