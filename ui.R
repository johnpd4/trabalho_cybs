if(!require(pacman)){install.packages("pacman")}
pacman::p_load(shiny, bslib, plotly, shinyWidgets)

source("tabs/main_BDP.R")
source("tabs/pred_vs_real.R")

ui = page_navbar(
  title = "BDP",
  theme = bs_theme(preset = "flatly"),
  
  # in tabs/main_BDP.R
  main_BDP_ui(),
  
  # in tabs/pred_vs_real.R
  pred_vs_real_ui(),
  
)