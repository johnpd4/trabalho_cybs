if(!require(pacman)){install.packages("pacman")}
pacman::p_load(shiny, bslib, plotly, shinyWidgets)

source("tabs/main_BDP.R")

ui = page_navbar(
  title = "BDP",
  theme = bs_theme(preset = "flatly"),
  
  # in tabs/main_dice.R
  main_BDP_ui(),
  
)