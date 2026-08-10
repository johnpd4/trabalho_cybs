source("funcs.R")

main_BDP_ui = function(){
  
  nav_panel(
    
    title = "Birth and Death Processes",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "20%",
        
        radioButtons("process_type",
                     "Type of Process to be Simulated",
                     choices = c("Linear", "Immigration", "Restricted Growth", "SIS")),
        
        numericInputIcon("a",
                         "Starting Value",
                         min = 1,
                         max = 100,
                         value = 10),
        
        sliderInput("t_max",
                    "Time to Simulate",
                    min = 0.1,
                    max = 10,
                    value = 1),
        
        uiOutput("process_controls"),
        
      ), # sidebar
      
      plotlyOutput("process_plot")
      
    ) # layout sidebar
    
  ) # nav panel
  
} # ui

main_BDP_server = function(input, output, session){
 
  output$process_controls = renderUI({
    
    if(tolower(input$process_type) == "linear"){
      dynamic_buttons(ids = c("gamma", "mu"),
                      labels = c("Gamma", "Mu"),
                      default_vals = c(0.5, 0.3))
    } else if(tolower(input$process_type) == "immigration"){
      dynamic_buttons(ids = c("gamma", "mu", "nu"),
                      labels = c("Gamma", "Mu", "Nu"),
                      default_vals = c(0.5, 0.3, 0.2))
    } else if(tolower(input$process_type) == "restricted growth"){
      dynamic_buttons(ids = c("gamma", "mu", "beta"),
                      labels = c("Gamma", "Mu", "Beta"),
                      default_vals = c(0.5, 0.3, 1))
    } else if(tolower(input$process_type) == "sis"){
      dynamic_buttons(ids = c("beta", "gamma", "N"),
                      labels = c("Beta", "Gamma", "N"),
                      default_vals = c(0.5, 0.3, 5))
    }
    
  })
  
  output$process_plot = renderPlotly({
    
    fig = NULL
    
    for(i in 1:5){
      
      print(paste("iteration:", i, "tmax:", input$t_max))
    
      if(tolower(input$process_type) == "linear"){
        process = BDP(a = input$a, t_max = input$t_max,
                      method = "linear", theta = c(input$gamma, input$mu))
      } else if(tolower(input$process_type) == "immigration"){
        process = BDP(a = input$a, t_max = input$t_max,
                      method = "immigration", theta = c(input$gamma, input$mu, input$nu))
      } else if(tolower(input$process_type) == "restricted growth"){
        process = BDP(a = input$a, t_max = input$t_max,
                      method = "restricted_growth", theta = c(input$gamma, input$mu, input$beta))
      } else if(tolower(input$process_type) == "sis"){
        process = BDP(a = input$a, t_max = input$t_max,
                      method = "sis", theta = c(input$beta, input$gamma, input$N))
      }
      
      
      fig = plotly_process(process, old_fig = fig, add = !is.null(fig))
      
    }
    
    return(fig)
    
  })
  
}