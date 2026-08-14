source("funcs.R")

pred_vs_real_ui = function(){
  
  nav_panel(
    
    title = "Prediction vs Real Process",
    
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        width = "20%",
        
        radioButtons("current_process",
                     "Choose which Process to Analyze",
                     choices = c("Process 1", "Process 2", "Process 3",
                                 "Process 4", "Process 5")),
        
        sliderInput("num_rand_processes",
                    "Number of 'Random' Processes",
                    min = 0,
                    max = 50,
                    value = 10,
                    step = 1)
        
      ), # sidebar
      
      plotlyOutput("main_plot")
      
    ), # layout sidebar
    
  ) # nav panel
  
} # ui

pred_vs_real_server = function(input, output, session, process_list){
  
  process = reactive({
    
    process_list = process_list()
    
    current_process = input$current_process
    
    current_process = substring(current_process, nchar(current_process)) |> as.numeric()
    
    return(process_list[[current_process]])
    
  })
  
  param_list = reactive({
    process = process()
    param_list = list(a = process |> getElement("state") |> getElement(1),
                      t_max = process |> getElement("T.") |> sum(),
                      U = process |> getElement("U") |> sum(),
                      D = process |> getElement("D") |> sum())
    return(param_list)
    
  })
  
  output$main_plot = renderPlotly({
    
    param_list = param_list()
    a = param_list |> getElement("a")
    t_max = param_list |> getElement("t_max")
    U = param_list |> getElement("U")
    D = param_list |> getElement("D")
    
    
    fig = plotly_process(extreme_paths(a = a, t_max = t_max, U = U, D = D, lowest = T),
                         name = "Lowest Path", alpha = 0.75)
    fig = plotly_process(extreme_paths(a = a, t_max = t_max, U = U, D = D, lowest = F),
                         name = "Highest Path", alpha = 0.75, old_fig = fig)
    
    for(i in 1:input$num_rand_processes){
      fig = plotly_process(random_path(a = a, t_max = t_max, U = U, D = D),
                           name = paste0("Random Path ", i), alpha = 0.35, old_fig = fig)
    }
    fig = plotly_process(process(), name = "Original Process", old_fig = fig, color = "#000000")
    
    return(fig)
    
  })
  
}