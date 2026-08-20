source("funcs.R")

sufficient_statistics_ui = function(){
  
  nav_panel(
    
    title = "Sufficient Statistics",
    
    layout_columns(
      
      card(class = "border-0 bg-transparent shadow-none",
           h1("Coloque texto explicando coisas aqui!"),
           card(
             h3("Controles Avançados"),
             
             numericInput("Mj",
                          "Mj",
                          min = 5,
                          max = 50,
                          value = 35),
             
             numericInput("M0",
                          "M0",
                          min = 5,
                          max = 50,
                          value = 35),
             
             numericInput("J",
                          "J",
                          min = 5,
                          max = 50,
                          value = 35),
             
             numericInput("K",
                          "K",
                          min = 5,
                          max = 50,
                          value = 35),
             
           )
      ),
      
      card(class = "border-0 bg-transparent shadow-none",
      
        layout_columns(
          
          card(
            style = "background-color: #636EFA !important;",
            layout_columns(
              style = "align-items: baseline;",
              h2("Process 1"), textOutput("Y_p1")
            ),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_real_p1"),
                   textOutput("D_real_p1"),
                   textOutput("T_real_p1")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_est_p1"),
                   textOutput("D_est_p1"),
                   textOutput("T_est_p1")
              ),
            )
          ),
          
          card(
            style = "background-color: #EF553B !important;",
            layout_columns(
              style = "align-items: baseline;",
              h2("Process 2"), textOutput("Y_p2")
            ),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_real_p2"),
                   textOutput("D_real_p2"),
                   textOutput("T_real_p2")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_est_p2"),
                   textOutput("D_est_p2"),
                   textOutput("T_est_p2")
              ),
            )
          )
          
        ),
        
        layout_columns(
          
          card(
            style = "background-color: #00CC96 !important;", 
            layout_columns(
              style = "align-items: baseline;",
              h2("Process 3"), textOutput("Y_p3")
            ),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_real_p3"),
                   textOutput("D_real_p3"),
                   textOutput("T_real_p3")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_est_p3"),
                   textOutput("D_est_p3"),
                   textOutput("T_est_p3")
              ),
            )
          ),
          card(
            style = "background-color: #AB63FA !important;",
            layout_columns(
              style = "align-items: baseline;",
              h2("Process 4"), textOutput("Y_p4")
            ),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_real_p4"),
                   textOutput("D_real_p4"),
                   textOutput("T_real_p4")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   textOutput("U_est_p4"),
                   textOutput("D_est_p4"),
                   textOutput("T_est_p4")
              ),
            )
          )
          
        ),
        
        card(
          style = "width: 50%; margin-left: auto; margin-right: auto; background-color: #FFA15A !important;",
          layout_columns(
            style = "align-items: baseline;",
            h2("Process 5"), textOutput("Y_p5")
          ),
          layout_columns(
            card(class = "border-0 bg-transparent shadow-none",
                 textOutput("U_real_p5"),
                 textOutput("D_real_p5"),
                 textOutput("T_real_p5")
            ),
            card(class = "border-0 bg-transparent shadow-none",
                 textOutput("U_est_p5"),
                 textOutput("D_est_p5"),
                 textOutput("T_est_p5")
            ),
          )
        )
        
      ),
    
    )
    
  ) # nav panel
  
} # ui

sufficient_statistics_server = function(input, output, session, process_list){
  
  sufficient_list = reactive({
    
    x = list()
    process_list = process_list()
    
    for(i in 1:length(process_list)){
      x[[i]] = real_suff_stats(process_list[[i]])
    }
    
    return(x)
    
  })
  
  process_type = reactive({
    return(attributes(process_list())$type)
  })
  
  theta_vec = reactive({
    return(attributes(process_list())$theta)
  })
  
  theta_update = reactive({
    
    x = switch(process_type(),
               "linear" = theta_update_simple_linear,
               "immigration" = theta_update_immigration,
               "restricted_growth" = theta_update_restricted_growth,
               "sis" = theta_update_sis)
    
    return(x)
    
  })
  
  Y_list = reactive({
    
    process_list = process_list()
    
    Y_list = list()
    
    for(i in 1:length(process_list)){
      
      Y_list[[i]] = get_Y(process_list[[i]])
      
    }
    
    return(Y_list)
    
  })
  
  EU_list = reactive({

    Y_list = Y_list()

    EU_list = list()
    
    theta = theta_vec()
    
    update_func = theta_update()

    for(i in 1:length(Y_list)){

      Y = Y_list[[i]]
      
      a = Y[[1]]
      b = Y[[2]]
      t = Y[[3]]
      
      EU_list[[i]] = E_U(a = a, b = b, t = t,
                         K = input$K, M0 = input$M0,
                         Mj = input$Mj, J = input$J,
                         theta_update = update_func,
                         theta = theta)

    }

    return(EU_list)

  })
  
  ED_list = reactive({
    
    Y_list = Y_list()
    
    ED_list = list()
    
    theta = theta_vec()
    
    update_func = theta_update()
    
    for(i in 1:length(Y_list)){
      
      Y = Y_list[[i]]
      
      a = Y[[1]]
      b = Y[[2]]
      t = Y[[3]]
      
      ED_list[[i]] = E_D(a = a, b = b, t = t,
                         K = input$K, M0 = input$M0,
                         Mj = input$Mj, J = input$J,
                         theta_update = update_func,
                         theta = theta)
      
    }
    
    return(ED_list)
    
  })
  
  ET_list = reactive({
    
    Y_list = Y_list()
    
    ET_list = list()
    
    theta = theta_vec()
    
    update_func = theta_update()
    
    for(i in 1:length(Y_list)){
      
      Y = Y_list[[i]]
      
      a = Y[[1]]
      b = Y[[2]]
      t = Y[[3]]
      
      ET_list[[i]] = E_T(a = a, b = b, t = t,
                         K = input$K, M0 = input$M0,
                         Mj = input$Mj, J = input$J,
                         theta_update = update_func,
                         theta = theta)
      
    }
    
    return(ET_list)
    
  })
  
  output$Y_p1 = renderText({
    Y_list = Y_list() |> getElement(1)
    paste0("Y = (a = ", Y_list[1],
           ", b = ", Y_list[2],
           ", t = ", Y_list[3], ")")
  })
  
  output$Y_p2 = renderText({
    Y_list = Y_list() |> getElement(2)
    paste0("Y = (a = ", Y_list[1],
           ", b = ", Y_list[2],
           ", t = ", Y_list[3], ")")
  })
  
  output$Y_p3 = renderText({
    Y_list = Y_list() |> getElement(3)
    paste0("Y = (a = ", Y_list[1],
           ", b = ", Y_list[2],
           ", t = ", Y_list[3], ")")
  })
  
  output$Y_p4 = renderText({
    Y_list = Y_list() |> getElement(4)
    paste0("Y = (a = ", Y_list[1],
           ", b = ", Y_list[2],
           ", t = ", Y_list[3], ")")
  })
  
  output$Y_p5 = renderText({
    Y_list = Y_list() |> getElement(5)
    paste0("Y = (a = ", Y_list[1],
           ", b = ", Y_list[2],
           ", t = ", Y_list[3], ")")
  })
  
  output$U_real_p1 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[1]] |> getElement("U") |> sum()
    paste0("Real U: ", x)
  })
  
  output$U_real_p2 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[2]] |> getElement("U") |> sum()
    paste0("Real U: ", x)
  })
  
  output$U_real_p3 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[3]] |> getElement("U") |> sum()
    paste0("Real U: ", x)
  })
  
  output$U_real_p4 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[4]] |> getElement("U") |> sum()
    paste0("Real U: ", x)
  })
  
  output$U_real_p5 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[5]] |> getElement("U") |> sum()
    paste0("Real U: ", x)
  })
  
  output$U_est_p1 = renderText({
    EU_list = EU_list()
    x = EU_list[[1]] |> round(1)
    paste0("Est. U: ", x)
  })
  
  output$U_est_p2 = renderText({
    EU_list = EU_list()
    x = EU_list[[2]] |> round(1)
    paste0("Est. U: ", x)
  })
  
  output$U_est_p3 = renderText({
    EU_list = EU_list()
    x = EU_list[[3]] |> round(1)
    paste0("Est. U: ", x)
  })
  
  output$U_est_p4 = renderText({
    EU_list = EU_list()
    x = EU_list[[4]] |> round(1)
    paste0("Est. U: ", x)
  })
  
  output$U_est_p5 = renderText({
    EU_list = EU_list()
    x = EU_list[[5]] |> round(1)
    paste0("Est. U: ", x)
  })
  
  output$D_real_p1 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[1]] |> getElement("D") |> sum()
    paste0("Real D: ", x)
  })
  
  output$D_real_p2 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[2]] |> getElement("D") |> sum()
    paste0("Real D: ", x)
  })
  
  output$D_real_p3 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[3]] |> getElement("D") |> sum()
    paste0("Real D: ", x)
  })
  
  output$D_real_p4 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[4]] |> getElement("D") |> sum()
    paste0("Real D: ", x)
  })
  
  output$D_real_p5 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[5]] |> getElement("D") |> sum()
    paste0("Real D: ", x)
  })
  
  output$D_est_p1 = renderText({
    ED_list = ED_list()
    x = ED_list[[1]] |> round(1)
    paste0("Est. D: ", x)
  })
  
  output$D_est_p2 = renderText({
    ED_list = ED_list()
    x = ED_list[[2]] |> round(1)
    paste0("Est. D: ", x)
  })
  
  output$D_est_p3 = renderText({
    ED_list = ED_list()
    x = ED_list[[3]] |> round(1)
    paste0("Est. D: ", x)
  })
  
  output$D_est_p4 = renderText({
    ED_list = ED_list()
    x = ED_list[[4]] |> round(1)
    paste0("Est. D: ", x)
  })
  
  output$D_est_p5 = renderText({
    ED_list = ED_list()
    x = ED_list[[5]] |> round(1)
    paste0("Est. D: ", x)
  })
  
  output$T_real_p1 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[1]] |> getElement("T.") |> sum()
    paste0("Real T: ", x)
  })
  
  output$T_real_p2 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[2]] |> getElement("T.") |> sum()
    paste0("Real T: ", x)
  })
  
  output$T_real_p3 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[3]] |> getElement("T.") |> sum()
    paste0("Real T: ", x)
  })
  
  output$T_real_p4 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[4]] |> getElement("T.") |> sum()
    paste0("Real T: ", x)
  })
  
  output$T_real_p5 = renderText({
    sufficient_list = sufficient_list()
    x = sufficient_list[[5]] |> getElement("T.") |> sum()
    paste0("Real T: ", x)
  })
  
  output$T_est_p1 = renderText({
    ET_list = ET_list()
    x = ET_list[[1]] |> round(1)
    paste0("Est. T: ", x)
  })
  
  output$T_est_p2 = renderText({
    ET_list = ET_list()
    x = ET_list[[2]] |> round(1)
    paste0("Est. T: ", x)
  })
  
  output$T_est_p3 = renderText({
    ET_list = ET_list()
    x = ET_list[[3]] |> round(1)
    paste0("Est. T: ", x)
  })
  
  output$T_est_p4 = renderText({
    ET_list = ET_list()
    x = ET_list[[4]] |> round(1)
    paste0("Est. T: ", x)
  })
  
  output$T_est_p5 = renderText({
    ET_list = ET_list()
    x = ET_list[[5]] |> round(1)
    paste0("Est. T: ", x)
  })
  
}