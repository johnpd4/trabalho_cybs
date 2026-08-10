source("funcs.R")

pred_vs_real_ui = function(){
  
  nav_panel(
    
    title = "Estimated vs Real",
    
    layout_columns(
      
      card(class = "border-0 bg-transparent shadow-none",
           h1("Coloque texto explicando coisas aqui!")
      ),
      
      card(class = "border-0 bg-transparent shadow-none",
      
        layout_columns(
          
          card(
            style = "background-color: #636EFA !important;",
            h2("Process 1"),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                p("Real U: 0"),
                p("Real D: 0"),
                p("Real T: 0")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                p("Pred. U: 0"),
                p("Pred. D: 0"),
                p("Pred. T: 0")
              ),
            )
          ),
          
          card(
            style = "background-color: #EF553B !important;",
            h2("Process 2"),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   p("Real U: 0"),
                   p("Real D: 0"),
                   p("Real T: 0")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   p("Pred. U: 0"),
                   p("Pred. D: 0"),
                   p("Pred. T: 0")
              ),
            )
          )
          
        ),
        
        layout_columns(
          
          card(
            style = "background-color: #00CC96 !important;", 
            h2("Process 3"),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   p("Real U: 0"),
                   p("Real D: 0"),
                   p("Real T: 0")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   p("Pred. U: 0"),
                   p("Pred. D: 0"),
                   p("Pred. T: 0")
              ),
            )
          ),
          card(
            style = "background-color: #AB63FA !important;",
            h2("Process 4"),
            layout_columns(
              card(class = "border-0 bg-transparent shadow-none",
                   p("Real U: 0"),
                   p("Real D: 0"),
                   p("Real T: 0")
              ),
              card(class = "border-0 bg-transparent shadow-none",
                   p("Pred. U: 0"),
                   p("Pred. D: 0"),
                   p("Pred. T: 0")
              ),
            )
          )
          
        ),
        
        card(
          style = "width: 50%; margin-left: auto; margin-right: auto; background-color: #FFA15A !important;",
          h2("Process 5"),
          layout_columns(
            card(class = "border-0 bg-transparent shadow-none",
                 p("Real U: 0"),
                 p("Real D: 0"),
                 p("Real T: 0")
            ),
            card(class = "border-0 bg-transparent shadow-none",
                 p("Pred. U: 0"),
                 p("Pred. D: 0"),
                 p("Pred. T: 0")
            ),
          )
        )
        
      ),
    
    )
    
  ) # nav panel
  
} # ui

pred_vs_real_server = function(input, output, session){
  
  
  
}