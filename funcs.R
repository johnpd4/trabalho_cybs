BDP = function(a, t_max, lambda_k, mu_k, theta){

  k = a
  tau = 0

  path = data.frame(state = numeric(),
                    U = numeric(),
                    D = numeric(),
                    T. = numeric())
  
  path[nrow(path)+1, ] = c(k, 0, 0, 0)

  while(sum(path$T.) <= t_max){

    prob_birth = lambda_k(k, theta)
    prob_death = mu_k(k, theta)
    
    tau = rexp(1, rate = (prob_birth + prob_death))
    
    if(sum(path$T.) + tau >= t_max){
      path[nrow(path)+1, ] = c(k, 0, 0, t_max - sum(path$T.))
      return(path)
    }

    birth_range = prob_birth
    death_range = prob_birth + prob_death

    random = runif(1)

    if(random <= birth_range){
      k = k + 1
      path[nrow(path)+1, ] = c(k, 1, 0, tau)
    } else if(random <= death_range){
      k = k - 1
      path[nrow(path)+1, ] = c(k, 0, 1, tau)
    } else {
      path[nrow(path)+1, ] = c(k, 0, 0, tau)
    }

  }
  
  return(path)

}

lambda_k_simple_linear = function(k, theta){
  
  if(length(theta) > 2){stop("Length of theta implies immigration, but birth function for simple linear was used!")}

  return(min(1, k*theta[1]))

}

mu_k_simple_linear = function(k, theta){

  return(min(1, k*theta[2]))

}

lambda_k_linear_immigration = function(k, theta){
  
  if(length(theta) < 3){stop("Length of theta implies no immigration, but birth function for immigration was used!")}
  
  return(min(1, k*theta[1] + theta[3]))
  
}

plotly_process = function(obj){
  
  fig = plot_ly(data = obj, x =~ cumsum(T.), y =~ state, type = "scatter", mode = "lines+markers", line = list(shape = "hv"))
  fig = fig |> layout(xaxis = list(title = "Time"),
                      yaxis = list(title = "Population Size"))
  
  return(fig)
  
}
