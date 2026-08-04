BDP_base = function(a, t_max, theta_update, theta){

  k = a
  tau = 0

  path = data.frame(state = numeric(),
                    U = numeric(),
                    D = numeric(),
                    T. = numeric())
  
  path[nrow(path)+1, ] = c(k, 0, 0, 0)

  while(sum(path$T.) <= t_max){

    rates = theta_update(k, theta)
    rate_birth = rates[1]
    rate_death = rates[2]
    
    tau = rexp(1, rate = (rate_birth + rate_death))
    
    if(sum(path$T.) + tau > t_max){
      path[nrow(path)+1, ] = c(k, 0, 0, t_max - sum(path$T.))
      return(path)
    }

    birth_range = rate_birth / (rate_birth + rate_death)
    # death_range = prob_birth + prob_death

    random = runif(1)

    if(random <= birth_range){
      k = k + 1
      path[nrow(path)+1, ] = c(k, 1, 0, tau)
    } else {
      k = k - 1
      path[nrow(path)+1, ] = c(k, 0, 1, tau)
    }

  }
  
  return(path)

}

BDP = function(a, t_max, method, theta){
  
  # METHODS:
  # linear -> theta = (\gamma, \mu)
  # immigration -> theta = (\gamma, \mu, \nu)
  # restricted_growth -> theta = (\gamma, \mu, \beta)
  # sis -> theta = (\beta, \gamma, N)
  
  func = switch(method,
    "linear" = theta_update_simple_linear,
    "immigration" = theta_update_immigration,
    "restricted_growth" = theta_update_restricted_growth,
    "sis" = theta_update_sis,
    stop("Unknown method: ", method)
  )
  
  obj = BDP_base(a = a, t_max = t_max,
                 theta_update = func,
                 theta = theta)
  
  return(obj)
  
}

a_j = function(j, theta_update, theta){
  
  if(j == 1){return(1)}
  
  gamma = theta_update_func(j - 2, theta) |> getElement(1)
  mu = theta_update_func(j - 1, theta) |> getElement(2)
  
  return(-gamma * mu)
  
}

b_j = function(s, j, theta_update, theta){
  
  if(j == 1){
    gamma0 = theta_update_func(0, theta) |> getElement(1)
    return(s + gamma0)
  }
  
  params = theta_update_func(j - 1, theta)
  gamma = params[1]
  mu = params[2]
  
  return(s + gamma + mu)
  
}

theta_update_simple_linear = function(k, theta){
  
  # if(length(theta) > 2){stop("Length of theta implies immigration, but birth function for simple linear was used!")}

  return(c(k*theta[1], k*theta[2]))

}

theta_update_immigration = function(k, theta){
  
  # if(length(theta) < 3){stop("Length of theta implies no immigration, but birth function for immigration was used!")}
  
  return(c(k*theta[1] + theta[3], k*theta[2]))
  
}

theta_update_restricted_growth = function(k, theta){
  
  # if(length(theta) < 3){stop("Length of theta implies no immigration, but birth function for immigration was used!")}
  
  return(c(theta[1] * k^2 * exp(-theta[3] * k), k*theta[2]))
  
}

theta_update_sis = function(k, theta){
  
  # if(length(theta) < 3){stop("Length of theta implies no immigration, but birth function for immigration was used!")}
  
  return(c(theta[1] * k * (theta[3] - k) / theta[3],
           k * theta[2] / theta[3]))
  
}

plotly_process = function(obj){
  
  fig = plot_ly(data = obj, x =~ cumsum(T.), y =~ state, type = "scatter", mode = "lines+markers", line = list(shape = "hv"))
  fig = fig |> layout(xaxis = list(title = "Time"),
                      yaxis = list(title = "Population Size"))
  
  return(fig)
  
}
