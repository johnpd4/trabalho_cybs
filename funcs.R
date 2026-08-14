# Process & Simulation
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
    
    if((rate_birth + rate_death) <= 0){
      path[nrow(path) + 1, ] = c(k, 0, 0, t_max - sum(path$T.))
      
      return(path)
    }
    
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


theta_update_simple_linear = function(k, theta) {
  
  lambda = theta[1]
  mu = theta[2]
  
  birth = lambda * k
  death = mu * k
  
  return(c(birth, death))
  
}

theta_update_immigration = function(k, theta) {
  
  lambda = theta[1]
  mu = theta[2]
  nu = theta[3]
  
  birth = (lambda * k) + nu
  death = mu * k
  
  return(c(birth, death))
  
}

theta_update_restricted_growth = function(k, theta) {
  
  lambda = theta[1]
  mu = theta[2]
  beta = theta[3]
  
  birth = lambda * (k^2) * exp(-beta * k)
  death = mu * k
  
  return(c(birth, death))
  
}

theta_update_sis = function(k, theta) {
  
  beta = theta[1]
  gamma = theta[2]
  N = theta[3]
  
  birth = (beta * k * (N - k)) / N
  death = (gamma * k) / N
  
  return(c(birth, death))
  
}

extreme_paths = function(a, t_max, U, D, lowest = T){
  
  steps = U + D
  
  state = rep(0, steps + 1)
  state[1] = a
  
  time_delta = t_max / steps
  time = rep(time_delta, steps + 1)
  time[1] = 0
  
  if(lowest){
    preffered_direction = -D
    other_direction = U
    U_new = c(0, rep(0, U), rep(1, D))
    D_new = c(0, rep(1, U), rep(0, D))
  }
  if(!lowest){
    preffered_direction = U
    other_direction = -D
    U_new = c(0, rep(1, U), rep(0, D))
    D_new = c(0, rep(0, U), rep(1, D))
  }
  
  for(i in 2:(steps+1)){
    
    # Testa se ele ainda pode ir na direcao preferida
    if(abs(preffered_direction) > 0){
      state[i] = state[i-1] + preffered_direction/abs(preffered_direction)
      preffered_direction = preffered_direction - sign(preffered_direction)
    # Se ele n pode ele vai na outra
    } else {
      state[i] = state[i-1] + other_direction/abs(other_direction)
      other_direction = other_direction - sign(other_direction)
    }
    
  }
  
  return(data.frame(state = state, 
                    U = U_new, D = D_new,
                    T. = time))
  
}

random_path = function(a, t_max, U, D){
  
  steps = U + D
  
  state = rep(0, steps + 1)
  state[1] = a
  
  U_new = rep(0, steps + 1)
  D_new = rep(0, steps + 1)
  
  time_delta = t_max / steps
  time = rep(time_delta, steps + 1)
  
  path = c(rep(1, U), rep(-1, D))
  path = sample(path, length(path))
  
  for(i in 2:(steps + 1)){
    
    state[i] = state[i-1] + path[i-1]
    if(sign(path[i-1]) < 0){D_new[i-1] = 1}
    if(sign(path[i-1]) > 0){U_new[i-1] = 1}
    
  }
  
  return(data.frame(state = state, 
                    U = U_new, D = D_new,
                    T. = time))
  
}

plotly_process = function(obj, old_fig = NULL, alpha = 1, name = NULL, color = NULL){
  
  if(is.null(old_fig)){
    if(is.null(name)){name = "P1"}
    fig = plot_ly(data = obj, x =~ cumsum(T.), y =~ state, name = name,
                  type = "scatter", mode = "lines+markers", line = list(shape = "hv"), opacity = alpha)
    fig = fig |> layout(xaxis = list(title = "Time"),
                        yaxis = list(title = "Population Size"))
  } else {
    trace_num = length(old_fig$x$visdat) + 1
    if(is.null(name)){name = paste0("P", trace_num)}
    if(is.null(color)){
      fig = old_fig |>
        add_trace(data = obj, x =~ cumsum(T.), y =~ state, name = name,
                  type = "scatter", mode = "lines+markers", line = list(shape = "hv"), opacity = alpha)
    } else {
      fig = old_fig |>
        add_trace(data = obj, x =~ cumsum(T.), y =~ state, name = name,
                  type = "scatter", mode = "lines+markers",
                  line = list(shape = "hv", color = color), marker = list(color = color), opacity = alpha)
    }
  }
  
  return(fig)
  
}

real_suff_stats = function(obj){
  
  return(obj |> dplyr::group_by(state) |> summarise(U = sum(U), D = sum(D), T. = sum(T.)))
  
}

dynamic_buttons = function(ids, labels, default_vals){
  
  x = list()
  
  for(i in 1:length(ids)){
    
    x[[i]] = sliderInput(ids[i],
                         labels[i],
                         min = 0,
                         value = default_vals[i],
                         max = 10,
                         step = 0.01)
    
  }
  
  return(div(x))
  
}

get_Y = function(obj){
  
  return(list(a = obj$state[1],
              b = obj$state[length(obj$state)],
              t = sum(obj$T.)))
  
}

# Estimation
a_j = function(j, theta_update, theta){
  
  if(j == 0){return(0)}
  if(j == 1){return(1)}
  
  gamma = theta_update(j - 2, theta) |> getElement(1)
  mu = theta_update(j - 1, theta) |> getElement(2)
  
  return(-gamma * mu)
  
}

b_j = function(s, j, theta_update, theta){
  
  if(j == 1){
    gamma0 = theta_update(0, theta) |> getElement(1)
    return(s + gamma0)
  }
  
  params = theta_update(j - 1, theta)
  gamma = params[1]
  mu = params[2]
  
  return(s + gamma + mu)
  
}

A_k = function(s, k, theta_update, theta){
  
  A_minus_2 = 0
  
  if(k == 0){
    return(A_minus_2)
  }
  
  A_minus_1 = a_j(1, theta_update, theta)
  
  if(k == 1){
    return(A_minus_1)
  }
  
  for(i in 2:k){
    
    A_k = b_j(s, i, theta_update, theta) * A_minus_1 +
      a_j(i, theta_update, theta) * A_minus_2
    
    A_minus_2 = A_minus_1
    A_minus_1 = A_k
    
  }
  
  return(A_k)
  
}

B_k = function(s, k, theta_update, theta){
  
  B_minus_2 = 1
  
  if(k == 0){
    return(B_minus_2)
  }
  
  B_minus_1 = b_j(s, 1, theta_update, theta)
  
  if(k == 1){
    return(B_minus_1)
  }
  
  for(i in 2:k){
    
    B_k = b_j(s, i, theta_update, theta) * B_minus_1 +
      a_j(i, theta_update, theta) * B_minus_2
    
    B_minus_2 = B_minus_1
    B_minus_1 = B_k
    
  }
  
  return(B_k)
  
}

B_list = function(s, K, theta_update, theta){
  
  B = rep(0, K + 1)
  
  B[1] = 1
  
  if(K == 0){
    return(B)
  }
  
  B[2] = b_j(s, 1, theta_update, theta)
  
  if(K == 1){
    return(B)
  }
  
  for(k in 2:K){  
    B[k + 1] =
      b_j(s, k, theta_update, theta) * B[k] +
      a_j(k, theta_update, theta) * B[k - 1]
  }
  
  return(B)
  
}

f_ij = function(i, j, s, theta_update, theta, M){
  
  # Caso especial f00
  if(i == 0 && j == 0){ 
    f00 = A_k(s, M, theta_update, theta) / B_k(s, M, theta_update, theta)
    return(f00)
  }
  
  if(M < 3){
    stop("M must be at least 3")
  }
  
  if(j <= i){
    
    if(j < i){
      mu_list = c()
      
      # for(k in (j + 1):i){
      #   
      #   mu_list[k] = theta_update(k, theta) |> getElement(2)
      #   
      # }
      
      mu_list = sapply((j + 1):i, function(k) {
        theta_update(k, theta)[2]
      })
      
      mu_prod = prod(mu_list, na.rm = T)
      
    } else {
      mu_prod = 1
    }
    
    fraction = NULL
    
    # Sdds C; my beloved
    for(k in seq(i + M, i + 3, by = -1)){
      
      if(is.null(fraction)){
        fraction = a_j(k, theta_update, theta) / b_j(s, k, theta_update, theta)
      } else {
        fraction = a_j(k, theta_update, theta) / (b_j(s, k, theta_update, theta) + fraction)
      }
      
    }
    
    BList = B_list(s, i + 1, theta_update, theta)
    
    # Step "2"
    fraction = (BList[i + 1] * a_j(i + 2, theta_update, theta)) / (b_j(s, i + 2, theta_update, theta) + fraction)
    
    # Step "1"
    fraction = BList[j + 1] / (BList[i + 2] + fraction)
    
    final_awnser = mu_prod * fraction
    
  } else if(i < j){
    
    gamma_list = c()
    
    # for(k in i:(j - 1)){
    #   
    #   gamma_list[k] = theta_update(k, theta) |> getElement(1)
    #   
    # }
    
    gamma_list = sapply(i:(j - 1), function(k) {
      theta_update(k, theta)[1]
    })
    
    gamma_prod = prod(gamma_list, na.rm = T)
    
    fraction = NULL
    
    # Sdds C; my beloved
    for(k in seq(j + M, j + 3, by = -1)){
      
      if(is.null(fraction)){
        fraction = a_j(k, theta_update, theta) / b_j(s, k, theta_update, theta)
      } else {
        fraction = a_j(k, theta_update, theta) / (b_j(s, k, theta_update, theta) + fraction)
      }
      
    }
    
    BList = B_list(s, j + 1, theta_update, theta)
    
    # Step "2"
    fraction = (BList[j + 1] * a_j(j + 2, theta_update, theta)) / (b_j(s, j + 2, theta_update, theta) + fraction)
    
    # Step "1"
    fraction = BList[i + 1] / (BList[j + 2] + fraction)
    
    final_awnser = gamma_prod * fraction
    
  }
  
  return(final_awnser)
  
}

laplace_inv = function(fun, t, M0, Mj, J, eps = 8){
  
  A = log(10^eps * (1 + (3/2 + J)) / t)
  
  s0 = A / (2 * t)
  
  first = (exp(A/2) / (2 * t)) * Re(fun(s0, M0))
  
  # j = 1,...,J terms
  sum_second = 0
  
  for(j in 1:J){
    
    sj = (A + 2 * pi * j * 1i) / (2 * t)
    
    sum_second = sum_second + (-1)^j * Re(fun(sj, Mj))
    
  }
  
  second = (exp(A/2) / t) * sum_second
  
  return(first + second)
  
}

E_U = function(a, b, t, 
               K, M0, Mj, J, theta_update, theta){
  
  func_numerator = function(s, M){
    sum = 0
    
    for(k in 0:K){
      
      # EUk = E_Uk_num(a = a, b = b, k = k, t = t,
      #                M0 = M0, Mj = Mj, J = J, theta_update = theta_update, theta = theta)
      
      lambda_k = theta_update(k, theta) |> getElement(1)
      
      if(identical(theta_update, theta_update_immigration)){
        
        lambda = theta[1]
        nu = theta[3]
        
        p_k = (k * lambda) / (k * lambda + nu)
        
        weight = p_k
        
      } else {
        weight = 1
      }
     
      sum = sum + weight * lambda_k *
            f_ij(a, k, s, theta_update, theta, M) * f_ij(k + 1, b, s, theta_update, theta, M)
       
    }
    
    return(sum)
    
  }
  
  numerator = laplace_inv(fun = func_numerator, t = t, M0 = M0, Mj = Mj, J = J)
  
  fun_denominator = function(s, M){
    f_ij(a, b, s, theta_update, theta, M)
  }
  
  denominator = laplace_inv(fun = fun_denominator, t = t, M0 = M0, Mj = Mj, J = J)
  
  return(numerator / denominator)
  
}

E_D = function(a, b, t, 
               K, M0, Mj, J, theta_update, theta){
  
  func_numerator = function(s, M){
    sum = 0
    
    for(k in 1:K){
      
      # EUk = E_Uk_num(a = a, b = b, k = k, t = t,
      #                M0 = M0, Mj = Mj, J = J, theta_update = theta_update, theta = theta)
      
      mu_k = theta_update(k, theta) |> getElement(2)
      
      sum = sum + mu_k *
        f_ij(a, k, s, theta_update, theta, M) * f_ij(k - 1, b, s, theta_update, theta, M)
      
    }
    
    return(sum)
    
  }
  
  numerator = laplace_inv(fun = func_numerator, t = t, M0 = M0, Mj = Mj, J = J)
  
  fun_denominator = function(s, M){
    f_ij(a, b, s, theta_update, theta, M)
  }
  
  denominator = laplace_inv(fun = fun_denominator, t = t, M0 = M0, Mj = Mj, J = J)
  
  return(numerator / denominator)
  
}

E_T = function(a, b, t, 
               K, M0, Mj, J, theta_update, theta,
               particle = TRUE){
  
  func_numerator = function(s, M){
    sum = 0
    
    for(k in 0:K){
      
      if(particle){
        sum = sum + k * f_ij(a, k, s, theta_update, theta, M) * f_ij(k, b, s, theta_update, theta, M)
      } else {
        sum = sum + f_ij(a, k, s, theta_update, theta, M) * f_ij(k, b, s, theta_update, theta, M)
      }
      
    }
    
    return(sum)
    
  }
  
  numerator = laplace_inv(fun = func_numerator, t = t, M0 = M0, Mj = Mj, J = J)
  
  fun_denominator = function(s, M){
    f_ij(a, b, s, theta_update, theta, M)
  }
  
  denominator = laplace_inv(fun = fun_denominator, t = t, M0 = M0, Mj = Mj, J = J)
  
  return(numerator / denominator)
  
}

E_UDT = function(a, b, t,
                 K, M0, Mj, J,
                 theta_update, theta){
  
  fun_denominador = function(s, M){
    f_ij(a, b, s, theta_update, theta, M)
  }
  
  fun_U = function(s, M){
    
    total = 0
    
    for(k in 0:K){
      
      lambda_k = theta_update(k, theta) |> getElement(1)
      
      total = total +
        lambda_k * f_ij(a, k, s, theta_update, theta, M) * f_ij(k + 1, b, s, theta_update, theta, M)
      
    }
    
   return(total)
    
  }
  
  fun_D = function(s, M){
    
    total = 0
    
    for(k in 1:K){
      
      mu_k = theta_update(k, theta) |> getElement(2)
      
      total = total +
        mu_k * f_ij(a, k, s, theta_update, theta, M) * f_ij(k - 1, b, s, theta_update, theta, M)
    }
    
    return(total)
    
  }
  
  fun_T = function(s, M){
    
    total = 0
    
    for(k in 0:K){
      
      total = total + 
        k * f_ij(a, k, s, theta_update, theta, M) * f_ij(k, b, s, theta_update, theta, M)
      
    }
    
    total
  }
  
  denominator = laplace_inv(fun_denominador, t, M0, Mj, J)
  
  EU = laplace_inv(fun_U, t, M0, Mj, J) / denominator
  ED = laplace_inv(fun_D, t, M0, Mj, J) / denominator
  ET = laplace_inv(fun_T, t, M0, Mj, J) / denominator
  
  return(c(U = EU, D = ED, T. = ET))
  
}

EM_linear = function(a, b, t,
                     theta_init,
                     K, M0, Mj, J,
                     tol = 1e-6,
                     max_iter = 100){
  
  theta = theta_init
  
  path = data.frame(
    iter = 0,
    lambda = theta[1],
    mu = theta[2]
  )
  
  for(iter in 1:max_iter){
    
    EU_total = 0
    ED_total = 0
    ET_total = 0
    
    # E-step
    for(i in seq_along(a)){
      
      E_udt = E_UDT(a = a[i], b = b[i], t = t[i],
                    K = K, M0 = M0, Mj = Mj, J = J,
                    theta_update = theta_update_simple_linear, theta = theta)
      
      EU_total = EU_total + E_udt["U"]
      ED_total = ED_total + E_udt["D"]
      ET_total = ET_total + E_udt["T."]
      
    }
    
    # M-step
    lambda_new = EU_total / ET_total
    mu_new = ED_total / ET_total
    
    theta_new = c(lambda_new, mu_new)
    
    
    cat("Iteracao ", iter, " | Lambda: ", lambda_new, " | Mu: ", mu_new, "\n",
      sep = "")
    
    
    path[nrow(path) + 1, ] = c(
      iter = iter,
      lambda = lambda_new,
      mu = mu_new
    )
    
    # Convergence
    if(max(abs(theta_new - theta)) < tol){
      theta = theta_new
      break
    }
    
    theta = theta_new
  }
  
  return(path)
  
}
