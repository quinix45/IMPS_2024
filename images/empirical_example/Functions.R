## BMA weights

dirichlet_rng <- function(n, alpha) {
  K <- length(alpha)
  gamma_sim <- matrix(rgamma(K * n, alpha), ncol = K, byrow = TRUE)
  gamma_sim / rowSums(gamma_sim)
}


bma_weight <- function (elpd_mat, boot_n = 2000, seed = NULL){
  
  set.seed(seed)
  K <- ncol(elpd_mat)
  N <- nrow(elpd_mat)
  BB_n <-  boot_n
  alpha <- 1
  
  temp <- matrix(NA, BB_n, K)
  
  BB_weighting <- dirichlet_rng(BB_n, rep(alpha, N))
  for (bb in 1:BB_n) {
    z_bb <- BB_weighting[bb, ] %*% as.matrix(elpd_mat) * N
    uwts <- exp(z_bb - max(z_bb))
    temp[bb, ] <- uwts / sum(uwts)
  }
  wts <- structure(
    colMeans(temp),
    names = paste0(colnames(elpd_mat)),
    class = "pseudobma_bb_weights"
  )
  
  return(wts)
}


# bma_weight(loo_reslist_elpd, boot_n = 4000, seed = 567)

# item averaging functions

# Stacking weights

stacking_weights <- function(lpd_point,
                             optim_method = "BFGS",
                             optim_control = list()) {
    
    stopifnot(is.matrix(lpd_point))
    N <- nrow(lpd_point)
    K <- ncol(lpd_point)
    if (K < 2) {
      stop("At least two models are required for stacking weights.")
    }
    
    exp_lpd_point <- exp(lpd_point)
    negative_log_score_loo <- function(w) {
      # objective function: log score
      stopifnot(length(w) == K - 1)
      w_full <- c(w, 1 - sum(w))
      sum <- 0
      for (i in 1:N) {
        sum <- sum + log(exp(lpd_point[i, ]) %*% w_full)
      }
      return(-as.numeric(sum))
    }
    
    gradient <- function(w) {
      # gradient of the objective function
      stopifnot(length(w) == K - 1)
      w_full <- c(w, 1 - sum(w))
      grad <- rep(0, K - 1)
      for (k in 1:(K - 1)) {
        for (i in 1:N) {
          grad[k] <- grad[k] +
            (exp_lpd_point[i, k] - exp_lpd_point[i, K]) / (exp_lpd_point[i,]  %*% w_full)
        }
      }
      return(-grad)
    }
    
    ui <- rbind(rep(-1, K - 1), diag(K - 1))  # K-1 simplex constraint matrix
    ci <- c(-1, rep(0, K - 1))
    w <- constrOptim(
      theta = rep(1 / K, K - 1),
      f = negative_log_score_loo,
      grad = gradient,
      ui = ui,
      ci = ci,
      method = optim_method,
      control = optim_control
    )$par
    
    wts <- structure(
      c(w, 1 - sum(w)),
      names = paste0("model", 1:K),
      class = c("stacking_weights")
    )
    
    return(wts)
  }



sim_quantile_prob <- function(theta, 
                              pars,
                              quantiles = 0.1,
                              condition = "control")
{
  
  # Define number of items and participants
  
  n_person <- length(theta)
  n_items <- nrow(pars)
  
  # Define quantiles
  
  quants <- c(n_person*seq(0, 1, by = quantiles))
  quants[1] <- 1
  
  # create theta quantiles and repeat each row per each item to vectorize probability calculation
  
  theta_quants <- sort(theta)[quants]
  
  quant_rep <- data.frame(t(theta_quants)) %>% 
    dplyr::slice(rep(1:n(), each = n_items))
  
  if(condition == "control")
  {
    
    irf_2PL <- function(theta = 0, a = 1, b = 0) 
    {
      exp(a * (theta - b))/(1 + exp(a * (theta - b)))
    }
    
    
    return(irf_2PL(quant_rep,
                   pars[,1],
                   pars[,2]))
  }
  else if(condition == "stukel")
  {
    
    
    irf_stukel <- function (theta, a = 1, b = 0, alpha1 = 0.00001, alpha2 = 0.00001) 
    {
      
      # Using the fact that TRUE and FALSE are 1 and 0 respectively to vectorize pice-wise function   
      
      h <- ((theta - b) > 0)*
        (((alpha1 > 0) * alpha1^(-1) * (exp(alpha1 * a * (theta - b)) - 1)) +
           ((alpha1 == 0) * a * (theta - b)) + 
           (alpha1 < 0) * -alpha1^-1 * log(abs(1 - alpha1 * a * (theta - b)))) +
        
        ((theta - b) <= 0)*
        (((alpha2 > 0) * -alpha2^(-1) * (exp(alpha2 * a * ((b - theta))) - 1)) +
           ((alpha2 == 0) * a * (theta - b)) + 
           (alpha2 < 0) * alpha2^(-1) * log(abs(1 - alpha2 * a * ((b - theta)))))
      
      # Make sure that h does not become too large for R to handle
      
      ifelse(h > 601.7777, 601.7777, h)
      ifelse(h < -601.7777, -601.7777, h)
      
      return(exp(h)/(1 + exp(h)))
    }
    
    res <- irf_stukel(quant_rep,
                      rep(pars[,1], length(theta_quants)),
                      rep(pars[,2], length(theta_quants)),
                      rep(pars[,3], length(theta_quants)),
                      rep(pars[,4], length(theta_quants)))
    
    colnames(res) <- round(theta_quants, 3)
    
    return(data.frame(res))
    
  }
  
}

Quantile_prob <- function(parameter_draws, 
                          n_items,
                          n_person,
                          quantiles = 0.1, 
                          method = "empirical",
                          n_draws = 6000,
                          irf_function = "4PL")
{
  
  # Define the number of MCMC draws
  n_draws <- nrow(parameter_draws)
  
  quants <- c(n_person*seq(0, 1, by = quantiles))
  quants[1] <- 1
  
  if(method == "empirical"){
    
    # create theta quantiles and repeat each row per each item to vectorize probability calculation
    
    draws_theta <- apply(t(parameter_draws[, c(grepl("r_id", colnames(parameter_draws)))]), 
                         2, 
                         sort, 
                         decreasing=FALSE)
    
    quant <- t(draws_theta[quants,])
    
    # dimension of quantile matrix is a [n_items*n_draws, n_quantiles]            
    quant_rep <- data.frame(quant) %>% 
      dplyr::slice(rep(1:n(), each = n_items))
  } else if (method == "theoretical"){
    
    theoretical_quantiles <- data.frame(t(qnorm(c(.01, seq(.1, .9, by = .1), .99))))    
    
    quant_rep <- theoretical_quantiles %>% 
      dplyr::slice(rep(1:n(), each = n_items*n_draws))
    
  }
  
  # sort item parameters correctly
  unsorted_draws_it <- parameter_draws[, c(grepl("r_item", colnames(parameter_draws)))]
  draws_it <- unsorted_draws_it[,gtools::mixedsort(colnames(unsorted_draws_it))]
  
  # Get pooled intercepts of item parameters
  unsorted_post_intercepts <- parameter_draws[, c(grepl("b_", substr(colnames(parameter_draws), 1, 2)))]
  post_intercepts <- unsorted_post_intercepts[, gtools::mixedsort(colnames(unsorted_post_intercepts))]
  
  
  if(irf_function == "4PL")
  {
    
    irf_4PL <- function(theta = 0, a = 1, b = 0, c = 0, d = 1) 
    {
      (c + (d - c) * (exp(a * (theta - b))/(1 + exp(a * (theta - b)))))
    }
    
    if(ncol(draws_it) == n_items)
    {
      ####### 1 PL #######
      a <- rep(exp(post_intercepts[,1]), each = n_items)
      b <- c(unlist(t(draws_it + post_intercepts[,2])))
      c <- rep(0, n_draws*n_items)
      d <- rep(1, n_draws*n_items)
      
      return(irf_4PL(quant_rep, a, b, c, d))
    }
    else if(ncol(draws_it) == 2*n_items)
    {
      ####### 2 PL #######
      a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
      b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
      c <- rep(0, n_draws*n_items)
      d <- rep(1, n_draws*n_items)
      
      return(irf_4PL(quant_rep, a, b, c, d))
    }
    else if(ncol(draws_it) == 3*n_items)
    {
      ####### 3 PL #######
      a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
      b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
      c <- brms::inv_logit_scaled(c(unlist(t(draws_it[,(1 + n_items*2):(n_items*3)] + post_intercepts[,3]))))
      d <- rep(1, n_draws*n_items)
      
      return(irf_4PL(quant_rep, a, b, c, d))
    }
    else
    {
      ####### 4 PL #######
      a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
      b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
      c <- brms::inv_logit_scaled(c(unlist(t(draws_it[,(1 + n_items*2):(n_items*3)] + post_intercepts[,3]))))
      d <- brms::inv_logit_scaled(c(unlist(t(draws_it[,(1 + n_items*3):(n_items*4)] + post_intercepts[,4]))))
      
      return(irf_4PL(quant_rep, a, b, c, d))
    }
    
  }
  else if(irf_function == "2CLL")
  {
    
    irf_2CLL <- function(theta = 0, a = 1, b = 0) 
    {
      1 - exp( -exp( a * (theta - b)))
    }
    
    if(ncol(draws_it) == n_items)
    {
      ####### 1 CLL #######
      a <- rep(exp(post_intercepts[,1]), each = n_items)
      b <- c(unlist(t(draws_it + post_intercepts[,2]))) 
      
      
      return(irf_2CLL(quant_rep, a, b))
    }
    else if(ncol(draws_it) == 2*n_items)
    {
      ####### 2 CLL ####### 
      a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
      b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
      
      return(irf_2CLL(quant_rep, a, b))
    }
  }
  else if(irf_function == "2NLL"){
    
    irf_2NLL <- function (theta = 0, a = 1, b = 0)
    {
      exp( -exp( -a * (theta - b)))
    }
    
    if(ncol(draws_it) == n_items)
    {
      ####### 1 NLL #######
      a <- rep(exp(post_intercepts[,1]), each = n_items)
      b <- c(unlist(t(draws_it + post_intercepts[,2]))) 
      
      return(irf_2NLL(quant_rep, a, b))
    }
    else if(ncol(draws_it) == 2*n_items)
    {
      ####### 2 NLL ####### 
      a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
      b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
      
      return(irf_2NLL(quant_rep, a, b))
    }
    
  }
  else if(irf_function == "LPE"){
    
    irf_LPE <- function(theta, a, b, acc)
    {
      (exp(a * (theta - b))/(1 + exp(a * (theta - b))))^acc
    }
    
    a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
    b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
    acc <- exp(c(unlist(t(draws_it[,(1 + n_items*2):(n_items*3)] + post_intercepts[,3]))))
    
    return(irf_LPE(quant_rep, a, b, acc))
  }
  
}



Quantile_item_sample <- function(quant_list,
                                 n_items,
                                 n_draws = 6000,
                                 weights) {
  
  item_list <- list()
  
  for(i in 1:n_items)
  {
    sampler <- function(df,ws)
    {
      # select 1 item at a time
      reduced_df <- df[seq(i, nrow(df)-(20-i), by = n_items),]
      # sample from item draw of specific model base on corresponding weight
      reduced_df[floor(runif((n_draws*ws),1,n_draws)),]
    }
    
    # NOTE: models and weights need to be entered in the same order 
    # and have the same length (i.e, as many weights as models) 
    
    
    
    item_list[[i]] <- purrr::list_rbind(mapply(sampler, 
                                               quant_list, 
                                               ws = weights[i,],
                                               SIMPLIFY = FALSE))
  }
  return(item_list)  
}


Quantile_sample <- function(quant_list,
                            n_items,
                            n_draws = 6000,
                            weights)
{
   
   item_list <- list()
   
   for(i in 1:n_items)
   {
      sampler <- function(df,ws)
      {
         # select 1 item at a time
         reduced_df <- df[seq(i, nrow(df)-(20-i), by = n_items),]
         # sample from item draw of specific model base on corresponding weight
         reduced_df[floor(runif((n_draws*ws),1,n_draws)),]
      }
      
      # NOTE: models and weights need to be entered in the same order 
      # and have the same length (i.e, as many weights as models) 
      
      item_list[[i]] <- purrr::list_rbind(mapply(sampler, 
                                                 quant_list, 
                                                 ws = weights,
                                                 SIMPLIFY = FALSE))
   }
   return(item_list)  
}



