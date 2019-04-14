#' Calculate the a-priori power for the minimum desired effect and specified sample size
#'
#' @param N Total sample size.
#' @param min_de Minimum desire effect (as increase in response rate, e.g., 0.04).
#' @param repetitions Number of simulations.
#' @param n_conditions Number of conditions (default is 2: treatment and control).
#' @param n_strata Number of strata in each condition.
#' @param names_strata Character vector of strata names.
#' @param baselines Vector of baseline response rates (as proportions between 0 and 1). Same length as n_strata, one baseline for each stratum. 
#' @param baseline_factor Increase/decrease baselines (must be a number between 0 and 1).
#' @param one_side_test One-sided (default) or two-sided significance test for treament effect.
#' @return The estimated a-priori power to detect the desired effect.

power_by_sim_fun <- function(
      N = 2300,            
      min_de = 0.04,       
      repetitions = 1000,  
      n_conditions = 2,    
      n_strata = 5,        
      names_strata = c('jan', 'feb', 'mar', 'apr', 'may'),   
      baselines = c(0.17, 0.17, 0.17, 0.14, 0.29),           
      baseline_factor = 0, 
      one_side_test = TRUE
      ){
      
      n_groups <- N/(n_conditions*n_strata)
      
      # expected baseline response rates 
      baselines <- baselines + baseline_factor
      
      # variables:
      # stratification design
      strata_vars = rep(names_strata, n_conditions)
      # treatment
      cond_var = rep(n_conditions - seq_len(n_conditions), each=n_strata)
      # response rates for DV
      rates = c( baselines + min_de, baselines )
      # 
      cond_var    = rep(cond_var, times=n_groups)
      strata_vars = rep(strata_vars, times=n_groups)
      
      # matrix to collect regression test significance results
      significant = matrix(nrow=repetitions, ncol=2)
      
      startT = proc.time()[3]
      for(i in 1:repetitions){
            responses          = rbinom(n=N, size=1, prob=rates)
            model              = glm(responses~cond_var+strata_vars, family=binomial(link="logit"))
            
            # Wald's z test for treatment effect
            significant[i,1]   = (summary(model)$coefficients[2,4]/(as.numeric(one_side_test) + 1) <.05)
            
            # model all effects (not reporting this atm)
            modelDev           = model$null.deviance-model$deviance
            significant[i,2]   = (1-pchisq(modelDev, 5))<.05
      }
      endT = proc.time()[3]
      endT-startT
      
      # power for pre-specified treatment effect (cond_var)
      sum(significant[,1])/repetitions
      }


