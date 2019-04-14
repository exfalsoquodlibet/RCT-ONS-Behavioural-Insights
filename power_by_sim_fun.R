
power_by_sim_fun <- function(
      repetitions = 1000,  # number of simulations
      N = 2300,            # total sample size
      n_conditions = 2,    # number of conditions
      n_strata = 5,        # number of strata in each condition
      names_strata = c('jan', 'feb', 'mar', 'apr', 'may'),   # character vector with names of strata
      baselines = c(0.17, 0.17, 0.17, 0.14, 0.29),           # vector of baseline response rates, same length as n_strata, one baseline for each stratum 
      baseline_factor = 0, # increase/decrease baselines by these pecentage points
      min_de = 0.04,       # minimum desire effect
      one_side_test = TRUE # one-sided (default) or two-sided significance test for treament effect
      ){
      
      n_groups <- N/(n_conditions*n_strata)
      
      # expected baseline response rates (based on historical data)
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
      significant = matrix(nrow=repetitions, ncol=7)
      
      startT = proc.time()[3]
      for(i in 1:repetitions){
            responses          = rbinom(n=N, size=1, prob=rates)
            model              = glm(responses~cond_var+strata_vars, 
                  family=binomial(link="logit"))
            # one-sided (right-tail) Wald's z test for treatment
            significant[i,1]   = (summary(model)$coefficients[2,4]/(as.numeric(one_side_test) + 1) <.05)
            
            # model all effects (not reporting this atm)
            modelDev           = model$null.deviance-model$deviance
            significant[i,7]   = (1-pchisq(modelDev, 5))<.05
      }
      endT = proc.time()[3]
      endT-startT
      
      # pre-specified effect power for treatment effect (cond_var)
      sum(significant[,1])/repetitions
      }

