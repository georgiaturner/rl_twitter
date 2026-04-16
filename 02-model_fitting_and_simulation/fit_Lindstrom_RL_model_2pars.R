### run lindstrom model on the AH confirmatory dataset, first the same and then with changes according to minor revision where starting policy must be changed

rm(list = ls())
seed <- 14
set.seed(seed)

library(tidyverse)

### function to fit the RL model
fit_basic_RL_model <- function(pars, outp, input_data) { 
  
  # Inputs from data
  time_diff <- (input_data[, 1])
  likes <- input_data[, 2]

  ## parameters
  a1 <- as.numeric(pars[1]) # Updating rate
  
  # pol_par_start <- as.numeric(pars[2]) # Parameter P
  #cost <- as.numeric(pars[3]) # Effort cost
  
  ######## new parts: ######## 
  pol_par_start <- mean(input_data[, 1]) # Parameter P
  cost <- as.numeric(pars[2])
  ######## ########   ######## 
  
  # define all latent/output variables
  mean_R <- numeric(length(time_diff) + 1)
  mean_R[1] <- input_data[1, 3]  # Start value for Rmean
  
  probdata <- numeric(length(time_diff))
  gradient <- numeric(length(time_diff))
  pol_par_all <- numeric(length(time_diff))
  sim_data <- numeric(length(time_diff))
  PE <- numeric(length(time_diff))
  
  pol_par <- pol_par_start # Current policy set to parameter P
  
  ## transform if bad input
  if (pol_par < 0) {
    pol_par <- .0001
  } 
  
  
  
  start <- 1 # starting
  for (t in start:length(time_diff)) { # Loop over all datapoints
    
    curr.choice <- time_diff[t] # Tpost
    probdata[t] <- dexp(curr.choice, 1 / (pol_par)) # Loglikelihood
    pol_par_all[t] <- 1 / pol_par # save policy
    
    if (outp != 1) { # If Maximum Likelihood is not returned, simulate from current policy
      sim_data[t] <- median(rexp(1, 1 / (pol_par)))
    }
    
    ## Reward
    R <- likes[t] # observed reward
    
    
    # Prediction errors and Delta TPost
    last.choice <- time_diff[t - 1]
    delta_R <- R - (cost / curr.choice) - mean_R[t] * curr.choice
    PE[t] <- delta_R
    
    # Set DeltaTpost = 0 for T = 1
    delta_time <- ifelse(t!=1,curr.choice - last.choice,0) 
    
    # Gradient
    gradient[t] <- delta_time * delta_R #
    
    # Reward rate update
    mean_R[t + 1] <- mean_R[t] + a1 * delta_R 
    
    # Policy updated
    pol_par <- pol_par + a1 * gradient[t]  -a1*mean_R[t+1]  
  }
  
  
  probdata[probdata == 0] <- .000001 # If Max Likelihood == 0.
  
  L <- -2 * sum(log(probdata[-1])) # Exclude first datapoint where Tpost is undefined (set to 0.004 in datasets for visability). Can be rewritten with lagged Likes.
  
  
  #### Soft constraints on parameters
  if (a1 > 1 || a1 < 0) {
    L <- 1000000
  }
  if (pol_par_start <= 0.001) {
    L <- 1000000
  }
  if (cost <= 0) {
    L <- 1000000
  }
  # Return Maximum Likelihood or internal variables (for generating predictioons)
  if (outp == 1) {
    return(L)
  } else {
    return(list(time_diff, sim_data, pol_par_all, mean_R, PE))
  }
}


#### By Björn Lindström 2019 ####
#### bjorn.r.lindstrom@gmail.com 
#### 
#### This script calls the fit_basic_RL_mod.R function to estimate the RL model.

data_path        <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
data_name_string <- "241104_241104_240416_AHconf_cleaned_preproc"
tpost_unit = "seconds";
fit_data          <- read_csv(paste(data_path,  data_name_string, ".csv", sep = ""));
# convert tpost to days as model fitting code assumes this unit with various constraints e.g. when rounding '0' to .0001
if (tpost_unit == "seconds") {
  fit_data$t_post <- fit_data$t_post/ (3600*24)
  tpost_unit <- "days"
}
fit_path <- str_c("./../../data_processed/Twitter/fit/",data_name_string)

# initialize R_mean
fit_data$Rmean_start <- median(fit_data$likes[fit_data$post_num_pic == 1]) / median(fit_data$t_post[fit_data$post_num_pic == 2], na.rm = T)
fit_data$Rmean_start[1]


# count users
inds <- row.names(table(fit_data$user_num))
length(inds)



#fit_data   <- filter(fit_data, user_num < unique(fit_data$user_num)[4])


#########
### initialise df for subs who fail to converge
error_subs_RLsingleLR <- c()  
max_tries  <- 1000; # maximum amount of fitting tries before considering subject a failed sub
# have tried with max_tries = 50 and this doesnt seem to be enough bc sometimes there are 50 fails and still some successes


fit_RL <- data.frame()
start <- 1

### Loop over all users
for (i in start:(length(inds))) {
  sub <- subset(fit_data, user_num == inds[i])
  sub <- as.data.frame(sub)
  sub <- subset(sub, !is.na(t_post)) # remove any NAs
  s_data <- data.frame(t_post = as.numeric(sub$t_post), likes = as.numeric(sub$likes), mean_likes = as.numeric(sub$Rmean_start)) # ,diff_days=as.numeric(dif_ind_d))
  rownames(s_data) <- NULL
  
  inits <- 50 # Number of random initializations
  rm(fit, fit.temp)
  
  for (ins in 1:inits) { # loops over the random initializations
    ### added to skip subjects if not converging ###
    n_tries  <- 0; #added by Georgia to see how many errors there are and store and remove if too many.
    skip_sub <- FALSE;
    ############
    FitFound <- F
    
    
    while (!FitFound) {
      #starting <- c(runif(1, 0.00000001, .0001), runif(1, mean(s_data$t_post) / 4, mean(s_data$t_post) * 4), runif(1, .1, 10)) # randomized start values for the three parameters.
      
      starting <- c(runif(1, 0.00000001, .0001),  runif(1, .1, 10)) # randomized start values for the 2 parameters.
      
      fit.temp <- try(optim(starting, fit_basic_RL_model, outp = 1, input_data = s_data, method = "Nelder-Mead", control = list(maxit = 500)), silent = T)
      
      ###########################
      # evaluate result...
      if (class(fit.temp) != "try-error") {
        if (!FitFound) {
          FitFound <- TRUE
        }
      } else {
        n_tries <- n_tries + 1;
      }
      if ((n_tries > max_tries) & ins ==1) {   ## if failed too many times with this subject, and it's the first init, just move on to the next subj, and store the info.
        # the ins == 1 is here to minimise the likelihood of skipping subjects which could have succeeded - if there has already
        # been a successful init (i.e. ins > 1) then don't skip them.
        skip_sub = TRUE;
        print(paste0("Skipping the ", as.character(i), "th subject (user_num = ", as.character(sub$user_num[1]), ")"))
        break
      }
      
    }
    
    if (skip_sub == TRUE) {
      break
    }
    

    if (ins == 1) {
      fit <- fit.temp
    } else {
      if (fit.temp$value < fit$value) {
        fit <- fit.temp
      }
    }
  }
  ### store info about skipped subject if they were skipped due to nonconvergence
  if (skip_sub == TRUE) {
    error_subs_RLsingleLR <- append(error_subs_RLsingleLR, sub$user_num[1])
    fit_RL <- rbind(fit_RL, data.frame(user_num = sub$user_num[1], LL = NA, AIC = NA, alfa1 = NA, policy = NA, cost = NA))

    next
  }
  
  
  # Calculate Akaike Information Criterion
  fit$AIC <- fit$value + 2 * length(fit$par) 
  
  # save output
  fit_RL <- rbind(fit_RL, data.frame(subject = inds[i], LL = fit$value, AIC = fit$AIC, alfa1 = fit$par[1], policy = mean(s_data[,1]), cost = fit$par[2]))
  # print
  print(i / length(inds)) #percentage completed 
  print(i)
  write_csv(fit_RL, str_c(fit_path, "/", format(Sys.time(), "%y%m%d"), "RL_Lindstrom_2pars_seed", seed, ".csv"))
  
}

##############
