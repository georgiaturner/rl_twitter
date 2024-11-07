

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#-------------------------------------           02-FIT_MODELS.R          --------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# This script loads in the model functions and data to fit the models to, and 
# fits the models to the data. 
# The data that is loaded can be empirical or simulated.
# For a the model functions, refer to the script "01-model_functions.R".

# This script currently saves each model data after that model so that if the script crashes at least part
# of it can be saved.

# It takes in the specific model it is fitting as a parameter 'which_mod' using commandArgs, so that it can parallelise the model computation in the cluster.
# but if you want to run it you can just rest 'which_mod' within the script to the specific model you want to run.

# By Georgia Turner, 2024 < georgianjt@gmail.com >

################################################################################

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------                  SET UP         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


rm(list = ls())
seed = 1
set.seed(seed)


library(tidyverse)
library(lubridate)

using_cluster <- F

if (!using_cluster) {
  library(here) 
  setwd(here("02-model_fitting_and_simulation"))
  which_mod <- "RL1" # FP, CP, PH, RL1, RL2, RLH1, RLH2 - choose which model you want to do
  
} else {
  #### get model argument from commandline (these are looped through in the shell script, which should be saved in this directory)
  # Access the commandline arguments
  args <- commandArgs(trailingOnly = TRUE)
  print(str_c("args, ", args))
  # extract the model argument. if you want to use a specific model instead you can just reset which_mod to the model you want to fit.
  which_mod <- args[1];
  
}

source("01-model_functions.R"); # load in model functions.



#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------               LOAD DATA         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


######### ######### ######### ######### ######### ######### ######### ######### 
## specify settings
data_type     <- "AH_conf" # AH_conf or AH_disc or simgen_FP or simgen_CP etc
reward        <- "just_Likes" # just_Likes or just_RTs or LikesPlusRTs
shorten_posts <- F # if limiting the number of posts to be fitted, e.g. in simulations where only want to fit to a certain number of posts
if (shorten_posts) {
  nposts      <- 80 # number of posts to do, this is only used if shorten_posts == T
} else {
  nposts      <- "unshortened"
}
shorten_subs  <- F # if limiting the number of subjects to fit, e.g. to debug
if (shorten_subs) {
  nsubs       <- 10 # number of subjects to do, this is only used if shorten_subs == T
}
######### ######### ######### ######### ######### ######### ######### ######### 


if (data_type == "AH_disc") {
  
  data_path        <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
  data_name_string <- "241104_241104_240228_AHdisc_cleaned_preproc"
  tpost_unit = "seconds";

} else if (data_type == "AH_conf") {
  
  data_path        <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
  data_name_string <- "241104_241104_240416_AHconf_cleaned_preproc"
  tpost_unit = "seconds";

} else if (data_type == "simgen_FP") {
  
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2020FP"
  tpost_unit = "days";
  
} else if (data_type == "simgen_CP") {
  
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2023CP"
  tpost_unit = "days";

} else if (data_type == "simgen_PH") {
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2031PH"
  tpost_unit = "days";
  
} else if (data_type == "simgen_RL1") {
  
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2025RL1"
  tpost_unit = "days";
  
} else if (data_type == "simgen_RL2") {
  
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2027RL2"
  tpost_unit = "days";

} else if (data_type == "simgen_RLH1") {
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2037RLH1"
  tpost_unit = "days";

} else if (data_type == "simgen_RLH2") {
  
  data_path <- "./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/"
  data_name_string <- "241104-2043RLH2"
  tpost_unit = "days";

}

## create folders for saving if don't already exist
fit_path <- str_c("./../../data_processed/Twitter/fit/",data_name_string)
if (!file.exists(fit_path)) {
  dir.create(fit_path, recursive = TRUE)
}
sim1step_path <- str_c("./../../data_processed/Twitter/sim1step/",data_name_string)
if (!file.exists(sim1step_path)) {
  dir.create(sim1step_path, recursive = TRUE)
}

### load data
fit_dat          <- read_csv(paste(data_path,  data_name_string, ".csv", sep = ""));
# convert tpost to days as model fitting code assumes this unit with various constraints e.g. when rounding '0' to .0001
if (tpost_unit == "seconds") {
  fit_dat$t_post <- fit_dat$t_post/ (3600*24)
  tpost_unit <- "days"
}

##################################################################################################################
### Apply settings for this round of fitting
##################################################################################################################


if (reward == "just_RTs") {
  fit_dat$likes <- fit_dat$retweet_count
} else if (reward == "LikesPlusRTs") {
  fit_dat$likes <- fit_dat$likes + fit_dat$retweet_count
  
}

if (shorten_posts) {
  # shorten to relevant number of posts
  fit_dat <- fit_dat %>% filter(post_num_pic < (nposts + 1))
} 


if (shorten_subs) {
  fit_dat   <- filter(fit_dat, user_num < unique(fit_dat$user_num)[nsubs + 1])
}

dataset_description <- str_c(data_name_string, "_", nposts, "posts_reward", reward, "_seed", seed)


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------         FIT MODELS TO DATA      ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

#### decide which model to do based on which_mod

if (which_mod == "FP") {
  
  #####################################################################################
  ##### FIXED POLICY MODEL (FP)
  #####################################################################################
  
  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  #########
  
  # initialise dataframe of fit parameters for each user
  fitdat_FP = data.frame()
  
  for (i in 1:(length(inds))) { # loopover users
    
    print(str_c("Currently fitting ", which_mod, " to user ", i, " out of ", length(inds)))
    
    # select that user's data from df of all users
    sub    <- subset(fit_dat, user_num == inds[i])
    sub    = as.data.frame(sub)
    sub    = subset(sub, !is.na(t_post))
    s_data = data.frame(t_post = as.numeric(sub$t_post), 
                        likes = as.numeric(sub$likes))
    rownames(s_data) <- NULL
    
    # run the model 
    inits <- 50 # define how many initialisations for optim function
    
    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }
    
    for (ins in 1:inits){
      print(str_c("optim initialisation ", as.character(ins)))
      ############
      starting      <- c(runif(1, mean(s_data$t_post) / 4, mean(s_data$t_post) * 4)) # random starting point for optimisation procedure
      try(fit.temp  <- optim(starting,
                             mod_FP,
                             outp       ="fit",
                             input_dat  = s_data,
                             method     ="Brent", lower=0, upper=1000, control = list(maxit=500)))
      
      # store new fitted value if it is better than the last one
      if (ins==1) { 
        fit <- fit.temp
      } else {if (fit.temp$value < fit$value) {
        fit <- fit.temp
      }
      }
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC = fit$value + 2*length(fit$par)
    
    # Save output
    fitdat_FP = rbind(fitdat_FP, 
                      data.frame(user_num = sub$user_num[1],
                                 LL       = fit$value,
                                 AIC      = fit$AIC,
                                 policy   = fit$par[1]))
    
    
    print(i/length(inds))
    print(i)
  }
  
  #### save
  fitdat_FP$dataset_description    <- dataset_description # record the preprocessing rule
  write_csv(fitdat_FP, str_c(fit_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))
    # note that we always save time to the nearest second with the filename so it doesn't overwrite previous versions

} else if (which_mod == "CP") {
  
  #####################################################################################
  ##### CHANGING POLICY MODEL  (CP)
  #####################################################################################
  
  ### initialise df for subs who fail to converge, to store who they were for later inspection
  error_subs_CP <- c()  
  max_tries     <- 10000; # maximum amount of fitting tries before considering subject a failed sub
  
  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  #########
  
  fitdat_CP = data.frame()
  
  for (i in 1:(length(inds))){ # loop over users
    
    print(str_c("Currently fitting ", which_mod, " to user ", i, " out of ", length(inds)))
    
    # get subset of that subject's data
    sub    <- subset(fit_dat,user_num == inds[i])
    sub    = as.data.frame(sub)
    sub    = subset(sub, !is.na(t_post))
    s_data = data.frame(t_post = as.numeric(sub$t_post), 
                        likes = as.numeric(sub$likes))
    rownames(s_data) <- NULL
    
    inits <- 50

    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }
    
    
    for (ins in 1:inits){
      print(str_c("optim initialisation ", as.character(ins)))
      
      ### skip subjects if not converging ###
      n_tries  <- 0; # added to see how many errors there are and store and remove if too many.
      skip_sub <- F
      ############
      
      FitFound <- F

      while (!FitFound) {
        
        starting=c(runif(1, max(s_data$t_post) / 2, max(s_data$t_post) * 4), 
                   # initialised value for parameter 'a' of mitscherlich function. 
                   runif(1, 0.1,(max(s_data$t_post) - min(s_data$t_post))),
                   # initialised value for parameter 'b' of mitscherlich function.
                   runif(1, 0.001, 0.01)
                   # initialised value for parameter 'c' of mitscherlich function - 
                   # chosen by trial and error
                   ) # random starting points
        
        fit.temp <- try(optim(starting, 
                              mod_CP, 
                              outp       = "fit", 
                              input_dat  = s_data, 
                              method     = "Nelder-Mead", control = list(maxit = 500)), silent = T)
        
        ###########################
        # evaluate result...
        print(class(fit.temp))
        
        if (class(fit.temp) != "try-error") {
          if (!FitFound) {
            FitFound <- TRUE
          } 
          
        } else {
          n_tries <- n_tries + 1; # if failed to converge, record it so it doesn't go on forever
        }
        if ((n_tries > max_tries) & ins == 1) {   ## if failed too many times with this subject, and it's the first init, just move on to the next subj, and store the info.
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
      
      # store new fitted parameters if they were better than the last one (i.e. lower log likelihood)
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
      error_subs_CP <- append(error_subs_CP, sub$user_num[1])
      fitdat_CP     <- rbind(fitdat_CP, data.frame(user_num = sub$user_num[1], LL = NA, AIC = NA, a = NA, b = NA, c = NA))
      next
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC = fit$value + 2*length(fit$par)
    
    # Save output
    fitdat_CP = rbind(fitdat_CP, 
                      data.frame(user_num = sub$user_num[1],
                                 LL       = fit$value,
                                 AIC      = fit$AIC,
                                 a        = fit$par[1], 
                                 b        = fit$par[2], 
                                 c        = fit$par[3]))
    
    print(i/length(inds))
    print(i)
  }

  #### save
  fitdat_CP$dataset_description    <- dataset_description # record the preprocessing rule
  write_csv(fitdat_CP, str_c(fit_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))
  # note that we always save time with the filename so it doesn't overwrite previous versions
  
} else if (which_mod == "RL1") {
  
  #####################################################################################
  #####  RL SINGLE LEARNING RATE (RL1)
  #####################################################################################
  
  ### initialise df for subs who fail to converge
  error_subs_RL1 <- c()  
  max_tries  <- 10000; # maximum amount of fitting tries before considering subject a failed sub

  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  #########
  
  fitdat_RL1 <- data.frame()
  # initialise df to simulate data as well so we can extract the subjective policy 
  # (we didn't need to do this with FP and CP models, because you can 
  # just easily derive the policy from the fit parameters in those cases)
  simdat_RL1 <- data.frame()
  
  start <- 1
  ### Loop over all users
  for (i in start:(length(inds))) {
    
    print(str_c("Currently fitting ", which_mod, " to user ", i, " out of ", length(inds)))
    
    # select that subject's data
    sub    <- subset(fit_dat, user_num == inds[i])
    sub    <- as.data.frame(sub)
    sub    <- subset(sub, !is.na(t_post)) # remove any NAs
    s_data <- data.frame(t_post     = as.numeric(sub$t_post), 
                         likes      = as.numeric(sub$likes) 
                         ) 
    rownames(s_data) <- NULL
    
    inits <- 50 # Number of random initializations

    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }
    
    
    for (ins in 1:inits) { # loops over the random initializations
      print(str_c("optim initialisation ", as.character(ins)))
      
      ### added to skip subjects if not converging ###
      n_tries  <- 0; #added to see how many errors there are and store and remove if too many.
      skip_sub <- F;
      
      ############
      FitFound <- F
      
      while (!FitFound) {
        
        # randomized start values for the parameters:
        starting <- c(runif(1, 0, 1),   # alpha
                      runif(1, .1, 10)  # cost
                      )
        
        # run fit:
        fit.temp <- try(optim(starting, 
                              mod_RL, 
                              LR_rule    = 1,  # 1 learning rate
                              outp       = "fit", 
                              input_dat  = s_data, 
                              method     = "Nelder-Mead", control = list(maxit = 500)), silent = T)
        ###########################
        # evaluate result...
        print(class(fit.temp))
        
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
      
      # store new fitted parameters if better than the last ones (i.e. lower log likelihood)
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
      error_subs_RL1 <- append(error_subs_RL1, sub$user_num[1])
      fitdat_RL1     <- rbind(fitdat_RL1, data.frame(user_num  = sub$user_num[1], 
                                                                    LL        = NA, 
                                                                    AIC       = NA, 
                                                                    alpha     = NA, 
                                                                    cost      = NA))
      
      next
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC <- fit$value + 2 * length(fit$par) 
    
    # save output
    fitdat_RL1 <- rbind(fitdat_RL1, 
                        data.frame(user_num = sub$user_num[1], 
                                   LL       = fit$value, 
                                   AIC      = fit$AIC, 
                                   alpha    = fit$par[1], 
                                   cost     = fit$par[2]))
    
    # Now simulate the model again with the best fitting parameters, in order to extract the policy (pol_par_all) and subjective reward rate (R_est) for plotting
    ################################################################################
    ## SIMULATE RL1  
    ################################################################################
    
    # Get best fitting parameters for that subject.
    fit_params_RL1   <- unlist(dplyr::select(filter(fitdat_RL1, 
                                                    user_num == sub$user_num[1]),
                                                    alpha, cost));
    simsub_RL1       <- mod_RL(pars      = fit_params_RL1, 
                               outp      = "sim1step", 
                               LR_rule   = 1, 
                               input_dat = s_data);
    
    
    # Save output
    simdat_RL1 <- rbind(simdat_RL1, 
                        data.frame(user_num    = sub$user_num[1], 
                                   t_post      = c(s_data$t_post, NA),
                                   pol_par_all = simsub_RL1$pol_par_all,
                                   simdat      = simsub_RL1$simdat, # simdat is simulated from the policy; different from t_post which is the data the model learned from - see model equations in model_functions for explanation of this difference
                                   likes       = simsub_RL1$likes,
                                   R_est       = simsub_RL1$R_est,
                                   RPE         = simsub_RL1$RPE, 
                                   AIC         = fitdat_RL1[i, "AIC"], 
                                   alpha       = fitdat_RL1[i, "alpha"], 
                                   cost        = fitdat_RL1[i, "cost"]))
    ################################################################################
    
    # print
    print(i / length(inds)) #percentage completed 
    print(i)
  }
  
  #### save
  fitdat_RL1$dataset_description     <- dataset_description
  simdat_RL1$dataset_description     <- dataset_description
  write_csv(fitdat_RL1, str_c(fit_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))
  write_csv(simdat_RL1, str_c(sim1step_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv")) 
  
} else if (which_mod == "RL2")  {
  
  #####################################################################################
  ##### RL DOUBLE LEARNING RATE (RL2)
  #####################################################################################
  
  ### initialise df for subs who fail to converge
  error_subs_RL2 <- c()  
  max_tries  <- 10000; # maximum amount of fitting tries before considering subject a failed sub

  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  
  #########
  
  fitdat_RL2 <- data.frame()
  simdat_RL2 <- data.frame()
  start <- 1
  
  ### Loop over all users
  for (i in start:(length(inds))) {
    
    print(str_c("Currently fitting ",which_mod, " to user ", i, " out of ", length(inds)))
    
    # get data for this subject
    sub <- subset(fit_dat, user_num == inds[i])
    sub <- as.data.frame(sub)
    sub <- subset(sub, !is.na(t_post)) # remove any NAs
    s_data <- data.frame(t_post    = as.numeric(sub$t_post), 
                         likes     = as.numeric(sub$likes))
    rownames(s_data) <- NULL
    
    inits <- 50 # Number of random initializations
    
    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }
    
    for (ins in 1:inits) { # loops over the random initializations
      print(str_c("optim initialisation ", as.character(ins)))

      ### skip subjects if not converging ###
      n_tries  <- 0; #  to see how many errors there are and store and remove if too many.
      skip_sub <- F;
      ############
      FitFound <- F
      
      while (!FitFound) {
        # randomised start values for parameters:
        starting <- c(runif(1, 0, 1), # alpha_P
                      runif(1, 0, 1), # alpha_N
                      runif(1, .1, 10)) # cost

        # run fit:
        fit.temp <- try(optim(starting, mod_RL, 
                              LR_rule    = 2, # double learning rate
                              outp       = "fit", 
                              input_dat  = s_data, 
                              method     = "Nelder-Mead", control = list(maxit = 500)), silent = T)
        
        ###########################
        # evaluate result...
        print(class(fit.temp))
        
        if (class(fit.temp) != "try-error") {
          if (!FitFound) {
            FitFound <- TRUE
          } 
          
        } else {
          n_tries <- n_tries + 1;
        }
        if ((n_tries > max_tries) & ins == 1) {   ## if failed too many times with this subject, and it's the first init, just move on to the next subj, and store the info.
          # the ins == 1 is here to minimise the likelihood of skipping subjets which could have succeeded - if there has already
          # been a successful init (i.e. ins > 1) then don't skip them.
          skip_sub = TRUE;
          print(paste0("Skipping the ", as.character(i), "th subject (user_num = ", as.character(sub$user_num[1]), ")"))
          break
        }
        
      }
      
      if (skip_sub == TRUE) {
        break
      }
      
      # store new parameter values if better than the last (i.e. if log likelihood is lower)
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
      error_subs_RL2 <- append(error_subs_RL2, sub$user_num[1])
      fitdat_RL2     <- rbind(fitdat_RL2, data.frame(user_num = sub$user_num[1],  
                                                    LL        = NA, 
                                                    AIC       = NA, 
                                                    alpha_P   = NA, 
                                                    alpha_N   = NA,  
                                                    cost      = NA))
      
      next
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC <- fit$value + 2 * length(fit$par) 
    
    # save output
    fitdat_RL2 <- rbind(fitdat_RL2, 
                        data.frame(user_num = sub$user_num[1], 
                                   LL       = fit$value, 
                                   AIC      = fit$AIC, 
                                   alpha_P  = fit$par[1], 
                                   alpha_N  = fit$par[2], 
                                   cost     = fit$par[3]))
    
    # Now simulate the model again with the best fitting parameters, in order to extract the policy (pol_par_all) and subjective reward rate (R_est) for plotting
    ################################################################################
    ## SIMULATE RL2
    ################################################################################
    
    # Get best fitting parameters for that subject.
    fit_params_RL2   <- unlist(dplyr::select(filter(fitdat_RL2, user_num == sub$user_num[1]), alpha_P, alpha_N, cost));
    simsub_RL2       <- mod_RL(pars = fit_params_RL2, 
                               outp       = "sim1step", 
                               LR_rule    = 2, 
                               input_dat = s_data);
    
    
    # Save output
    simdat_RL2 <- rbind(simdat_RL2,
                        data.frame(user_num    = sub$user_num[1], 
                                   t_post      = c(s_data$t_post, NA),
                                   pol_par_all = simsub_RL2$pol_par_all,   
                                   simdat      = simsub_RL2$simdat, 
                                   likes       = simsub_RL2$likes,
                                   R_est       = simsub_RL2$R_est, 
                                   RPE         = simsub_RL2$RPE, 
                                   AIC         = fitdat_RL2[i, "AIC"], 
                                   alpha_P     = fitdat_RL2[i, "alpha_P"], 
                                   alpha_N     = fitdat_RL2[i, "alpha_N"],
                                   cost        = fitdat_RL2[i, "cost"]))
    ################################################################################
    
    # print
    print(i / length(inds)) #percentage completed 
    print(i)
  }
  
  ##############
  #### save
  fitdat_RL2$dataset_description  <- dataset_description
  simdat_RL2$dataset_description  <- dataset_description
  write_csv(fitdat_RL2, str_c(fit_path, "/", format(Sys.time(),  "%y%m%d-%H%M"), which_mod, seed, ".csv"))
  write_csv(simdat_RL2,  str_c(sim1step_path, "/", format(Sys.time(),  "%y%m%d-%H%M"), which_mod, seed, ".csv"))

} else if (which_mod == "PH")  {
  
  #####################################################################################
  ##### PURE HABIT MODEL (PH)
  #####################################################################################
  
  ### initialise df for subs who fail to converge
  error_subs_PH  <- c()  
  max_tries  <- 10000; # maximum amount of fitting tries before considering subject a failed sub
  
  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  
  #########
  
  fitdat_PH <- data.frame()
  simdat_PH <- data.frame()
  start <- 1
  
  ### Loop over all users
  for (i in start:(length(inds))) {
    
    print(str_c("Currently fitting ",which_mod, " to user ", i, " out of ", length(inds)))
    
    # get data for this subject
    sub <- subset(fit_dat, user_num == inds[i])
    sub <- as.data.frame(sub)
    sub <- subset(sub, !is.na(t_post)) # remove any NAs
    s_data <- data.frame(t_post    = as.numeric(sub$t_post), 
                         likes     = as.numeric(sub$likes)) 
    rownames(s_data) <- NULL
    
    inits <- 50 # Number of random initializations

    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }
    
    
    for (ins in 1:inits) { # loops over the random initializations
      
      print(str_c("optim initialisation ", as.character(ins)))
      
      ### skip subjects if not converging ###
      n_tries  <- 0; #  to see how many errors there are and store and remove if too many.
      skip_sub <- FALSE;
      ############
      FitFound <- F
      
      
      while (!FitFound) {
        
        # randomized start values for the parameter:
        starting <- c(runif(1, 0, 1) # alpha_action
                      ) 

        fit.temp <- try(optim(starting, mod_PH, 
                              outp       = "fit", 
                              input_dat  = s_data, 
                              method     ="Brent",lower=0, upper=1, control = list(maxit = 500)), silent = T)
        
        ###########################
        # evaluate result...
        print(class(fit.temp))
        
        if (class(fit.temp) != "try-error") {
          if (!FitFound) {
            FitFound <- TRUE
          } 
          
        } else {
          n_tries <- n_tries + 1;
        }
        if ((n_tries > max_tries) & ins == 1) {   ## if failed too many times with this subject, and it's the first init, just move on to the next subj, and store the info.
          # the ins == 1 is here to minimise the likelihood of skipping subjets which could have succeeded - if there has already
          # been a successful init (i.e. ins > 1) then don't skip them.
          skip_sub = TRUE;
          print(paste0("Skipping the ", as.character(i), "th subject (user_num = ", as.character(sub$user_num[1]), ")"))
          break
        }
        
      }
      
      if (skip_sub == TRUE) {
        break
      }
      
      # store new parameter values if better than the last (i.e. if log likelihood is lower)
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
      error_subs_PH <- append(error_subs_PH, sub$user_num[1])
      fitdat_PH     <- rbind(fitdat_PH, data.frame(user_num = sub$user_num[1], 
                                                   LL = NA, 
                                                   AIC = NA, 
                                                   alpha_action = NA
                                                   ))
      
      next
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC <- fit$value + 2 * length(fit$par) 
    
    # save output
    fitdat_PH <- rbind(fitdat_PH, 
                       data.frame(user_num = sub$user_num[1], 
                                  LL       = fit$value, 
                                  AIC      = fit$AIC, 
                                  alpha_action  = fit$par[1]))
    
    # Now simulate the model again with the best fitting parameters, in order to extract the policy (pol_par_all) and subjective habit policy for plotting
    
    ################################################################################
    ## SIMULATE PH
    ################################################################################
    
    # Get best fitting parameters for that subject.
    fit_params_PH   <- unlist(dplyr::select(filter(fitdat_PH, user_num == sub$user_num[1]), alpha_action));

    simsub_PH       <- mod_PH(pars = fit_params_PH, 
                              outp = "sim1step", 
                              input_dat = s_data);
    
    # Save output
    simdat_PH       <- rbind(simdat_PH, 
                             data.frame(user_num    = sub$user_num[1], 
                                        t_post   = c(s_data$t_post, NA),
                                        pol_par_all = simsub_PH$pol_par_all, 
                                        simdat      = simsub_PH$simdat, 
                                        likes       = simsub_PH$likes,
                                        APE         = simsub_PH$APE,  
                                        habit       = simsub_PH$habit,
                                        AIC         = fitdat_PH[i, "AIC"], 
                                        alpha_action = fitdat_PH[i, "alpha_action"]
                                        ))
    
    ################################################################################
    
    # print
    print(i / length(inds)) #percentage completed 
    print(i)
  }
  
  ##############
  #### save
  fitdat_PH$dataset_description  <- dataset_description
  simdat_PH$dataset_description  <- dataset_description
  write_csv(fitdat_PH, str_c(fit_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))
  write_csv(simdat_PH, str_c(sim1step_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))

} else if (which_mod == "RLH1")  {
  
  #####################################################################################
  ##### RL-HABIT SINGLE LEARNING RATE MODEL (RLH1)
  #####################################################################################
  
  ### initialise df for subs who fail to converge
  error_subs_RLH1 <- c()  
  max_tries  <- 10000; # maximum amount of fitting tries before considering subject a failed sub
  
  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  
  #########
  
  fitdat_RLH1 <- data.frame()
  simdat_RLH1 <- data.frame()
  start <- 1
  
  ### Loop over all users
  for (i in start:(length(inds))) {
    
    print(str_c("Currently fitting ",which_mod, " to user ", i, " out of ", length(inds)))
    
    # get data for this subject
    sub <- subset(fit_dat, user_num == inds[i])
    sub <- as.data.frame(sub)
    sub <- subset(sub, !is.na(t_post)) # remove any NAs
    s_data <- data.frame(t_post    = as.numeric(sub$t_post), 
                         likes     = as.numeric(sub$likes)) 
    rownames(s_data) <- NULL
    
    inits <- 50 # Number of random initializations
    
    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }

    for (ins in 1:inits) { # loops over the random initializations
      print(str_c("optim initialisation ", as.character(ins)))
      
      ### skip subjects if not converging ###
      n_tries <- 0; #  to see how many errors there are and store and remove if too many.
      skip_sub <- FALSE;
      ############
      FitFound <- F
      
      while (!FitFound) {
        
        # randomized start values for the parameter:
        starting <- c(runif(1, 0, 1), # alpha
                      runif(1, .1, 10), # cost
                      runif(1, 0, 1), # alpha_action
                      runif(1, 0, 1) # stickiness_weight
        )

        # now run the fit
        fit.temp <- try(optim(starting, 
                              mod_RLH, 
                              LR_rule    = 1, # single learning rate
                              outp       = "fit", 
                              input_dat  = s_data, 
                              method     = "Nelder-Mead", control = list(maxit = 500)), silent = T)
        
        ###########################
        # evaluate result...
        print(class(fit.temp))
        
        if (class(fit.temp) != "try-error") {
          if (!FitFound) {
            FitFound <- TRUE
          } 
          
        } else {
          n_tries <- n_tries + 1;
        }
        if ((n_tries > max_tries) & ins == 1) {   ## if failed too many times with this subject, and it's the first init, just move on to the next subj, and store the info.
          # the ins == 1 is here to minimise the likelihood of skipping subjets which could have succeeded - if there has already
          # been a successful init (i.e. ins > 1) then don't skip them.
          skip_sub = TRUE;
          print(paste0("Skipping the ", as.character(i), "th subject (user_num = ", as.character(sub$user_num[1]), ")"))
          break
        }
        
      }
      
      if (skip_sub == TRUE) {
        break
      }
      
      # store new fitted value if it is better than the last one
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
      error_subs_RLH1 <- append(error_subs_RLH1, sub$user_num[1])
      fitdat_RLH1     <- rbind(fitdat_RLH1, data.frame(user_num = sub$user_num[1], 
                                                       LL    = NA, 
                                                       AIC   = NA, 
                                                       alpha = NA,
                                                       cost  = NA,
                                                       alpha_action      = NA,
                                                       stickiness_weight = NA
      ))
      
      next
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC <- fit$value + 2 * length(fit$par) 
    
    # save output
    fitdat_RLH1 <- rbind(fitdat_RLH1, 
                         data.frame(user_num = sub$user_num[1], 
                                    LL       = fit$value, 
                                    AIC      = fit$AIC, 
                                    alpha    = fit$par[1],
                                    cost     = fit$par[2],
                                    alpha_action       = fit$par[3],
                                    stickiness_weight  = fit$par[4]
                                    ))
    
    # Now simulate the model again with the best fitting parameters, in order to extract the policy (pol_par_all) and subjective habit policy for plotting
    
    ################################################################################
    ## SIMULATE RLH1
    ################################################################################
    
    # Get best fitting parameters for that subject.
    fit_params_RLH1   <- unlist(dplyr::select(filter(fitdat_RLH1, user_num == sub$user_num[1]), alpha, cost, alpha_action, stickiness_weight));
    print("before simsub")
    simsub_RLH1          <- mod_RLH(pars   = fit_params_RLH1, 
                                    LR_rule   = 1,
                                    outp      = "sim1step", 
                                    input_dat = s_data);
    

    # Save output
    simdat_RLH1 <- rbind(simdat_RLH1, 
                         data.frame(user_num    = sub$user_num[1], 
                                    t_post      = c(s_data$t_post, NA),
                                    pol_par_all = simsub_RLH1$pol_par_all,  
                                    simdat      = simsub_RLH1$simdat, 
                                    likes       = simsub_RLH1$likes,
                                    APE         = simsub_RLH1$APE, 
                                    habit       = simsub_RLH1$habit,
                                    RPE         = simsub_RLH1$RPE,
                                    R_est       = simsub_RLH1$R_est,
                                    AIC         = fitdat_RLH1[i, "AIC"], 
                                    alpha       = fitdat_RLH1[i, "alpha"],
                                    cost        = fitdat_RLH1[i, "cost"],
                                    alpha_action      = fitdat_RLH1[i, "alpha_action"],
                                    stickiness_weight = fitdat_RLH1[i, "stickiness_weight"]
                                    ))
    
    ################################################################################
    
    # print
    print(i / length(inds)) #percentage completed 
    print(i)
  }
  
  ##############
  #### save
  fitdat_RLH1$dataset_description  <- dataset_description
  simdat_RLH1$dataset_description  <- dataset_description
  write_csv(fitdat_RLH1, str_c(fit_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))
  write_csv(simdat_RLH1,  str_c(sim1step_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))


} else if (which_mod == "RLH2")  {
  
  #####################################################################################
  ##### RL-HABIT DOUBLE LEARNING RATE MODEL
  #####################################################################################
  
  ### initialise df for subs who fail to converge
  error_subs_RLH2 <- c()  
  max_tries  <- 100000; # maximum amount of fitting tries before considering subject a failed sub
  
  # count users
  inds <- row.names(table(fit_dat$user_num))
  print(str_c(length(inds), " users to fit"))
  #########
  
  fitdat_RLH2 <- data.frame()
  simdat_RLH2 <- data.frame()
  start <- 1
  
  ### Loop over all users
  for (i in start:(length(inds))) {
    
    print(str_c("Currently fitting ", which_mod, " to user ", i, " out of ", length(inds)))
    
    # get data for this subject
    sub <- subset(fit_dat, user_num == inds[i])
    sub <- as.data.frame(sub)
    sub <- subset(sub, !is.na(t_post)) # remove any NAs
    s_data <- data.frame(t_post    = as.numeric(sub$t_post), 
                         likes     = as.numeric(sub$likes)) 
    rownames(s_data) <- NULL
    
    inits <- 50 # Number of random initializations

    if (exists("fit")) {
      rm(fit)
    }
    if (exists("fit.temp")) {
      rm(fit.temp)
    }
    
    for (ins in 1:inits) { # loops over the random initializations
      print(str_c("optim initialisation ", as.character(ins)))
      
      ### skip subjects if not converging ###
      n_tries  <- 0; #  to see how many errors there are and store and remove if too many.
      skip_sub <- FALSE;
      ############
      FitFound <- F

      while (!FitFound) {
        # randomly initialise starting points for parameters
        starting <- c(runif(1, 0, 1),   # alpha_P
                      runif(1, 0, 1),   # alpha_N
                      runif(1, .1, 10), # cost
                      runif(1, 0, 1),   # alpha_action
                      runif(1, 0, 1)    # stickiness_weight
        )

        # run fits        
        fit.temp <- try(optim(starting, 
                              mod_RLH, 
                              LR_rule    = 2,
                              outp       = "fit", 
                              input_dat  = s_data, 
                              method     = "Nelder-Mead", control = list(maxit = 500)), silent = T)
        
        ###########################
        # evaluate result...
        print(class(fit.temp))
        
        if (class(fit.temp) != "try-error") {
          if (!FitFound) {
            FitFound <- TRUE
          } 
          
        } else {
          n_tries <- n_tries + 1;
        }
        if ((n_tries > max_tries) & ins == 1) {   ## if failed too many times with this subject, and it's the first init, just move on to the next subj, and store the info.
          # the ins == 1 is here to minimise the likelihood of skipping subjets which could have succeeded - if there has already
          # been a successful init (i.e. ins > 1) then don't skip them.
          skip_sub = TRUE;
          print(paste0("Skipping the ", as.character(i), "th subject (user_num = ", as.character(sub$user_num[1]), ")"))
          break
        }
        
      }
      
      if (skip_sub == TRUE) {
        break
      }
      
      # store best fitting parameter values if better than previous ones (i.e. if log likelihood is lower than previous ones)
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
      error_subs_RLH2 <- append(error_subs_RLH2, sub$user_num[1])
      fitdat_RLH2     <- rbind(fitdat_RLH2, 
                               data.frame(user_num = sub$user_num[1],
                                          LL      = NA, 
                                          AIC     = NA, 
                                          alpha_P = NA,
                                          alpha_N = NA,
                                          cost    = NA,
                                          alpha_action      = NA,
                                          stickiness_weight = NA
                                          ))
      next
    }
    
    # Calculate Akaike Information Criterion
    fit$AIC <- fit$value + 2 * length(fit$par) 
    
    # save output
    fitdat_RLH2 <- rbind(fitdat_RLH2, 
                         data.frame(user_num = sub$user_num[1], 
                                    LL       = fit$value, 
                                    AIC      = fit$AIC, 
                                    alpha_P  = fit$par[1],
                                    alpha_N  = fit$par[2],
                                    cost     = fit$par[3],
                                    alpha_action       = fit$par[4],
                                    stickiness_weight  = fit$par[5]
                                    ))
    
    # Now simulate the model again with the best fitting parameters, in order to extract the policy (pol_par_all) and subjective habit policy for plotting
   
    ################################################################################
    ## SIMULATE RLH2
    ################################################################################
    
    # Get best fitting parameters for that subject.
    fit_params_RLH2   <- unlist(dplyr::select(filter(fitdat_RLH2, user_num == sub$user_num[1]), alpha_P, alpha_N, cost, alpha_action, stickiness_weight));

    simsub_RLH2       <- mod_RLH(pars      = fit_params_RLH2, 
                                 LR_rule   = 2,
                                 outp      = "sim1step", 
                                 input_dat = s_data);
    

    # Save output
    simdat_RLH2 <- rbind(simdat_RLH2, 
                         data.frame(user_num     = sub$user_num[1], 
                                    t_post       = c(s_data$t_post, NA),
                                    pol_par_all  = simsub_RLH2$pol_par_all,  
                                    simdat       = simsub_RLH2$simdat, 
                                    likes        = simsub_RLH2$likes,
                                    APE          = simsub_RLH2$APE, 
                                    habit        = simsub_RLH2$habit,
                                    RPE          = simsub_RLH2$RPE,
                                    R_est        = simsub_RLH2$R_est,
                                    AIC          = fitdat_RLH2[i, "AIC"], 
                                    alpha_P      = fitdat_RLH2[i, "alpha_P"],
                                    alpha_N      = fitdat_RLH2[i, "alpha_N"],
                                    cost         = fitdat_RLH2[i, "cost"],
                                    alpha_action      = fitdat_RLH2[i, "alpha_action"],
                                    stickiness_weight = fitdat_RLH2[i, "stickiness_weight"]
                                    ))
    ################################################################################
    
    # print
    print(i / length(inds)) #percentage completed 
    print(i)
  }
  
  ##############
  #### save
  fitdat_RLH2$dataset_description  <- dataset_description
  simdat_RLH2$dataset_description  <- dataset_description
  write_csv(fitdat_RLH2, str_c(fit_path, "/", format(Sys.time(),"%y%m%d-%H%M"), which_mod, seed, ".csv"))
  write_csv(simdat_RLH2,  str_c(sim1step_path,"/", format(Sys.time(), "%y%m%d-%H%M"), which_mod, seed, ".csv"))
}
