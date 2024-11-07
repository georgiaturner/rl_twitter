


#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#-------------------------------------     03-SIMULATION_GENERATIVE.R     --------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# This script uses the model functions to run generative simulations and save new synthetic simulated datasets.
# For a the model functions, refer to the script "01-model_functions.R".

# By Georgia Turner, 2024 < georgianjt@gmail.com >

################################################################################

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------                  SET UP         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

rm(list = ls())
seed <- 14
set.seed(seed)

library(tidyverse)
library(lubridate)
library(here)

setwd(here("02-model_fitting_and_simulation"))
source("01-model_functions.R"); # load in model functions.


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------               LOAD DATA         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

########## 
## load empirical and fitted data, which will be used to get distribution of parameters for simulations.
## If you don't want to use empirical data can just set the simulation parameters (i.e. lambda_start and step_size) below to whatever value is desired
empdat_path              <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
#data_name_string <- "241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw" # this is the dataset we will be basing the simulation parameters off
data_name_string <- "241105_241104_241104_240416_AHconf_cleaned_preproc_AICw" # this is the dataset we will be basing the simulation parameters off
empdat           <- read_csv(str_c(empdat_path, data_name_string, ".csv"))
tpost_unit       <- "days";
# note the t_post unit days is necessary for anything using 
# the model functions, as these have inbuilt parameter constraints which assume a t_post unit of a certain range
#####
if (tpost_unit == "seconds") {
  empdat$t_post <- empdat$t_post/ (3600*24)
  tpost_unit <- "days"
}


## create folders for saving if don't already exist
simgen_path <- str_c("./../../data_processed/Twitter/simgen/",data_name_string)
if (!file.exists(simgen_path)) {
  dir.create(simgen_path, recursive = TRUE)
}



#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#----------------         DEFINE SIMULATION PARAMETERS     -------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

######### ######### ######### ######### ######### ######### ######### ######### 
## specify settings

n_simposts <- 1000   # number of posts to simulate
which_pars <- "pars_actual" 
# if pars_normrange --- the parameters for simulations are drawn from the range of all fitted parameters, this is used for model and param recovery
# if pars_actual --- pars actually fitted, this is used for model falsification
if (which_pars == "pars_normrange") {
  n_simsubs  <- 1500  # number of subjects to simulate
} else if (which_pars == "pars_actual") {
  n_simsubs  <- length(unique(empdat$user_num))
} else {
  stop("Define which parameters to use for simulations by setting the variable 'which_pars' to a valid value.")
}

######### ######### ######### ######### ######### ######### ######### ######### 


################################################################################################# 
######  General variable set up: set up variables which will be in common between models.  ###### 
#################################################################################################


startsub   <- 1 # the subject to start from. so the subjects will be from subject startsub : subject startsub + nsimsubs

########
## Define functions to simulate likes
########

# function to generate likes using this poisson dist and step size.
gen_evolving_pois_likes <- function(lambda_start, step_size, n_simposts) {
  # initialise empty vectors
  lambda_likes    <- numeric(n_simposts); # the mean of the poisson dist for likes changes each post; initialise the means here.
  sim_likes       <- numeric(n_simposts); # the actual likes (drawn from the poisson dist)
  # generate the lambda of the poisson distribution for this post
  for (post in 1:n_simposts) {
    if (post == 1) {
      lambda_likes[post] <- lambda_start;
    } else {
      lambda_likes[post] <- rnorm(1, lambda_likes[post-1], step_size)
    }
    if (lambda_likes[post] < 0) {
      lambda_likes[post] <- 0
    }
    # draw from this distribution to generate likes
    sim_likes[post] <- rpois(1, lambda_likes[post])
  }
  
  return(list(sim_likes    = sim_likes,
              lambda_likes = lambda_likes))
}

## get lambda_start: median of mean likes for each user, which will be the lambda for the poisson dist for the first post for each simulated user before the random walk
# this is intended to get an idea of the average amount of likes;
# chose mean for each user as it is mean of poisson dist, and also to have a non whole number (if did both medians then would be a whole number)
# then median of this as is exponential, so dont want to be too dependent on skew
lambda_start <- empdat %>%  
  group_by(user_num) %>%
  summarize(mean_likes_per_user = mean(likes)) %>%
  pull(mean_likes_per_user) %>%
  median()
## get step size: this will be the standard deviation for the gaussian random walk
step_size <- empdat %>% 
  group_by(user_num) %>%
  summarize(range_likes = diff(range(likes))) %>%
  pull(range_likes) %>% 
  median()/n_simposts


######## record settings
if (which_pars == "pars_normrange") {
  # store info about parameters for saving later
  simulation_params <- str_c(which_pars, "_likesPoissonLambdaStart_",lambda_start, "_likesPoissonStepSize", step_size, "_seed", seed)
} else if (which_pars == "pars_actual") {
  # store info about parameters for saving later
  simulation_params <- str_c(which_pars, "_seed", seed)
} 


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------        RUN GENERATIVE SIMULATIONS FOR EACH MOD  -----------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

#####################################################################################
##### FIXED POLICY MODEL (FP)
#####################################################################################

which_mod <- "FP" # for saving filename

###########
# initialise dataframe for simulated data
simgen_FP    <- data.frame() 

# load the fitted parameter data to sample simulated pars from from.
empparams_FP <- filter(empdat, post_num_pic==1)$policy.fitdat_FP

###### run simulations for each simulated agent

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all agents to simulate
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))

  ################################################################################
  # generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  sim_likes     <- gen_likes$sim_likes
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes

  ################################################################################
  # generate parameter for this subject
  ################################################################################
  
  if (which_pars == "pars_normrange") { # randomly draw from range of empirical parameters
    policy <- rnorm(1, mean = mean(empparams_FP), sd = 0.5*IQR(empparams_FP))
    
  } else if (which_pars == "pars_actual") { # use policy from each subject separately
    policy <- empparams_FP[i]

    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))
    
  }
  
  if (which_pars != "pars_actual") { # if simulating with actual parameters dont change them
    if (policy < 0.001) {
      policy <- 0.001
    }
  }

  ################################################################################
  # run model
  ################################################################################
  
  input_dat <- data.frame(likes = sim_likes)
  
  ## run simulations for this subject
  simsub_FP    <- mod_FP(pars = policy,
                         outp        = "simgen",
                         input_dat   = input_dat
                         )
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_FP    <- rbind(simgen_FP,
                        data.frame(user_num     = i,
                                   policy       = policy,
                                   lambda_likes = lambda_likes,
                                   t_post       = simsub_FP$simdat,
                                   likes        = simsub_FP$likes,
                                   post_num_pic = seq(length(sim_likes))
                                   )
                        )
                                         
  
}

#### quick visualisation to check it worked
### tpost vs. policy
user_to_plot <- 33
ggplot(filter(simgen_FP, user_num ==user_to_plot)) +
  geom_point(alpha = 0.1, aes(x = post_num_pic, y = t_post, color = as.factor(user_num))) +
  geom_line(aes(x = post_num_pic, y = policy, color = as.factor(user_num))) +
  xlab("Post Number") +
  ylab("Simulated T_Post") +
  theme_classic()+
  theme(legend.position = "none")
### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_FP, user_num == user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Post number") +
  ylab("Lambda and Simulated Likes (Poisson draws)") 

###### save
simgen_FP$simulation_params <- simulation_params
write_csv(simgen_FP, str_c(simgen_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))


#####################################################################################
##### CHANGING POLICY MODEL
#####################################################################################

which_mod <- "CP"


###########
# initialise dataframe for simulated data
simgen_CP    <- data.frame() 

# load the fitted parameter data to sample simulated pars from from.
empparams_CP <- select(filter(empdat, post_num_pic==1), a.fitdat_CP, b.fitdat_CP, c.fitdat_CP)

###### run simulations for each simulated agent

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all possible parameter values
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))
  
  ################################################################################
  # randomly generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes
  sim_likes     <- gen_likes$sim_likes
  
  ################################################################################
  # generate parameter for this subject
  ################################################################################
  
  
  if (which_pars == "pars_normrange") { # randomly draw from range of empirical
    a <- rnorm(1, mean = mean(empparams_CP$a.fitdat_CP), sd = 0.5*IQR(empparams_CP$a.fitdat_CP))
    b <- rnorm(1, mean = mean(empparams_CP$b.fitdat_CP), sd = 0.5*IQR(empparams_CP$b.fitdat_CP))
    c <- rnorm(1, mean = mean(empparams_CP$c.fitdat_CP), sd = 0.5*IQR(empparams_CP$c.fitdat_CP))
    
  } else if (which_pars == "pars_actual") { # use policy from each subject separately
    a <- empparams_CP$a.fitdat_CP[i]
    b <- empparams_CP$b.fitdat_CP[i]
    c <- empparams_CP$c.fitdat_CP[i]
    
    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))
    
  }

  ################################################################################
  # run model
  ################################################################################
  
  input_dat <- data.frame(likes = sim_likes)
  
  ## run simulations for this subject
  simsub_CP   <- mod_CP(pars        = c(a,b,c), # get parameters a, b and c for this subject
                        outp        = "simgen",
                        input_dat   = input_dat
  )                                            
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_CP    <- rbind(simgen_CP,
                        data.frame(user_num = i,
                                   a        = a, 
                                   b        = b,
                                   c        = c,
                                   lambda_likes = lambda_likes,
                                   likes    = simsub_CP$likes, # store the likes this agent experienced - its decisions should be irrelevant to them, but this allows for proof of this in future plotting/comparison with RL
                                   t_post   = simsub_CP$simdat,
                                   policy   = simsub_CP$policy,
                                   post_num_pic = seq(length(sim_likes))
                                         
                              ))
  
}

### tpost vs. policy
user_to_plot <- 11; # which user you want to plot
ggplot(filter(simgen_CP, user_num == user_to_plot)) +
  geom_point(aes(x = post_num_pic, y = t_post, color = "t_post")) +
  geom_point(aes(x = post_num_pic, y = policy, color = "policy")) +
  xlab("Post Number") +
  ylab("Simulated T_Post") +
  theme(legend.position = "none")
### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_CP, user_num ==user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Post Number") +
  ylab("Simulated Likes (Poisson draws)") +
  theme(legend.position = "none")


###### save
simgen_CP$simulation_params <- simulation_params
write_csv(simgen_CP, str_c(simgen_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))

#####################################################################################
##### RL SINGLE LEARNING RATE MODEL (RL1)
#####################################################################################

which_mod <- "RL1"


###########
# initialise dataframe for simulated data
simgen_RL1 <- data.frame()

# load the fitted parameter data to sample simulated pars from from.
empparams_RL1  <- select(filter(empdat, post_num_pic==1), alpha.fitdat_RL1, cost.fitdat_RL1)

###### run simulations for each simulated agent

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all possible parameter values
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))
  
  
  ################################################################################
  # randomly generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes
  sim_likes     <- gen_likes$sim_likes

  ################################################################################
  # generate parameter for this subject
  ################################################################################
  
  if (which_pars == "pars_normrange") { # randomly draw from range of empirical
    alpha  <- rnorm(1, mean = mean(empparams_RL1$alpha.fitdat_RL1), sd = 0.5*IQR(empparams_RL1$alpha.fitdat_RL1))
    cost   <- rnorm(1, mean = mean(empparams_RL1$cost.fitdat_RL1), sd = 0.5*IQR(empparams_RL1$cost.fitdat_RL1))
    
  } else if (which_pars == "pars_actual") { # use policy from each subject separately
    alpha    <- empparams_RL1$alpha.fitdat_RL1[i]
    cost     <- empparams_RL1$cost.fitdat_RL1[i]
    
    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))
    
  }
  
  ################################################################################
  # apply constraints to parameters as necessary
  
  if (which_pars != "pars_actual") { # if simulating with actual parameters dont change them
    
    if (alpha < 0.001) {
      alpha <- 0.001
    } else if (alpha >=1) {
      alpha <- 1
    }
    
    if (cost < 0.001) {
      cost <- 0.001
    }
  }

  ################################################################################
  # run model
  ################################################################################
  
  # run simulations for that subject
  
  input_dat <- data.frame(likes = sim_likes)
  
  simsub_RL1 <- tryCatch({
    mod_RL(LR_rule    = 1,
           pars    = c(alpha, cost),
           outp    = "simgen",
           input_dat = input_dat
    )
  }, error = function(e) {
    # Error handling code
    print("Error: unable to simulate, skipping this subject.")
    list(policy = rep(NA, length(sim_likes)),
         simdat = rep(NA, length(sim_likes)), 
         likes  = rep(NA, length(sim_likes)),
         R_est  = rep(NA, length(sim_likes)),
         PE     = rep(NA, length(sim_likes))
    )
  })
  
  
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_RL1    <- rbind(simgen_RL1,
                               data.frame(user_num = i,
                                          alpha        = alpha,
                                          cost         = cost,
                                          lambda_likes = lambda_likes,
                                          likes        = ifelse(is.na(simsub_RL1$likes[1:length(sim_likes)]), NA, na.omit(simsub_RL1$likes)), # store the likes this agent experienced - its decisions should be irrelevant to them, but this allows for proof of this in future plotting/comparison with RL
                                          t_post       = ifelse(is.na(simsub_RL1$simdat[1:length(sim_likes)]), NA, na.omit(simsub_RL1$simdat)),
                                          policy       = ifelse(is.na(simsub_RL1$pol_par_all[1:length(sim_likes)]), NA, na.omit(simsub_RL1$pol_par_all)),
                                          R_est        = ifelse(is.na(simsub_RL1$R_est[1:length(sim_likes)]), NA, simsub_RL1$R_est[-length(simsub_RL1$R_est)]),
                                          post_num_pic = seq(length(sim_likes))
                                          
                               ))
  
  
  
}

### tpost vs. policy
user_to_plot <- 8
ggplot(filter(simgen_RL1, user_num == user_to_plot)) +
 # geom_point(aes(x = post_num_pic, y = t_post, color = "t_post")) +
  #  geom_point(aes(x = post_num_pic, y = likes, color = "sim_likes")) +
 # geom_point(aes(x = post_num_pic, y = lambda_likes, color = "lambda_likes")) +
   geom_point(aes(x = post_num_pic, y = likes, color = "likes")) +
  geom_point(aes(x = post_num_pic, y = R_est, color = "R_est")) +
  #geom_point(aes(x = post_num_pic, y = policy, color = "policy")) +
  
# coord_cartesian(ylim = c(0,100)) +
  xlab("Post Number") +
  ylab("Post latency / Likes") + 
  #ggtitle(str_c("User ", user_to_plot, ", RLnewNoInt"))
  ggtitle(str_c("alpha = ", filter(simgen_RL1, user_num == user_to_plot)$alpha[1],
                "\ncost = ", filter(simgen_RL1, user_num == user_to_plot)$cost[1]))
empparams_RL1[user_to_plot,]


### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_RL1, user_num ==user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Post Number") +
  ylab("Simulated Likes and Lambda Likes (Poisson draws)") +
  theme(legend.position = "none")


###### save
simgen_RL1$simulation_params <- simulation_params
write_csv(simgen_RL1, str_c(simgen_path, "/", format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))


#####################################################################################
##### RL DOUBLE LEARNING RATE MODEL (RL2)
#####################################################################################

which_mod <- "RL2"

###########
# initialise dataframe for simulated data
simgen_RL2 <- data.frame()

# load the fitted parameter data to sample simulated pars from from.
empparams_RL2     <- select(filter(empdat, post_num_pic==1), alpha_P.fitdat_RL2,alpha_N.fitdat_RL2, cost.fitdat_RL2)

###### run simulations for each simulated agent

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all possible parameter values
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))
  
  ################################################################################
  # randomly generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes
  sim_likes     <- gen_likes$sim_likes

  ################################################################################
  # generate parameter for this subject
  ################################################################################
  
  if (which_pars == "pars_normrange") { # randomly draw from range of empirical
    alpha_P  <- rnorm(1, mean = mean(empparams_RL2$alpha_P.fitdat_RL2), sd = 0.5*IQR(empparams_RL2$alpha_P.fitdat_RL2))
    alpha_N  <- rnorm(1, mean = mean(empparams_RL2$alpha_N.fitdat_RL2), sd = 0.5*IQR(empparams_RL2$alpha_N.fitdat_RL2))
    cost     <- rnorm(1, mean = mean(empparams_RL2$cost.fitdat_RL2), sd = 0.5*IQR(empparams_RL2$cost.fitdat_RL2))
    

  } else if (which_pars == "pars_actual") { # use policy from each subject separately
    alpha_P    <- empparams_RL2$alpha_P.fitdat_RL2[i]
    alpha_N    <- empparams_RL2$alpha_N.fitdat_RL2[i]
    cost       <- empparams_RL2$cost.fitdat_RL2[i]
    
    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))
    
    
  }
  
  ################################################################################
  # apply constraints to parameters as necessary
  
  if (which_pars != "pars_actual") { # if simulating with actual parameters dont change them
    
    
    if (alpha_P < 0.001) {
    } else if (alpha_P >=1) {
      alpha_P <- 1
    }
    if (alpha_N < 0.001) {
      alpha_N <- 0.001
    } else if (alpha_N >=1) {
      alpha_N <- 1
    }
    
    if (cost < 0.001) {
      cost <- 0.001
    }
  }
  
  ################################################################################
  # run model
  ################################################################################
  
  # run simulations for that subject
  
  input_dat <- data.frame(likes    = sim_likes)
  
  simsub_RL2 <- tryCatch({
    mod_RL(LR_rule = 2,
           pars    = c(alpha_P, alpha_N, cost),
           outp    = "simgen",
           input_dat = input_dat
    )
  }, error = function(e) {
    # Error handling code
    print("Error: unable to sim, skipping.")
    list(policy = rep(NA, length(sim_likes)),
         simdat = rep(NA, length(sim_likes)), 
         likes  = rep(NA, length(sim_likes)),
         R_est  = rep(NA, length(sim_likes)),
         PE     = rep(NA, length(sim_likes))
    )
  })
  
  
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_RL2    <- rbind(simgen_RL2,
                                 data.frame(user_num = i,
                                            alpha_P      = alpha_P,
                                            alpha_N      = alpha_N,
                                            cost         = cost,
                                            lambda_likes = lambda_likes,
                                            likes        = ifelse(is.na(simsub_RL2$likes[1:length(sim_likes)]), NA, na.omit(simsub_RL2$likes)), 
                                            t_post       = ifelse(is.na(simsub_RL2$simdat[1:length(sim_likes)]), NA, na.omit(simsub_RL2$simdat)),
                                            policy       = ifelse(is.na(simsub_RL2$pol_par_all[1:length(sim_likes)]), NA, na.omit(simsub_RL2$pol_par_all)),
                                            R_est        = ifelse(is.na(simsub_RL2$R_est[1:length(sim_likes)]), NA, simsub_RL2$R_est[-length(simsub_RL2$R_est)]),
                                            post_num_pic = seq(length(sim_likes))
                                            
                                 ))
  }

### tpost vs. policy
user_to_plot <- 24 # which user you want to plot
ggplot(filter(simgen_RL2, user_num == user_to_plot)) +
 # geom_point(aes(x = post_num_pic, y = t_post, color = "t_post")) +
  #geom_point(aes(x = post_num_pic, y = policy, color = "policy")) +
    geom_point(aes(x = post_num_pic, y = likes, color = "sim_likes")) +
  geom_point(aes(x = post_num_pic, y = R_est, color = "R_est")) +
# geom_point(aes(x = post_num_pic, y = lambda_likes, color = "lambda_likes")) +
  # geom_point(aes(x = post_num_pic, y = likes, color = "likes")) +
  #coord_cartesian(ylim = c(0,100)) +
 # xlab("Post Number") +
  ylab("Simulated T_Post") + 
  ggtitle(str_c("User ", user_to_plot, ", RLdoubLR"))
empparams_RL2[user_to_plot,]


### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_RL2, user_num ==user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Lambda of Likes") +
  ylab("Simulated Likes (Poisson draws)") +
  theme(legend.position = "none")


###### save
simgen_RL2$simulation_params <- simulation_params
write_csv(simgen_RL2, str_c(simgen_path, "/",  format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))



#####################################################################################
##### PURE HABIT MODEL (PH)
#####################################################################################

# Note that the generative simulations of this model (as done using 'simgen') are 
# very dependent on the distribution from which t_post is drawn from the policy. 
# If, as here, it is drawn from an exponential distribution, because the differences 
# habit and therefore policy will tend towards 0 because APE will usually be negative. 
# If instead it is drawn from a distribution where t_post is more evenly distributed 
# around the policy, e.g. a truncated gaussian, then the policy will just follow 
# random fluctations in the data like a gaussian random walk. 

###########

which_mod <- "PH"

###########
# initialise dataframe for simulated data
simgen_PH <- data.frame()

# load the fitted parameter data to sample simulated pars from from.
empparams_PH    <- select(filter(empdat, post_num_pic==1), alpha_action.fitdat_PH)

###### run simulations for each simulated agent

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all possible parameter values
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))
  
  ################################################################################
  # randomly generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes
  sim_likes     <- gen_likes$sim_likes

  ################################################################################
  # generate parameter for this subject
  ################################################################################
  
  if (which_pars == "pars_normrange") { # randomly draw from range of empirical
    alpha_action  <- rnorm(1, mean = mean(empparams_PH$alpha_action.fitdat_PH), sd = 0.5*IQR(empparams_PH$alpha_action.fitdat_PH))
    sim_tpost     <- rexp(n_simposts, rate = 1 / median(filter(empdat, post_num_pic !=1)$t_post))


  } else if (which_pars == "pars_actual") { # use policy from each subject separately
    alpha_action    <- empparams_PH$alpha_action.fitdat_PH[i]
    sim_tpost       <- filter(empdat, user_num == unique(empdat$user_num)[i])$t_post

    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))
    
  }
  
  ################################################################################
  # apply constraints to parameters as necessary

  if (which_pars != "pars_actual") { # if simulating with actual parameters dont change them
    
    if (alpha_action < 0.001) {
      alpha_action <- 0.001
    } else if (alpha_action >=1) {
      alpha_action <- 1
    }
  }

  ################################################################################
  # run model
  ################################################################################
  

  input_dat <- data.frame(likes    = sim_likes,
                          t_post   = sim_tpost ) 
  
  
  simsub_PH <- tryCatch({
    mod_PH(pars    = alpha_action,
           outp    = "simgen",
           input_dat = input_dat
    )
  }, error = function(e) {
    # Error handling code
    print("Error: unable to sim, skipping.")
    list(policy = rep(NA, length(sim_likes)),
         simdat = rep(NA, length(sim_likes)), 
         likes  = rep(NA, length(sim_likes)),
         APE    = rep(NA, length(sim_likes)),
         habit  = rep(NA, length(sim_likes))
    )
  })
  
  
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_PH    <- rbind(simgen_PH,
                        data.frame(user_num     = i,
                                   alpha_action = alpha_action,
                                   lambda_likes = lambda_likes,
                                   likes        = ifelse(is.na(simsub_PH$likes[1:length(sim_likes)]), NA, na.omit(simsub_PH$likes)), # store the likes this agent experienced - its decisions should be irrelevant to them, but this allows for proof of this in future plotting/comparison with RL
                                   t_post       = ifelse(is.na(simsub_PH$simdat[1:length(sim_likes)]), NA, na.omit(simsub_PH$simdat)),
                                   policy       = ifelse(is.na(simsub_PH$pol_par_all[1:length(sim_likes)]), NA, na.omit(simsub_PH$pol_par_all)),
                                   APE          = ifelse(is.na(simsub_PH$APE[1:length(sim_likes)]), NA, simsub_PH$APE[-length(simsub_PH$APE)]),
                                   habit        = ifelse(is.na(simsub_PH$habit[1:length(sim_likes)]), NA, simsub_PH$habit[-length(simsub_PH$habit)]),
                                   post_num_pic = seq(length(sim_likes))
                                   ))
}

### tpost vs. policy
user_to_plot <- 6; # which user you want to plot
ggplot(filter(simgen_PH, user_num == user_to_plot)) +
  geom_point(aes(x = post_num_pic, y = t_post, color = "t_post")) +
  geom_point(aes(x = post_num_pic, y = policy, color = "policy")) +
  #  geom_point(aes(x = post_num_pic, y = likes, color = "sim_likes")) +
 ## geom_point(aes(x = post_num_pic, y = habit, color = "habit")) +
#  geom_point(aes(x = post_num_pic, y = lambda_likes, color = "lambda_likes")) +
 #  geom_point(aes(x = post_num_pic, y = likes, color = "likes")) +
 # coord_cartesian(ylim = c(0,100)) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  ggtitle(str_c("User ", user_to_plot, ", ", which_mod))
empparams_PH[user_to_plot,]

### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_PH, user_num ==user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Lambda of Likes") +
  ylab("Simulated Likes (Poisson draws)") +
  theme(legend.position = "none")


###### save
simgen_PH$simulation_params <- simulation_params
write_csv(simgen_PH, str_c(simgen_path, "/",  format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))


#####################################################################################
##### RL-HABIT SINGLE LEARNING RATE MODEL (RLH1)
#####################################################################################

which_mod <- "RLH1"

###########
# initialise dataframe for simulated data
simgen_RLH1 <- data.frame()

# load the fitted parameter data to sample simulated pars from from.
empparams_RLH1 <- select(filter(empdat, post_num_pic==1), 
                         alpha.fitdat_RLH1, 
                         cost.fitdat_RLH1,
                         alpha_action.fitdat_RLH1,
                         stickiness_weight.fitdat_RLH1)

###### run simulations for each simulated agent

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all possible parameter values
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))
  
  ################################################################################
  # randomly generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes
  sim_likes     <- gen_likes$sim_likes

  ################################################################################
  # generate parameter for this subject
  ################################################################################
  
  if (which_pars == "pars_normrange") { # randomly draw from range of empirical
    alpha              <- rnorm(1, mean = mean(empparams_RLH1$alpha.fitdat_RLH1), sd = 0.5*IQR(empparams_RLH1$alpha.fitdat_RLH1))
    cost               <- rnorm(1, mean = mean(empparams_RLH1$cost.fitdat_RLH1), sd = 0.5*IQR(empparams_RLH1$cost.fitdat_RLH1))
    alpha_action       <- rnorm(1, mean = mean(empparams_RLH1$alpha_action.fitdat_RLH1), sd = 0.5*IQR(empparams_RLH1$alpha_action.fitdat_RLH1))
    stickiness_weight  <- rnorm(1, mean = mean(empparams_RLH1$stickiness_weight.fitdat_RLH1), sd = 0.5*IQR(empparams_RLH1$stickiness_weight.fitdat_RLH1))
    
    sim_tpost          <- rexp(n_simposts, rate = 1 / median(filter(empdat, post_num_pic !=1)$t_post))
    
  } else if (which_pars == "pars_actual") { # use policy from each subject separately
    alpha             <- empparams_RLH1$alpha.fitdat_RLH1[i]
    cost              <- empparams_RLH1$cost.fitdat_RLH1[i]
    alpha_action      <- empparams_RLH1$alpha_action.fitdat_RLH1[i]
    stickiness_weight <- empparams_RLH1$stickiness_weight.fitdat_RLH1[i]
    
    sim_tpost       <- filter(empdat, user_num == unique(empdat$user_num)[i])$t_post

    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))
    
  }
  
  #alpha <- 0.01
  #cost <- 0.5
  #alpha_action <- 0.001
  #stickiness_weight <- 0.6
  
  ################################################################################
  # apply constraints
  if (which_pars != "pars_actual") { # if simulating with actual parameters dont change them
    
    if (alpha < 0.001) {
      alpha <- 0.001
    } else if (alpha >=1) {
      alpha <- 1
    }
    if (cost < 0.001) {
      cost <- 0.001
    }
    if (alpha_action < 0.001) {
      alpha_action <- 0.001
    } else if (alpha_action >=1) {
      alpha_action <- 1
    }
    if (stickiness_weight < 0.001) {
      stickiness_weight <- 0.001
    } else if (stickiness_weight >=1) {
      stickiness_weight <- 1
    }
  }
  
  ################################################################################
  # run model
  ################################################################################
  
  input_dat <- data.frame(likes    = sim_likes,
                          t_post   = sim_tpost)
  
  simsub_RLH1 <- tryCatch({
    mod_RLH(pars      = c(alpha, cost, alpha_action, stickiness_weight),
            LR_rule   = 1,
            outp      = "simgen",
            input_dat = input_dat
    )
  }, error = function(e) {
    # Error handling code
    print("Error: unable to sim, skipping.")
    list(policy = rep(NA, length(sim_likes)),
         simdat = rep(NA, length(sim_likes)), 
         likes  = rep(NA, length(sim_likes)),
         APE    = rep(NA, length(sim_likes)),
         habit  = rep(NA, length(sim_likes)),
         RPE    = rep(NA, length(sim_likes)),
         R_est  = rep(NA, length(sim_likes))
    )
  })
  
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_RLH1    <- rbind(simgen_RLH1,
                          data.frame(user_num     = i,
                                     alpha        = alpha,
                                     cost         = cost,
                                     alpha_action = alpha_action, 
                                     stickiness_weight = stickiness_weight,
                                     lambda_likes = lambda_likes,
                                     likes        = ifelse(is.na(simsub_RLH1$likes[1:length(sim_likes)]), NA, na.omit(simsub_RLH1$likes)), 
                                     t_post       = ifelse(is.na(simsub_RLH1$simdat[1:length(sim_likes)]), NA, na.omit(simsub_RLH1$simdat)),
                                     policy       = ifelse(is.na(simsub_RLH1$pol_par_all[1:length(sim_likes)]), NA, na.omit(simsub_RLH1$pol_par_all)),
                                     APE          = ifelse(is.na(simsub_RLH1$APE[1:length(sim_likes)]), NA, simsub_RLH1$APE[-length(simsub_RLH1$APE)]),
                                     habit        = ifelse(is.na(simsub_RLH1$habit[1:length(sim_likes)]), NA, simsub_RLH1$habit[-length(simsub_RLH1$habit)]),
                                     RPE          = ifelse(is.na(simsub_RLH1$RPE[1:length(sim_likes)]), NA, simsub_RLH1$RPE[-length(simsub_RLH1$RPE)]),
                                     R_est        = ifelse(is.na(simsub_RLH1$R_est[1:length(sim_likes)]), NA, simsub_RLH1$R_est[-length(simsub_RLH1$R_est)]),
                                     RL_policy    = ifelse(is.na(simsub_RLH1$RL_policy[1:length(sim_likes)]), NA, simsub_RLH1$RL_policy[-length(simsub_RLH1$RL_policy)]),
                                     post_num_pic = seq(length(sim_likes))
                                     ))
}

### tpost vs. policy
user_to_plot <- 3; # which user you want to plot
ggplot(filter(simgen_RLH1, user_num == user_to_plot)) +
# geom_point(aes(x = post_num_pic, y = t_post, color = "t_post")) +
 #geom_point(aes(x = post_num_pic, y = likes, color = "likes")) +
#  geom_point(aes(x = post_num_pic, y = habit, color = "habit")) +
# geom_point(aes(x = post_num_pic, y = lambda_likes, color = "lambda_likes")) +
  geom_point(aes(x = post_num_pic, y = R_est, color = "R_est")) +
  geom_point(aes(x = post_num_pic, y = policy, color = "policy")) +
  
 # geom_point(aes(x = post_num_pic, y = RL_policy, color = "RL_policy")) +
  # coord_cartesian(ylim = c(0,100)) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
 # coord_cartesian(ylim = c(0, 500)) +
  ggtitle(str_c("User ", user_to_plot, ", ", which_mod))
empparams_RLH1[user_to_plot,]

### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_RLH1, user_num ==user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Lambda of Likes") +
  ylab("Simulated Likes (Poisson draws)") +
  theme(legend.position = "none")


###### save
simgen_RLH1$simulation_params <- simulation_params
write_csv(simgen_RLH1, str_c(simgen_path, "/",  format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))


#####################################################################################
##### RL-HABIT DOUBLE LEARNING RATE MODEL (RLH2)
#####################################################################################

which_mod <- "RLH2"

# initialise dataframe for simulated data
simgen_RLH2 <- data.frame()

###########
# load the fitted parameter data to sample simulated pars from from.
empparams_RLH2     <- select(filter(empdat, post_num_pic==1), 
                             alpha_P.fitdat_RLH2, 
                             alpha_N.fitdat_RLH2, 
                             cost.fitdat_RLH2,
                             alpha_action.fitdat_RLH2,
                             stickiness_weight.fitdat_RLH2)

###### run simulations for each simulated subject

for (i in seq(startsub, startsub + n_simsubs - 1)) { # loop over all possible parameter values
  
  print(str_c("Simulating ",  which_mod, " model for subject number ", i, " of ", (n_simsubs + 1 - startsub)))
  
  
  ################################################################################
  # randomly generate likes for the generated data 
  ################################################################################
  
  gen_likes     <- gen_evolving_pois_likes(lambda_start = lambda_start, step_size = step_size, n_simposts = n_simposts);
  lambda_likes  <- gen_likes$lambda_likes # store record of mean likes
  sim_likes     <- gen_likes$sim_likes

  ################################################################################
  # generate parameter for this subject
  ################################################################################

  if (which_pars == "pars_normrange") { # randomly draw from range of empirical
    alpha_P       <- rnorm(1, mean = mean(empparams_RLH2$alpha_P.fitdat_RLH2), sd = 0.5*IQR(empparams_RLH2$alpha_P.fitdat_RLH2))
    alpha_N       <- rnorm(1, mean = mean(empparams_RLH2$alpha_N.fitdat_RLH2), sd = 0.5*IQR(empparams_RLH2$alpha_N.fitdat_RLH2))
    cost          <- rnorm(1, mean = mean(empparams_RLH2$cost.fitdat_RLH2), sd = 0.5*IQR(empparams_RLH2$cost.fitdat_RLH2))
    alpha_action  <- rnorm(1, mean = mean(empparams_RLH2$alpha_action.fitdat_RLH2), sd = 0.5*IQR(empparams_RLH2$alpha_action.fitdat_RLH2))
    stickiness_weight  <- rnorm(1, mean = mean(empparams_RLH2$stickiness_weight.fitdat_RLH2), sd = 0.5*IQR(empparams_RLH2$stickiness_weight.fitdat_RLH2))

    sim_tpost          <- rexp(n_simposts, rate = 1 / median(filter(empdat, post_num_pic !=1)$t_post))
    
    } else if (which_pars == "pars_actual") { # use policy from each subject separately
    alpha_P         <- empparams_RLH2$alpha_P.fitdat_RLH2[i]
    alpha_N         <- empparams_RLH2$alpha_N.fitdat_RLH2[i]
    cost            <- empparams_RLH2$cost.fitdat_RLH2[i]
    alpha_action    <- empparams_RLH2$alpha_action.fitdat_RLH2[i]
    stickiness_weight    <- empparams_RLH2$stickiness_weight.fitdat_RLH2[i]
    
    sim_tpost       <- filter(empdat, user_num == unique(empdat$user_num)[i])$t_post

    # set likes to exactly the same as the real subject's likes
    sim_likes    <- filter(empdat, user_num == unique(empdat$user_num)[i])$likes
    lambda_likes <- rep(NA, length(sim_likes))

  }
  
  ################################################################################
  # apply constraints

  if (which_pars != "pars_actual") { # if simulating with actual parameters dont change them
    
    
    if (alpha_P < 0.001) {
      alpha_P <- 0.001
    } else if (alpha_P >=1) {
      alpha_P <- 1
    }
    if (alpha_N < 0.001) {
      alpha_N <- 0.001
    } else if (alpha_N >=1) {
      alpha_N <- 1
    }
    if (cost < 0.001) {
      cost <- 0.001
    }
    if (alpha_action < 0.001) {
      alpha_action <- 0.001
    } else if (alpha_action >=1) {
      alpha_action <- 1
    }
    if (stickiness_weight < 0.001) {
      stickiness_weight <- 0.001
    } else if (stickiness_weight >=1) {
      stickiness_weight <- 1
    }
  }
  
  ################################################################################
  # run model
  ################################################################################
  
  input_dat <- data.frame(likes    = sim_likes,
                          t_post   = sim_tpost)
  
  simsub_RLH2 <- tryCatch({
    mod_RLH(pars    = c(alpha_P, alpha_N, cost, alpha_action, stickiness_weight),
            LR_rule = 2,
            outp    = "simgen",
            input_dat = input_dat
    )
  }, error = function(e) {
    # Error handling code
    print("Error: unable to sim, skipping.")
    list(policy = rep(NA, length(sim_likes)),
         simdat = rep(NA, length(sim_likes)), 
         likes  = rep(NA, length(sim_likes)),
         APE    = rep(NA, length(sim_likes)),
         habit  = rep(NA, length(sim_likes)),
         RPE    = rep(NA, length(sim_likes)),
         R_est  = rep(NA, length(sim_likes))
    )
  })
  
  
  ## add to list of all subjects' simulations, naming columns with the values which can go into fit_models.
  simgen_RLH2    <- rbind(simgen_RLH2,
                          data.frame(user_num = i,
                                     alpha_P      = alpha_P,
                                     alpha_N      = alpha_N,
                                     cost         = cost,
                                     alpha_action = alpha_action, 
                                     stickiness_weight = stickiness_weight,
                                     lambda_likes = lambda_likes,
                                     likes        = ifelse(is.na(simsub_RLH2$likes[1:length(sim_likes)]), NA, na.omit(simsub_RLH2$likes)),
                                     t_post       = ifelse(is.na(simsub_RLH2$simdat[1:length(sim_likes)]), NA, na.omit(simsub_RLH2$simdat)),
                                     policy       = ifelse(is.na(simsub_RLH2$pol_par_all[1:length(sim_likes)]), NA, na.omit(simsub_RLH2$pol_par_all)),
                                     APE          = ifelse(is.na(simsub_RLH2$APE[1:length(sim_likes)]), NA, simsub_RLH2$APE[-length(simsub_RLH2$APE)]),
                                     habit        = ifelse(is.na(simsub_RLH2$habit[1:length(sim_likes)]), NA, simsub_RLH2$habit[-length(simsub_RLH2$habit)]),
                                     RPE          = ifelse(is.na(simsub_RLH2$RPE[1:length(sim_likes)]), NA, simsub_RLH2$RPE[-length(simsub_RLH2$RPE)]),
                                     R_est        = ifelse(is.na(simsub_RLH2$R_est[1:length(sim_likes)]), NA, simsub_RLH2$R_est[-length(simsub_RLH2$R_est)]),
                                     post_num_pic = seq(length(sim_likes))
                                     ))
}

### tpost vs. policy
user_to_plot <- 10; # which user you want to plot
ggplot(filter(simgen_RLH2, user_num == user_to_plot)) +
  geom_point(aes(x = post_num_pic, y = t_post, color = "t_post")) +
  geom_point(aes(x = post_num_pic, y = policy, color = "policy")) +
 # geom_point(aes(x = post_num_pic, y = likes, color = "sim_likes")) +
#  geom_point(aes(x = post_num_pic, y = habit, color = "habit")) +
 # geom_point(aes(x = post_num_pic, y = lambda_likes, color = "lambda_likes")) +
  geom_point(aes(x = post_num_pic, y = R_est, color = "R_est")) +
  # coord_cartesian(ylim = c(0,100)) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  ggtitle(str_c("User ", user_to_plot, ", ", which_mod))
empparams_RLH2[user_to_plot,]

### lambda likes vs. likes - only plot for one user at a time
ggplot(filter(simgen_RLH2, user_num == user_to_plot )) +
  geom_point(aes(x = post_num_pic, y = lambda_likes, color = as.factor(user_num))) +
  geom_point(aes(x = post_num_pic, y = likes, color = as.factor(user_num))) +
  xlab("Lambda of Likes") +
  ylab("Simulated Likes (Poisson draws)") +
  theme(legend.position = "none")



###### save
simgen_RLH2$simulation_params <- simulation_params
write_csv(simgen_RLH2, str_c(simgen_path, "/",  format(Sys.time(), "%y%m%d-%H%M"), which_mod,  ".csv"))

