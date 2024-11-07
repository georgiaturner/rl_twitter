#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#-----------------------------------------    01-MODEL_FUNCTIONS.R        --------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

##### This script contains the model functions for the models to be fitted to 
##### Twitter data. Each of the model functions can be used either to fit
##### to data, or do one-step simulations or generative simulations (for a 
##### description of the difference between one-step and generative simulations, see
##### Palminteri, Wyart et al 2017 TICS).

# By Georgia Turner, 2024 < georgianjt@gmail.com >

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#---------------------------    OVERVIEW OF THE MODELS   ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

# Overall, this script contains the following models:

# 1) Fixed Policy model (FP) 

# This is equivalent to Lindstrom et al 2021's 'no learning' model
# We instead call it a 'fixed policy' model, as it assumes
# a fixed probability of posting across time, so effectively just extracts
# the average of the t_posts (such that the policy is the mean of the exponential
# distribution from which they are drawn) and uses that as the policy at every timepoint. 

# 2) Changing Policy model (CP)

# This fits a three-parameter Mitscherlich function to the evolving policy.
# this allows for a drifting change in posting latencies but not 
# one that 'learns' from likes or previous actions (unlike the RL and habit models, respectively). 

# 3) Pure RL Model (RL) (same function for both single and double learning rates; RL1 & RL2)

# Within the RL model there are two subtypes, which are defined with the input 
# argument to the function, "LR_rule" (short for 'learning rate rule').
# i)   RL1: A single learning rate.
# ii)  RL2: Separate learning rate for positive and negative reward prediction errors (RPEs).

# 4) Pure Habit Model (PH)

# Model inspired by Miller et al (2019) Habits without Values; just has an action learning rate.

# 5) Hybrid RL-habit model (RLH) (same function for both single and double learning rates; RLH1 & RLH2)

# Model inspired by Miller et al (2019) Habits without Values: 
# Combines a 'goal directed controller' (the same as the RL model)
# and a 'habitual controller' (the same as the PH model), 
# where the influence of each one on the final policy is weighted using a free 'stickiness weight' parameter.
# i)   RLH1: A single learning rate in the goal-directed controller.
# ii)  RLH2: Separate learning rate for positive and negative RPEs in the goal-directed controller.


###############################################################################

# All models differ in how the policy is defined, but then all their policies are used
# to make decisions in the same way (i.e., posting latency at time t is a draw from 
# an exponential distribution where policy at time t contributes to the mean of the exponential 
# distribution, as described in Lindstrom et al 2021.)
# For each model function, to make the same model function flexible to model 
# fitting, onestep simulations or generative simulations, 
# all model functions contain an input parameter called 'outp', which controls whether the 
# model outputs 
## (1) the loglikelihood, used for model fitting (outp = "fit"), 
## (2) onestep simulation data (outp = "sim1step"), or 
## (3) generative simulation data (outp = "simgen")

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------   DEFINE MODEL FUNCTIONS        ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

#####################################################################################
##### FIXED POLICY MODEL (FP)
#####################################################################################

# Note: this code adapted from 'fit_no_learning' model by Lindstrom, 
# saved at https://osf.io/gft7r, to which it is mathematically equivalent.

mod_FP <- function(pars, outp, input_dat) { 
  
  # note that for this model (and for the CP model) there is no algorithmic difference between sim1step and simgen
  # as the policy is defined by the input data (input_dat) rather than the subject's choices; however
  # we retain the distinction in this function for consistency with the PH, RL and RLH models
  
  #####################################
  # INPUT MEANINGS:
  # pars       = number, parameter to simulate with: the policy
  # outp       = string, what the function is used for: either "fit", "sim1step" (one-step ahead simulation), or "simgen" (generative simulation)
  # input_dat  = dataframe including at least likes sequence, for outp = 'fit' and outp = 'sim1step' it will also contain t_post sequence
  #              for fit and sim1step, the 'Like' sequence will usually be what a subject actually experienced,
  #              for simgen the 'Like' sequence will usually have been generated outside of the function.
  #              note that the Likes here are technically not necessary in this model, as all decisions
  #              by the agent ignore Likes.
  #              however, we keep it for comparison with RL models, i.e. so the model can output the Likes which 
  #              the agent 'received' and then future analyses can show the extent to which the model choices were
  #              independent of the Likes relative to other models which do include the ikes.
  
  #####################################
  ### GET INPUTS
  
  ### number of posts, likes and tpost
  nposts    <- length(input_dat$likes) # number of posts
  likes     <- input_dat$likes
  if (outp == "fit" || outp == "sim1step") { # for these cases, there is also an input of t_post made by the subject already.
    t_post <- input_dat$t_post # this actually only applies to fit and is not used by sim1step in this model but keep here for consistency with RL models
  } 
  
  ### parameter
  pol_par   <- pars[1];
  if (pol_par < 0) {
    pol_par <- .0001
  } ## transform if bad input, as policy can't be negative
  #####################################
  
  ### Initialise latent / output variables.
  probdat    <- rep(NA, nposts) # log likelihood for each choice based on policy
  simdat     <- rep(NA, nposts) # simulated data for 1step or generative simulations (which of these it is depends on 'outp')
  #####################################
  
  #####################################
  ### RUN MODEL
  start <- 1 # which number post in the sequence to start with
  for (t in start:nposts) {

    # define current choice
    # For fit this is taken from input dataset.
    # For sim1step and simgen this is simulated from the current policy.
    if (outp == "fit") {
      curr.choice <- t_post[t] 
    } else if (outp == "simgen"| outp == "sim1step") {
      curr.choice <- rexp(1, 1/ pol_par) # random draw from exponential distribution with mean as policy
      simdat[t]   <- curr.choice # save
    }
    
    # save the loglikelihood of the choice with respect to the policy
    probdat[t] <- dexp(curr.choice, 1 / pol_par)

  }
  
  # define log likelihood
  probdat[probdat == 0] <- .000001 # get rid of 0s so that can get log in next step without producing -Inf
  # L is 2x negative log likelihood (x 2 for input into AIC)), which is minimised in the fitting procedure
  L                     <- -2 * sum(log(probdat[-1])) # Exclude first datapoint where Tpost is undefined (and is usually set to a random, small value in input data)
  
  # Return L for fitting or internal variables for simulations
  if (outp == "fit") {
    return(L)
  } else if (outp == "sim1step" || outp == "simgen") {
    return(list(simdat    = simdat, 
                likes     = likes))
  }
  
}

#####################################################################################
##### CHANGING POLICY MODEL (CP)
#####################################################################################

# This model allows for a function with evolving change in the policy, 
# but where the change is nevertheless not related to reward.
# It applies the Mitscherlich equation to the data, i.e. 'y = a - b * e^(-cx)' 
# which has three parameters: a, b and c

mod_CP <- function(pars, outp, input_dat) { 
  
  # note that for this model, like for mod_FP, there is no difference between sim1step and simgen
  # as the policy is defined by the parameter rather than the subject's choices; however
  # we retain the distinction in this function for consistency with the RL models, 
  # e.g. so that it is easier to just run "simgens" of all models together without getting confused
  #############
  # INPUT MEANINGS:
  # pars       = vector of parameters to simulate with, params of mitscherlich function: pars[1]=a, pars[2]=b and pars[3]=c
  # outp       = string, what the function is used for: "fit", "sim1step" (one-step ahead simulation), or "simgen" (generative simulation)
  # input_dat  = dataframe including at least Likes sequence, for fit and sim1step it will also contain t_post sequence
  #              For fit and sim1step, the Like sequence will usually be what a subject actually experienced,
  #              For simgen the Like sequence will usually have been generated outside of the function.
  #              Note that the Likes here are technically not necessary in this model, as all decisions
  #              by the agent ignore the likes.
  #              However, we keep it for comparison with RL models

  #####################################
  ### GET INPUTS
  
  ### number of posts, Likes and tpost
  nposts  <- length(input_dat$likes);
  likes   <- input_dat$likes
  if (outp == "fit" || outp == "sim1step") {
    t_post <- input_dat$t_post # this actually only applies to fit and is not used by sim1step in this model but keep here for consistency with RL models
  }
  
  # get parameters for policy
  a <- pars[1]; # pars must be a vector for the optim function to work in model fitting so don't try to make this clearer with column names etc
  b <- pars[2];
  c <- pars[3];
  
  #################################################################################
  ##### GET POLICY AT EVERY TIME POINT
  # define mitscherlich equation
  mitscherlich <- function(x, a, b, c) {
    return(a - b * exp(-c * x))
  }
  # define sequence of 'time-points'
  # define each time point as each post (so the points are not (necessarily) equally spaced in real time, instead representing posts in a sequence)
  x <- seq(1:nposts) # define each time point as each post (so the points are not (necessarily) equally spaced in real time, instead representing posts in a sequence)
  # get changing policy.
  pol_par_all <- mitscherlich (x, a, b, c)
  pol_par_all[which(pol_par_all < 0)] <- .0001 ## transform if bad input as policy can't be negative

  ### Initialise latent / output variables.
  probdat    <- rep(NA, nposts) # log likelihood for each choice based on policy
  simdat     <- rep(NA, nposts) # simulated data for 1step or generative simulations (which of these it is depends on 'outp')
  
  #####################################
  #####################################
  ### RUN MODEL
  start <- 1;
  #######
  for (t in start:nposts) {
    
    pol_par <- pol_par_all[t] # get policy for this datapoint
    
    # define current choice
    # For fit this is taken from input dataset.
    # For sim1step and simgen this is simulated from the current policy.
    if (outp == "fit") {
      curr.choice <- t_post[t] # Tpost
    } else if (outp == "simgen"| outp == "sim1step") {
      curr.choice <- rexp(1, 1 / pol_par); # random draw from exponential distribution with mean as policy
      simdat[t]   <- curr.choice
    }
    # save the loglikelihood of the choice with respect to the policy
    probdat[t] <- dexp(curr.choice, 1 / pol_par)
    
  }
  
  # define log likelihood
  probdat[probdat == 0] <- .000001 # get rid of 0s so that can get log in next step without producing -Inf
  # L is 2x negative log likelihood (x 2 for input into AIC)), which is minimised in the fitting procedure
  L                     <- -2 * sum(log(probdat[-1])) # Exclude first datapoint where Tpost is undefined (and is usually set to a random, small value in input data)
  
  # Return L for fitting or internal variables for simulations
  if (outp == "fit") {
    return(L)
  } else if (outp == "sim1step" || outp == "simgen") {
    return(list(simdat    = simdat, 
                likes     = likes, 
                policy    = pol_par_all))
  }
  
}

#####################################################################################
##### PURE RL MODEL (RL1 and RL2)
#####################################################################################

mod_RL <- function(LR_rule, pars, outp, input_dat) { 
  
  #############
  # INPUT MEANINGS:
  # LR_rule   = integer, number of learning rates (1 or 2)
  # pars      = vector of parameters to simulate with: 
  #             if single learning rate, pars[1] = alpha, pars[2] = cost
  #             if double learning rate, pars[1] = alpha_P (alpha for positive PE), pars[2] = alpha_N, pars[3] = cost 
  # outp      = string, what the function is used for: string, either "fit", "sim1step" (one-step ahead simulation), or "simgen" (generative simulation)
  # input_dat = dataframe including at least likes
  #             for outp=="fit" and outp=="sim1step", input_dat also contains "t_post"

  #####################################
  ### GET INPUT
  ### number of posts, Likes and tpost
  nposts      <- length(input_dat$likes)
  likes       <- input_dat$likes
  if (outp == "fit" || outp == "sim1step") {
    t_post <- input_dat$t_post
  } 
  
  #####################################
  ### get parameters
  if (LR_rule == 1) {
    alpha           <- pars[1]
    cost            <- pars[2]
  } else if (LR_rule == 2) {
    alpha_P         <- pars[1] # learning rate for positive PE 
    alpha_N         <- pars[2] # learning rate for negative PE
    cost            <- pars[3]
  } else {
    stop("Invalid input for LR_rule (the number of learning rates): must be either the number 1 or 2")
  }
  
  #####################################
  ### Initialise latent / output variables.
  # (vectors go one further than nposts because they also predict the reward after their final action, some of the vectors are one more, for others there is just NA at the end)
  # 
  pol_par_all <- rep(NA, nposts) # agent's policy at each time point
  simdat      <- rep(NA, nposts) # simulated data for 1step or generative simulations (which it is depends on 'outp')
  R_est       <- rep(NA, nposts + 1) # agent's expected reward, include prediction for t+1 so is one longer
  RPE         <- rep(NA, nposts) # prediction error calculated in model.
  probdat     <- rep(NA, nposts) # log likelihood for each choice with respect to policy

  #####################################
  #####################################
  ### RUN MODEL

  # Initial policy set to be 'cost / mean reward', i.e. we don't have a subjective R_est yet
  # so we choose to set it to mean R which is an approximation to this. 
  R1      <- mean(likes) 
  if (R1 < 0.0001) { # cannot be below 0 as need to divide by it in next step
    R1 <- 0.0001
  }
  pol_par <- cost / R1 
  
  #####################################
  start <- 1;
  #######
  for (t in start:nposts) { # Loop over all datapoints
    ## adjust if pol_par negative; we do this in all models, here we have to do it within loop as policy is calculated incrementally within loop
    if (pol_par < 0) {
      pol_par <- .0001
    }
    ## save current policy 
    pol_par_all[t] <- pol_par;

    ######################################
    ## Make choice and save log likelihood
    ######################################
    
    if (outp == "fit") {
      curr.choice <- t_post[t] 
    } else if (outp == "simgen"| outp == "sim1step") {
      curr.choice <- rexp(1, 1 / pol_par); # random draw from exponential distribution with mean as policy
      simdat[t]   <- curr.choice
    }
    # save the loglikelihood of the choice with respect to the policy
    probdat[t] <- dexp(curr.choice, 1 / pol_par)
    
    ######################################
    ## Perform inference for the next step
    ######################################
    
    #############
    ## Get Reward
    R <- likes[t] # observed reward

    #############
    ## Expected Reward update
    if (t == 1) { # set R_est if t=1; otherwise it just carries forward
      R_est[t] <- R; # could also set to e.g. R1, depending on data
    } 
    # Prediction error
    delta_R <- R - R_est[t]
    RPE[t]  <- delta_R
    
    ## Get learning rate depending on which model it is and, if a 2LR model, whether it is a reward or punishment
    if (LR_rule == 1) {
      a1 <- alpha
    } else if (LR_rule == 2) {
      # define whether the learning rate will be the positive or negative one, based on value of PE:
      if (RPE[t] >= 0 ) { # arbitrarily, get the positive alpha if is equal to 0 - this is unlikely to ever happen statistically
        a1 <- alpha_P
      } else {
        a1 <- alpha_N
      }
    }
    
    ## update
    R_est[t + 1] <- R_est[t] + a1 * delta_R

    #############
    ## Policy update
    if (R_est[t+1] < 0.0001) { # correct so that can divide by this number, as can't divide by 0
      R_est[t+1] <- 0.0001
    }
    
    pol_par <- cost / R_est[t+1]  
    
  }

  # define log likelihood
  probdat[probdat == 0] <- .000001 # get rid of 0s so that can get log in next step without producing -Inf
  # L is 2x negative log likelihood (x 2 for input into AIC)), which is minimised in the fitting procedure
  L                     <- -2 * sum(log(probdat[-1])) # Exclude first datapoint where Tpost is undefined (and is usually set to a random, small value in input data)

  #### Soft constraints on parameters
  if (LR_rule == 1) {
    if (alpha > 1 || alpha < 0 ) {
      L <- 1000000
    }
  } else if (LR_rule == 2) { 
    if ( alpha_P > 1 || alpha_P < 0 || alpha_N > 1 || alpha_N < 0) {
      L <- 1000000
    }
  }
  if (cost <= 0) {
    L <- 1000000
  }

  # Return L for fitting or internal variables for simulations
  if (outp == "fit") {
    return(L)
  } else {
    return(list(
      pol_par_all = c(pol_par_all,NA), 
      simdat      = c(simdat, NA),
      likes       = c(likes, NA),
      R_est       = R_est, 
      RPE         = c(RPE, NA))
      )
  }     
}



#####################################################################################
##### Pure Habit Model (PH)
#####################################################################################


mod_PH <- function(pars, outp, input_dat) { 
  # Note that the generative simulations of this model (as done using 'simgen') are very dependent on the distribution from which t_post is drawn from the policy. 
  # If, as here, it is drawn from an exponential distribution, 
  # pol_par will tend towards 0 because action prediction error will usually be negative. 
  # If instead it is drawn from a distribution where t_post is more 
  # evenly distributed around pol_par, e.g. a truncated gaussian, then the policy will just follow random fluctations in the data like a 
  # gaussian random walk. 
  
  #############
  # INPUT MEANINGS:
  # pars       = number, parameter to simulate with: action learning rate
  # outp       = string, what the function is used for: string, either "fit", "sim1step" (one-step ahead simulation), or "simgen" (generative simulation)
  # input_dat  = dataframe or vector including at least likes sequence, and t_post sequence.
  #              for simgen, t_post sequence only needs to one example initial t_post (which could have been randomly generated outside the function, for
  #              example), for fit and sim1step it will contain the whole t_post sequence
  #              for fit and sim1step, the Like sequence will be what a subject actually experienced,
  #              for simgen the like sequence will have been generated outside of the function.
  #              note that the likes here are technically not necessary in this model, as all decisions
  #              by the agent ignore the likes.
  #              however, we keep it for comparison with RL models, i.e. so the model can output the Likes which 
  #              the agent 'received' and then future analyses can show the extent to which the model choices were
  #              independent of the likes relative to other models which do include the likes.
  
  #####################################
  ### GET INPUTS
  
  ### number of posts, likes and tpost
  nposts    <- length(input_dat$likes) # number of posts
  likes     <- input_dat$likes
  t_post    <- input_dat$t_post 

  ### parameter
  alpha_action   <- pars[1]; # action prediction error learning rate
  #####################################
  
  ### Initialise latent / output variables.
  probdat     <- rep(NA, nposts) # log likelihood for each choice based on policy
  simdat      <- rep(NA, nposts) # simulated data for 1step or generative simulations (which of these it is depends on 'outp')
  pol_par_all <- rep(NA, nposts) # agent's policy at each time point
  APE         <- rep(NA, nposts) # (Action) prediction error calculated in model.
  habit       <- rep(NA, nposts + 1) # habit strength which evolves across trials
  #####################################
  
  #####################################
  ### RUN MODEL
  
  pol_par    <- t_post[2]    # initialise the first t_post as the first t_post in the data.
                                # note that this is the SECOND t_post in the vector as the t_post for post 1 is randomly set to .004 

  start <- 1 
  for (t in start:nposts) {
    
    ## adjust if pol_par negative; we do this in all models, here we have to do it within loop as policy is calculated incrementally within loop
    if (pol_par < 0) {
      pol_par <- .0001 # note that these '0' corrections depend a bit on the range of the variable - e.g. if all pol_pars are in the range of 0-0.000001, this will need to be reduced
    }
    ## save current policy 
    pol_par_all[t] <- pol_par;

    ######################################
    ## Make choice and save log likelihood
    ######################################
    
    if (outp == "fit") {
      curr.choice <- t_post[t] 
    } else if (outp == "simgen" | outp == "sim1step") {
      curr.choice <- rexp(1, 1 / pol_par); # random draw from exponential distribution with mean as policy
      simdat[t]   <- curr.choice
    }
    # save the loglikelihood of the choice with respect to the policy
    probdat[t] <- dexp(curr.choice, 1 / pol_par)

    ######################################
    ## Perform inference for the next step
    ######################################
    
    #############
    ## Habit strength update
    if (t == 1) {
      habit[t]   <- pol_par_all[t]    # approximate the initial value of habit as the same as the first policy
    }
    
    ## Prediction error
    # for sim1step and fit, you learn from the input policy, whereas for simgen you learn from the simulated choice
    if (outp == "fit" || outp == "sim1step") {
      action      <- t_post[t]
    } else {
      action      <- simdat[t]
    }
    delta_H <- action - habit[t]
    APE[t]  <- delta_H

    ## update
    habit[t + 1] <- habit[t] + alpha_action * delta_H

    #############
    ## Policy update
    pol_par <- habit[t + 1]

  }

  # define log likelihood
  probdat[probdat == 0] <- .000001 # get rid of 0s so that can get log in next step without producing -Inf
  # L is 2x negative log likelihood (x 2 for input into AIC)), which is minimised in the fitting procedure
  L                     <- -2 * sum(log(probdat[-1])) # Exclude first datapoint where Tpost is undefined (and is usually set to a random, small value in input data)
  

  #### Soft constraints on parameters
  if (alpha_action > 1 || alpha_action < 0 ) {
      L <- 1000000
    }

  # Return Maximum Likelihood or internal variables (for generating predictions)
  if (outp == "fit") {
    return(L)
  } else {
    return(list(pol_par_all = c(pol_par_all, NA), # add NA on the end of the vectors to make them all the same length as APE which is 1 longer than the others because there is a final prediction at the end after all the posts
                simdat      = c(simdat, NA), 
                likes       = c(likes, NA),
                APE         = c(APE, NA),
                habit       = habit))
  }
  
}

#####################################################################################
##### Hybrid RL-habit model (RLH1 and RLH2)
#####################################################################################


mod_RLH <- function(LR_rule, pars, outp, input_dat) { 
  #############
  # INPUT MEANINGS:
  # LR_rule   = integer, number of learning rates (1 or 2)
  # pars      = vector of parameters to simulate with: 
  #             if single learning rate, pars[1] = alpha, pars[2] = cost, pars[3] = alpha_action, pars[4] = stickiness_weight
  #             if double learning rate, pars[1] = alpha_P (alpha for positive PE), pars[2] = alpha_N, pars[3] = cost, pars[4] = alpha_action, pars[5] = stickiness_weight  
  # outp      = string, what the function is used for: string, either "fit", "sim1step" (one-step ahead simulation), or "simgen" (generative simulation)
  # input_dat = dataframe including at least Likes and t_post 
  #             (for simgen, t_post sequence only needs to include the initial t_post 
  #             (which could have been randomly generated outside the function, for
  #             example), for fit and sim1step it will contain the whole t_post sequence

  #####################################
  ### GET INPUTS

  ### number of posts, likes and tpost
  nposts    <- length(input_dat$likes) # number of posts
  likes     <- input_dat$likes
  t_post    <- input_dat$t_post 

  #####################################
  ### get parameters
  if (LR_rule == 1) {
    alpha           <- pars[1]
    cost            <- pars[2]
    alpha_action    <- pars[3]
    stickiness_weight    <- pars[4]
  } else if (LR_rule == 2) {
    alpha_P         <- pars[1] # learning rate for positive PE 
    alpha_N         <- pars[2] # learning rate for negative PE
    cost            <- pars[3]
    alpha_action    <- pars[4]
    stickiness_weight    <- pars[5]
  } else {
    stop("Invalid input for LR_rule (the number of learning rates): must be either the number 1 or 2")
  }
  
  #####################################
  
  ### Initialise latent / output variables.
  probdat     <- rep(NA, nposts) # log likelihood for each choice based on policy
  simdat      <- rep(NA, nposts) # simulated data for 1step or generative simulations (which of these it is depends on 'outp')
  pol_par_all <- rep(NA, nposts) # agent's policy at each time point
  RPE         <- rep(NA, nposts) # Reward prediction error
  APE         <- rep(NA, nposts) # Action prediction error
  R_est       <- rep(NA, nposts + 1) # Reward estimated value which evolves across trials
  habit       <- rep(NA, nposts + 1) # habit strength which evolves across trials, also policy for habit component (habit controller)
  RL_policy   <- rep(NA, nposts + 1) # policy for RL component (goal-directed controller)
  #####################################
  
  #####################################
  #####################################
  ### RUN MODEL
  
  # Initial policy set to be cost / the mean reward, i.e. we don't have a subjective R_est yet
  # so we choose to set it to mean R which is an approximation to this. 
  R1      <- mean(likes) 
  if (R1 <= 0.0001) { # cannot be below 0 as need to divide by it in next step
    R1 <- 0.0001
  }
  pol_par <- cost / R1 
  
  #####################################
  start <- 1;
  #######
  for (t in start:nposts) {
    
    ## adjust if pol_par negative; we do this in all models, but here we have to do it within loop as policy is calculated incrementally within loop
    if (pol_par < 0) {
      pol_par <- .0001
    }
    ## save current policy 
    pol_par_all[t] <- pol_par;
    
    ######################################
    ## Make choice and save log likelihood
    ######################################
    
    if (outp == "fit") {
      curr.choice <- t_post[t] 
    } else if (outp == "simgen" | outp == "sim1step") {
      curr.choice <- rexp(1, 1 / pol_par); # random draw from exponential distribution with mean as policy
      simdat[t]   <- curr.choice
    }
    # save the loglikelihood of the choice with respect to the policy
    probdat[t] <- dexp(curr.choice, 1 / pol_par)
    
    ######################################
    ## Perform inference for the next step
    ######################################

    ##########################
    ## Habit update
    if (t == 1) {
      habit[t]   <- pol_par_all[t]    # approximate the initial value of habit as the same as the first policy
    }
    
    ## Prediction error
    # for sim1step and fit, you learn from the input policy, whereas for simgen you learn from the simulated choice
    if (outp == "fit" || outp == "sim1step") {
      action      <- t_post[t]
    } else {
      action      <- simdat[t]
    }
    delta_H      <- action - habit[t]
    APE[t]       <- delta_H
    habit[t + 1] <- habit[t] + alpha_action * delta_H
    
   
    ##########################
    ## Reward update
    ## Get Reward
    R <- likes[t] # observed reward
    if (t == 1) { # set R_est if t=1; otherwise it just carries forward
      R_est[t] <- R; # could also set to e.g. R1, depending on data
    } 
    # Prediction error
    delta_R  <- R - R_est[t]
    RPE[t]   <- delta_R
    
    #############
    ## Get reward learning rate depending on which model it is and, if a double learning rate model, whether it is a reward or punishment
    if (LR_rule == 1) {
      a1 <- alpha
    } else if (LR_rule == 2) {
      # define whether the learning rate will be the positive or negative one, based on value of PE:
      if (RPE[t] >= 0 ) { # arbitrarily, get the positive alpha if is equal to 0 - this is unlikely to ever happen statistically
        a1 <- alpha_P
      } else {
        a1 <- alpha_N
      }
    }
    
    ## Reward estimate update
    R_est[t + 1] <- R_est[t] + a1 * delta_R

    #############
    ## Policy update
    if (R_est[t+1] <= 0.0001) { # correct so that can divide by this number, as can't divide by 0
      R_est[t+1] <- 0.0001
    }
    
    RL_policy[t+1] <- ( cost / R_est[t + 1] )
    
    #############
    pol_par <-  ( stickiness_weight * habit[t + 1] )  + ((1 - stickiness_weight) * ( RL_policy[t + 1] ))
    
  }
  
  # define log likelihood
  probdat[probdat == 0] <- .000001 # get rid of 0s so that can get log in next step without producing -Inf
  # L is 2x negative log likelihood (x 2 for input into AIC)), which is minimised in the fitting procedure
  L                     <- -2 * sum(log(probdat[-1])) # Exclude first datapoint where Tpost is undefined (and is usually set to a random, small value in input data)
  
  #### Soft constraints on parameters
  if (alpha_action > 1 || alpha_action < 0 ) {
    L <- 1000000
  }
  if (stickiness_weight > 1 || stickiness_weight < 0 ) {
    L <- 1000000
  }
  if (LR_rule == 1) {
    if (alpha > 1 || alpha < 0 ) {
      L <- 1000000
    }
  } else if (LR_rule == 2) { 
    if ( alpha_P > 1 || alpha_P < 0 || alpha_N > 1 || alpha_N < 0) {
      L <- 1000000
    }
  }
  if (cost <= 0) {
    L <- 1000000
  }
  
  # Return L for fitting or internal variables for simulations
  if (outp == "fit") {
    return(L)
  } else {
    return(list(pol_par_all = c(pol_par_all, NA),
                simdat      = c(simdat, NA),
                likes       = c(likes, NA),
                APE         = c(APE, NA),
                RPE         = c(RPE, NA),
                habit       = habit,
                R_est       = R_est,
                RL_policy   = RL_policy))
  }
  
}
