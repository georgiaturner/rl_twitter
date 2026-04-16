



#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#-------------------------------------         03-RESULTS_STATS.R         --------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#


## This script uses the fitted data to compute statistics and figures for the paper, 
## 'A computational model of reward learning and habits on social media'.

# By Georgia Turner, 2024 < georgianjt@gmail.com >


################################################################################

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------                 SETUP           ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


rm(list = ls())
seed <- 1
set.seed(seed)

library(tidyverse)
library(ggplot2)
library(lubridate)
library(lme4)
library(forecast)
library(lmerTest)
library(viridis)
library(gridExtra)
library(here)
library(metafor)
library(boot)

setwd(here("03-stats_and_visualisation"))
source('00-functions.R')

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------               LOAD DATA         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


########################################
##### load empirical data
########################################

## AH
empdatAH_path              <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
AHdisc_data_name_string    <- "241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw"
AHconf_data_name_string    <- "251110_251105_251105_240416_AHconf_cleaned_preproc_AICw"
empdat_AHdisc              <- read_csv(str_c(empdatAH_path, AHdisc_data_name_string, ".csv"))
empdat_AHconf              <- read_csv(str_c(empdatAH_path, AHconf_data_name_string, ".csv"))

tpost_unit = "days";
#####
if (tpost_unit == "seconds") {
  empdat$t_post <- empdat$t_post/ (3600*24)
  tpost_unit <- "days"
  
}

# clean up empdat columns & add extra variables
empdat_AHdisc <- empdat_AHdisc %>%
 mutate(
   DOB                       = as.Date(paste(DOB, "-01", sep = "")),
   timeofquestionnaire_local = as.POSIXct(timeofquestionnaire_local, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
   age                       = as.numeric(difftime(timeofquestionnaire_local, DOB, units = "days") / 365.25)) %>%
 group_by(user_num) %>%
 mutate(mean_tpost  = mean(t_post),
        mean_likes  = mean(likes)) %>%
 ungroup()

empdat_AHconf <- empdat_AHconf %>%
  mutate(
    DOB                       = as.Date(paste(DOB, "-01", sep = "")),
    timeofquestionnaire_local = as.POSIXct(timeofquestionnaire_local, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    age                       = as.numeric(difftime(timeofquestionnaire_local, DOB, units = "days") / 365.25)) %>%
  group_by(user_num) %>%
  mutate(mean_tpost  = mean(t_post),
         mean_likes  = mean(likes)) %>%
  ungroup()

# # load simgens where they were all simulated with exact parameters of confirmatory sample
simgen_AHconf_path <- str_c("./../../data_processed/Twitter/simgen/", AHconf_data_name_string, "/");
simgen_FP_AHconf   <- read_csv(str_c(simgen_AHconf_path, "251110-1710FP.csv"))
simgen_CP_AHconf   <- read_csv(str_c(simgen_AHconf_path, "251110-1711CP.csv"))
simgen_PH_AHconf   <- read_csv(str_c(simgen_AHconf_path, "251110-1716HP.csv"))   # Note that HP is the same as the PH model
simgen_RL1_AHconf  <- read_csv(str_c(simgen_AHconf_path, "251110-1712RL1.csv"))
simgen_RL2_AHconf  <- read_csv(str_c(simgen_AHconf_path, "251110-1714RL2.csv"))
simgen_RLH1_AHconf <- read_csv(str_c(simgen_AHconf_path, "251110-1718RLH1.csv"))
simgen_RLH2_AHconf <- read_csv(str_c(simgen_AHconf_path, "251110-1721RLH2.csv"))


########################################

# save versions for OSF


#conf
empdat_AHconf_OSF <- empdat_AHconf
empdat_AHconf_OSF$user_num <- as.integer(factor(empdat_AHconf_OSF$user_num))
empdat_AHconf_OSF <- select(empdat_AHconf_OSF, 
                            -weekday, 
                            -datetime, 
                            -DOB, 
                            -timeofquestionnaire_local,
                            -t_firstpost
                            )
write_csv(empdat_AHconf_OSF,str_c(empdatAH_path,AHconf_data_name_string, "_OSF.csv"))

#disc
empdat_AHdisc_OSF <- empdat_AHdisc
empdat_AHdisc_OSF$user_num <- as.integer(factor(empdat_AHdisc_OSF$user_num))
empdat_AHdisc_OSF <- select(empdat_AHdisc_OSF, 
                            -weekday, 
                            -datetime, 
                            -DOB, 
                            -timeofquestionnaire_local,
                            -t_firstpost
)
write_csv(empdat_AHdisc_OSF,str_c(empdatAH_path,AHdisc_data_name_string, "_OSF.csv"))



########################################


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#----------------------   PREPARE VARIABLES FOR GLMS      --------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

# make datasets ready for GLMs


#### models
glmdat_sim_FP_AHconf   <- mkvars_glm(simgen_FP_AHconf, RPE_type = "prev10")
glmdat_sim_CP_AHconf   <- mkvars_glm(simgen_CP_AHconf, RPE_type = "prev10")
glmdat_sim_PH_AHconf   <- mkvars_glm(simgen_PH_AHconf, RPE_type = "prev10")
glmdat_sim_RL1_AHconf  <- mkvars_glm(simgen_RL1_AHconf, RPE_type = "prev10")
glmdat_sim_RL2_AHconf  <- mkvars_glm(simgen_RL2_AHconf, RPE_type = "prev10")
glmdat_sim_RLH1_AHconf <- mkvars_glm(simgen_RLH1_AHconf, RPE_type = "prev10")
glmdat_sim_RLH2_AHconf <- mkvars_glm(simgen_RLH2_AHconf, RPE_type = "prev10")

#### empirical
glmdat_empdatAHdisc   <- mkvars_glm(empdat_AHdisc, RPE_type = "prev10")
glmdat_empdatAHconf   <- mkvars_glm(empdat_AHconf, RPE_type = "prev10")


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#----------------------   GLM PREDICTION ERROR SIGNATURE  --------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------#
##########
############## Run GLMs
##########
#-----------------------------------------------------------------------------------#

#########
#### do GLMs
glmRPE_sim_FP_AHconf   <- glmdat_sim_FP_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_CP_AHconf   <- glmdat_sim_CP_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_PH_AHconf   <- glmdat_sim_PH_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL1_AHconf  <- glmdat_sim_RL1_AHconf  %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL2_AHconf  <- glmdat_sim_RL2_AHconf  %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH1_AHconf <- glmdat_sim_RLH1_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH2_AHconf <- glmdat_sim_RLH2_AHconf %>% glm_RPE_deltatpost(.)
# all empdat
glmRPE_empdatAHconf    <- glmdat_empdatAHconf %>% glm_RPE_deltatpost(.)

#### run GLMs for model-independent signature of habit
### AutoCorr
glmAuto_sim_FP_AHconf   <- glmdat_sim_FP_AHconf %>% glm_AutoCorr_tpost(.)
glmAuto_sim_CP_AHconf   <- glmdat_sim_CP_AHconf %>% glm_AutoCorr_tpost(.)
glmAuto_sim_PH_AHconf   <- glmdat_sim_PH_AHconf %>% glm_AutoCorr_tpost(.)
glmAuto_sim_RL1_AHconf  <- glmdat_sim_RL1_AHconf  %>% glm_AutoCorr_tpost(.)
glmAuto_sim_RL2_AHconf  <- glmdat_sim_RL2_AHconf  %>% glm_AutoCorr_tpost(.)
glmAuto_sim_RLH1_AHconf <- glmdat_sim_RLH1_AHconf %>% glm_AutoCorr_tpost(.)
glmAuto_sim_RLH2_AHconf <- glmdat_sim_RLH2_AHconf %>% glm_AutoCorr_tpost(.)
# all empdat
glmAuto_empdatAHconf    <- glmdat_empdatAHconf %>% glm_AutoCorr_tpost(.)


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#--------------   INDIVIDUAL DIFFERENCES AND VISUALISATIONS  -----------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------#
##########
############## Set up data frames 
##########
#-----------------------------------------------------------------------------------#


scatterdat_sim_AHconf <- data.frame(simgen_FP_pol                = filter(simgen_FP_AHconf, post_num_pic==1)$policy,
                                    
                                    simgen_CP_a                  = filter(simgen_CP_AHconf, post_num_pic==1)$a,
                                    simgen_CP_b                  = filter(simgen_CP_AHconf, post_num_pic==1)$b,
                                    simgen_CP_c                  = filter(simgen_CP_AHconf, post_num_pic==1)$c,

                                    simgen_PH_alpha_action       = filter(simgen_PH_AHconf, post_num_pic==1)$alpha_action,

                                    simgen_RL1_alpha_reward      = filter(simgen_RL1_AHconf, post_num_pic ==1)$alpha_reward,
                                    simgen_RL1_cost_constant     = filter(simgen_RL1_AHconf, post_num_pic ==1)$cost_constant,

                                    simgen_RL2_alph_P            = filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_P,
                                    simgen_RL2_alph_N            = filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_N,
                                    simgen_RL2_cost_constant     = filter(simgen_RL2_AHconf, post_num_pic==1)$cost_constant,

                                    simgen_RLH1_alph             = filter(simgen_RLH1_AHconf, post_num_pic==1)$alpha_reward,
                                    simgen_RLH1_cost             = filter(simgen_RLH1_AHconf, post_num_pic==1)$cost_constant,
                                    simgen_RLH1_alpha_action     = filter(simgen_RLH1_AHconf, post_num_pic==1)$alpha_action,
                                    simgen_RLH1_habit_weight     = filter(simgen_RLH1_AHconf, post_num_pic==1)$habit_weight,

                                    simgen_RLH2_alph_P           = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_P,
                                    simgen_RLH2_alph_N           = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_N,
                                    simgen_RLH2_cost_constant    = filter(simgen_RLH2_AHconf, post_num_pic==1)$cost_constant,
                                    simgen_RLH2_alpha_action     = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_action,
                                    simgen_RLH2_habit_weight     = filter(simgen_RLH2_AHconf, post_num_pic==1)$habit_weight,

                                    simgen_RL2_posbias           = filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_P - filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_N,
                                    simgen_RLH2_posbias          = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_P - filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_N,

                                    simgen_RL1_beta_RPE    = as.data.frame(ranef(glmRPE_sim_RL1_AHconf))$condval,
                                    simgen_RLH1_beta_RPE   = as.data.frame(ranef(glmRPE_sim_RLH1_AHconf))$condval,
                                    simgen_RLH2_beta_RPE   = as.data.frame(ranef(glmRPE_sim_RLH2_AHconf))$condval,
                                    
                                    simgen_RLH1_beta_Auto  =  subset(as.data.frame(ranef(glmAuto_sim_RLH1_AHconf)), term == "scaled_lag_log_tpost")$condval
                                    
                                    )


##### now empirical 

scatterdat_emp_AHdisc <- data.frame(emp_FP_pol       = filter(empdat_AHdisc, post_num_pic==1)$policy.fitdat_FP,
                                    
                                    emp_CP_a         = filter(empdat_AHdisc, post_num_pic==1)$a.fitdat_CP,
                                    eemp_CP_b         = filter(empdat_AHdisc, post_num_pic==1)$b.fitdat_CP,
                                    emp_CP_c         = filter(empdat_AHdisc, post_num_pic==1)$c.fitdat_CP,
                                    # 
                                    emp_HP_alpha_action  = filter(empdat_AHdisc, post_num_pic==1)$alpha_action.fitdat_PH,
                                    # 
                                    emp_RL1_alpha_reward = filter(empdat_AHdisc, post_num_pic==1)$alpha.fitdat_RL1, # In the discovery dataset the variables are 'alpha' instead of 'alpha_reward', 'stickiness_weight' instead of 'habit_weight' and 'cost' instead of 'cost_constant'
                                    emp_RL1_cost_constant= filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RL1,
                                        
                                    emp_RL2_alph_P        = filter(empdat_AHdisc, post_num_pic==1)$alpha_P.fitdat_RL2,
                                    emp_RL2_alph_N        = filter(empdat_AHdisc, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RL2_cost_constant = filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RL2,
                                    
                                    emp_RLH1_alpha_reward = filter(empdat_AHdisc, post_num_pic==1)$alpha.fitdat_RLH1,
                                    emp_RLH1_cost_constant= filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RLH1,
                                    emp_RLH1_alpha_action = filter(empdat_AHdisc, post_num_pic==1)$alpha_action.fitdat_RLH1,
                                    emp_RLH1_habit_weight = filter(empdat_AHdisc, post_num_pic==1)$stickiness_weight.fitdat_RLH1,
                                    
                                    emp_RLH2_alph_P        = filter(empdat_AHdisc, post_num_pic==1)$alpha_P.fitdat_RLH2,
                                    emp_RLH2_alph_N        = filter(empdat_AHdisc, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    emp_RLH2_cost_constant = filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RLH2,
                                    emp_RLH2_alpha_action  = filter(empdat_AHdisc, post_num_pic==1)$alpha_action.fitdat_RLH2,
                                    emp_RLH2_habit_weight  = filter(empdat_AHdisc, post_num_pic==1)$stickiness_weight.fitdat_RLH2,
                                    
                                    emp_RL2_posbias = filter(empdat_AHdisc, post_num_pic==1)$alpha_P.fitdat_RL2 - 
                                      filter(empdat_AHdisc, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RLH2_posbias = filter(empdat_AHdisc, post_num_pic==1)$alpha_P.fitdat_RLH2 - 
                                      filter(empdat_AHdisc, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    
                                    empdat_AHscore          = filter(empdat_AHdisc, post_num_pic==1)$Authentic_Happiness_inventory,
                                    empdat_followercount    = filter(empdat_AHdisc, post_num_pic==1)$followers_count,
                                    empdat_mean_tpost_days  = filter(empdat_AHdisc, post_num_pic==1)$mean_tpost,
                                    empdat_mean_likes       = filter(empdat_AHdisc, post_num_pic==1)$mean_likes,
                                    
                                    age              = filter(empdat_AHdisc, post_num_pic==1)$age,
                                    gender           = as.factor(filter(empdat_AHdisc, post_num_pic==1)$gender),
                                    user_num         = filter(empdat_AHdisc, post_num_pic==1)$user_num
                                    )


nPost_AHconf <- empdat_AHconf %>%
  group_by(user_num) %>%
  summarise(nposts = n())

scatterdat_emp_AHconf <- data.frame(emp_FP_pol       = filter(empdat_AHconf, post_num_pic==1)$policy.fitdat_FP,
                                    
                                    emp_CP_a         = filter(empdat_AHconf, post_num_pic==1)$a.fitdat_CP,
                                    emp_CP_b         = filter(empdat_AHconf, post_num_pic==1)$b.fitdat_CP,
                                    emp_CP_c         = filter(empdat_AHconf, post_num_pic==1)$c.fitdat_CP,
                                    # 
                                    emp_HP_alpha_action   = filter(empdat_AHconf, post_num_pic==1)$alpha_action.fitdat_HP,
                                    
                                    emp_RL1_alpha_reward  = filter(empdat_AHconf, post_num_pic==1)$alpha_reward.fitdat_RL1,
                                    emp_RL1_cost_constant = filter(empdat_AHconf, post_num_pic==1)$cost_constant.fitdat_RL1,
                                    
                                    emp_RL2_alph_P        = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RL2,
                                    emp_RL2_alph_N        = filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RL2_cost_constant = filter(empdat_AHconf, post_num_pic==1)$cost_constant.fitdat_RL2,
                                    
                                    emp_RLH1_alpha_reward = filter(empdat_AHconf, post_num_pic==1)$alpha_reward.fitdat_RLH1,
                                    emp_RLH1_cost_conatant= filter(empdat_AHconf, post_num_pic==1)$cost_constant.fitdat_RLH1,
                                    emp_RLH1_alpha_action = filter(empdat_AHconf, post_num_pic==1)$alpha_action.fitdat_RLH1,
                                    emp_RLH1_habit_weight = filter(empdat_AHconf, post_num_pic==1)$habit_weight.fitdat_RLH1,
                                    
                                    emp_RLH2_alph_P       = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RLH2,
                                    emp_RLH2_alph_N       = filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    emp_RLH2_cost_constant= filter(empdat_AHconf, post_num_pic==1)$cost_constant.fitdat_RLH2,
                                    emp_RLH2_alpha_action = filter(empdat_AHconf, post_num_pic==1)$alpha_action.fitdat_RLH2,
                                    emp_RLH2_habit_weight = filter(empdat_AHconf, post_num_pic==1)$habit_weight.fitdat_RLH2,
                                    
                                    emp_RL2_posbias = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RL2 - 
                                      filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RLH2_posbias = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RLH2 - 
                                      filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    
                                    emp_beta_RPE         = as.data.frame(ranef(glmRPE_empdatAHconf))$condval,
                                    emp_beta_Auto        = subset(as.data.frame(ranef(glmAuto_empdatAHconf)), term == "scaled_lag_log_tpost")$condval,

                                    empdat_AHscore          = filter(empdat_AHconf, post_num_pic==1)$Authentic_Happiness_inventory,
                                    empdat_followercount    = filter(empdat_AHconf, post_num_pic==1)$followers_count,
                                    empdat_mean_tpost_days  = filter(empdat_AHconf, post_num_pic==1)$mean_tpost,
                                    empdat_mean_likes       = filter(empdat_AHconf, post_num_pic==1)$mean_likes,
                                    
                                    empdat_nPosts           = nPost_AHconf$nposts,
                                    
                                    age              = filter(empdat_AHconf, post_num_pic==1)$age,
                                    gender           = as.factor(filter(empdat_AHconf, post_num_pic==1)$gender),
                                    user_num         = filter(empdat_AHconf, post_num_pic==1)$user_num
                                    )



#-----------------------------------------------------------------------------------#
##########
############## MAIN PAPER RESULTS
##########
#-----------------------------------------------------------------------------------#

#### general settings
# filter to remove age outliers (as specified in preregistration)
scatterdat_emp_age_AHdisc <- scatterdat_emp_AHdisc %>% filter(age >= 13 & age < 100)
scatterdat_emp_age_AHconf <- scatterdat_emp_AHconf %>% filter(age >= 13 & age < 100)
gender_palette <- c("Male" = "darkblue", "Female" = "darkred")


################
# Figure 1


##################################
##################################
# 1A Example users' observable data
##################################
##################################

### tpost vs. policy
user1_num <- unique(empdat_AHconf$user_num)[7]; # which user you want to plot
user1 <- ggplot(filter(empdat_AHconf, user_num == user1_num)) +
  geom_line(aes(x = post_num_pic, y = t_post), color = color5, linewidth = 1) +
  geom_point(aes(x = post_num_pic, y = likes),
             color = "#FFB347", size = 1.5) +
  xlab("Post Number") +
  ylab("Simulated T_Post") +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),   # remove y-axis numbers
  ) +
  ggtitle(str_c("User ", user1_num))
user2_num <- unique(empdat_AHconf$user_num)[15]; # which user you want to plot
user2 <- ggplot(filter(empdat_AHconf, user_num == user2_num)) +
  geom_line(aes(x = post_num_pic, y = t_post), color = color5, linewidth = 1) +
  geom_point(aes(x = post_num_pic, y = likes, stroke = NA), color = "#FFB347", size = 1.5) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  theme_classic() +
  theme(
    axis.text.y = element_blank(),   # remove y-axis numbers
  ) +
  ggtitle(str_c("User ", user2_num))
user3_num <- unique(empdat_AHconf$user_num)[13]; # which user you want to plot
user3 <- ggplot(filter(empdat_AHconf, user_num == user3_num)) +
  geom_line(aes(x = post_num_pic, y = t_post), color = color5, linewidth = 1) +
  geom_point(aes(x = post_num_pic, y = likes, stroke = NA), color = "#FFB347", size = 1.5) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),   # remove y-axis numbers
  ) +
  ggtitle(str_c("User ", user3_num))

##################################
##################################
# 1C Descriptives
##################################
##################################

AHdisc_age       <- plot_vardist(scatterdat_emp_age_AHdisc, "age", x_limits = c(13,90))
AHdisc_meantpost <- plot_vardist(scatterdat_emp_AHdisc, "empdat_mean_tpost_days", x_limits = c(0, 22))
AHdisc_AHI       <- plot_vardist(scatterdat_emp_AHdisc, "empdat_AHscore")
AHdisc_corr      <- plot_corr_matrix(scatterdat_emp_age_AHdisc,  
                                   c("empdat_followercount", "empdat_AHscore","empdat_mean_tpost_days","age"), 
                                   "Correlations across empirical variables")

AHconf_age       <- plot_vardist(scatterdat_emp_age_AHconf, "age", x_limits = c(13,90))
AHconf_meantpost <- plot_vardist(scatterdat_emp_AHconf,"empdat_mean_tpost_days", x_limits = c(0, 22))
AHconf_AHI       <- plot_vardist(scatterdat_emp_AHconf, "empdat_AHscore")
AHconf_corr      <- plot_corr_matrix(scatterdat_emp_age_AHconf,  
                                   c("empdat_followercount", "empdat_AHscore","empdat_mean_tpost_days","age"), 
                                   "Correlations across empirical variables")


#########
# Arrange Fig 1


observable_dat_fig <- grid.arrange(user1, user2, user3, nrow =3)

descs_fig <- grid.arrange(
  AHdisc_age,
  AHdisc_meantpost,
  AHdisc_AHI,
  AHdisc_corr,
  AHconf_age,
  AHconf_meantpost,
  AHconf_AHI,
  AHconf_corr,
  nrow = 2
)

# Define a layout matrix
Fig1_layout_matrix <- rbind(
  c(1, NA),  # First row: plot 1 spans two columns
  c(NA, NA), # Second row: blank space
  c(2, 2)    # Third row: plot 2 spans both columns
)

# Arrange plots with added space
Fig1 <- grid.arrange(
  observable_dat_fig, descs_fig,
  layout_matrix = Fig1_layout_matrix,
  heights = c(1, 0.2, 1)  # Control relative height of each row (0.2 for the spacer)
)


# save figure
ggsave("figures/Fig1.pdf", plot = Fig1, width = 17, height = 10, units = "in")
# save source data
## Fig1 Source Data
fig1_data <- list(
  a_user1_Tpost = filter(empdat_AHconf, user_num == user1_num)$t_post,
  a_user1_likes = filter(empdat_AHconf, user_num == user1_num)$likes,
  a_user2_Tpost = filter(empdat_AHconf, user_num == user2_num)$t_post,
  a_user2_likes = filter(empdat_AHconf, user_num == user2_num)$likes,
  a_user3_Tpost = filter(empdat_AHconf, user_num == user3_num)$t_post,
  a_user3_likes = filter(empdat_AHconf, user_num == user3_num)$likes,
  c_AHdisc_age                           = scatterdat_emp_age_AHdisc$age,
  d_AHdisc_meantpost                     = scatterdat_emp_AHdisc$empdat_mean_tpost_days,
  e_AHdisc_AHIscore                      = scatterdat_emp_AHdisc$empdat_AHscore,
  f_AHdisc_followers_ageOutliersexcluded = scatterdat_emp_age_AHdisc$empdat_followercount,
  f_AHdisc_meantpost_ageOutliersexcluded = scatterdat_emp_age_AHdisc$empdat_mean_tpost_days,
  f_AHdisc_AHI_ageOutliersexcluded       = scatterdat_emp_age_AHdisc$empdat_AHscore,
  g_AHconf_age                           = scatterdat_emp_age_AHconf$age,
  h_AHconf_meantpost                     = scatterdat_emp_AHconf$empdat_mean_tpost_days,
  i_AHconf_AHIscore                      = scatterdat_emp_AHconf$empdat_AHscore,
  j_AHconf_followers_ageOutliersexcluded = scatterdat_emp_age_AHconf$empdat_followercount,
  j_AHconf_meantpost_ageOutliersexcluded = scatterdat_emp_age_AHconf$empdat_mean_tpost_days,
  j_AHconf_AHI_ageOutliersexcluded       = scatterdat_emp_age_AHconf$empdat_AHscore
  
)
max_len_fig1 <- max(lengths(fig1_data))
fig1_padded <- lapply(fig1_data, function(x) { length(x) <- max_len_fig1; x })
data_fig1   <- as.data.frame(fig1_padded)
write_csv(data_fig1, "source_data/data_fig1.csv")



# Desired final length
target_length <- 1558

# Build data frame with direct padding using length<-
data_fig1 <- data.frame(
  c_AHdisc_age         = `length<-`(scatterdat_emp_age_AHdisc$age, target_length),
  d_AHdisc_mean_tpost  = `length<-`(scatterdat_emp_AHdisc$empdat_mean_tpost_days, target_length),
  e_AHdisc_AHI         = `length<-`(scatterdat_emp_AHdisc$empdat_AHscore, target_length),
  f_AHdisc_followcount = `length<-`(scatterdat_emp_AHdisc$empdat_followercount, target_length),
  g_AHconf_age         = `length<-`(scatterdat_emp_age_AHconf$age, target_length),
  h_AHconf_mean_tpost  = `length<-`(scatterdat_emp_AHconf$empdat_mean_tpost_days, target_length),
  i_AHconf_AHI         = `length<-`(scatterdat_emp_AHconf$empdat_AHscore, target_length),
  j_AHconf_followcount = `length<-`(scatterdat_emp_AHconf$empdat_followercount, target_length)
)

################
# Figure 3: Model Selection

##################################
##################################
# 3A Model Comparison
##################################
##################################

##### AH disc
AHdisc_mean_AICw <- empdat_AHdisc %>% filter(post_num_pic == 1) %>%
  summarise(
    meanAICw_FP   = mean(AICw_FP),
    meanAICw_CP   = mean(AICw_CP),
    meanAICw_HP   = mean(AICw_PH),  ## originally called PH in this dataset
    meanAICw_RL1  = mean(AICw_RL1),
    meanAICw_RL2  = mean(AICw_RL2),
    meanAICw_RLH1 = mean(AICw_RLH1),
    meanAICw_RLH2 = mean(AICw_RLH2),
  )
AHdisc_highest_meanAIC <- names(AHdisc_mean_AICw)[which.max(AHdisc_mean_AICw[1,])]
AHdisc_mean_AICw[AHdisc_highest_meanAIC]

##### AH conf
AHconf_mean_AICw <- empdat_AHconf %>% filter(post_num_pic == 1) %>%
  summarise(
    meanAICw_FP   = mean(AICw_FP),
    meanAICw_CP   = mean(AICw_CP),
    meanAICw_HP   = mean(AICw_HP),
    meanAICw_RL1  = mean(AICw_RL1),
    meanAICw_RL2  = mean(AICw_RL2),
    meanAICw_RLH1 = mean(AICw_RLH1),
    meanAICw_RLH2 = mean(AICw_RLH2),
  )
AHconf_highest_meanAIC <- names(AHconf_mean_AICw)[which.max(AHconf_mean_AICw[1,])]
AHconf_mean_AICw[AHconf_highest_meanAIC]


##################################
##################################
# 3B Model Falsification
##################################
##################################

### settings
glm_colour_palette <- c(  color5, rev(as.data.frame(colour_palette)$colour_palette))

dataset_names_AHconf <- c(str_c("9simgen_FP\n n=", length(unique(simgen_FP_AHconf$user_num))),
                          str_c("8simgen_CP\n n=",length(unique(simgen_CP_AHconf$user_num))),
                          str_c("7simgen_PH\n n=",length(unique(simgen_PH_AHconf$user_num))),
                          str_c("6simgen_RL1\n n=",length(unique(simgen_RL1_AHconf$user_num))),
                          str_c("5simgen_RL2\n n=",length(unique(simgen_RL2_AHconf$user_num))),
                          str_c("4simgen_RLH1\n n=",length(unique(simgen_RLH1_AHconf$user_num))),
                          str_c("3simgen_RLH2\n n=",length(unique(simgen_RLH2_AHconf$user_num))),
                          str_c("2emp_AHconf\n n=",length(unique(empdat_AHconf$user_num)))
                          )

glmRPE_list_AHconf <- list(
  glmRPE_sim_FP_AHconf   = glmRPE_sim_FP_AHconf,
  glmRPE_sim_CP_AHconf   = glmRPE_sim_CP_AHconf,
  glmRPE_sim_PH_AHconf   = glmRPE_sim_PH_AHconf,
  glmRPE_sim_RL1_AHconf  = glmRPE_sim_RL1_AHconf,
  glmRPE_sim_RL2_AHconf  = glmRPE_sim_RL2_AHconf,
  glmRPE_sim_RLH1_AHconf = glmRPE_sim_RLH1_AHconf,
  glmRPE_sim_RLH2_AHconf = glmRPE_sim_RLH2_AHconf, 
  glmRPE_empdatAHconf    = glmRPE_empdatAHconf
)

modfals_fig_prev10 <- plot_glm_forest(glmRPE_list_AHconf, dataset_names_AHconf, 
                                     glm_colour_palette, 
                                     pred_varname = "scaled_lag_RPE", 
                                     dep_varname  = "scaled_delta_logtpost")


##################################
# Stats

summary(glmRPE_sim_FP_AHconf)
effectsize::standardize_parameters(glmRPE_sim_FP_AHconf)
effectsize::standardize_parameters(glmRPE_sim_FP_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_FP_AHconf)$CI_high

summary(glmRPE_sim_CP_AHconf)
effectsize::standardize_parameters(glmRPE_sim_CP_AHconf)
effectsize::standardize_parameters(glmRPE_sim_CP_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_CP_AHconf)$CI_high

summary(glmRPE_sim_PH_AHconf)
effectsize::standardize_parameters(glmRPE_sim_PH_AHconf)
effectsize::standardize_parameters(glmRPE_sim_PH_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_PH_AHconf)$CI_high

summary(glmRPE_sim_RL1_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RL1_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RL1_AHconf)$Std_Coefficient
effectsize::standardize_parameters(glmRPE_sim_RL1_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_RL1_AHconf)$CI_high

summary(glmRPE_sim_RL2_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RL2_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RL2_AHconf)$Std_Coefficient
effectsize::standardize_parameters(glmRPE_sim_RL2_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_RL2_AHconf)$CI_high

summary(glmRPE_sim_RLH1_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RLH1_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RLH1_AHconf)$Std_Coefficient
effectsize::standardize_parameters(glmRPE_sim_RLH1_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_RLH1_AHconf)$CI_high

summary(glmRPE_sim_RLH2_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RLH2_AHconf)
effectsize::standardize_parameters(glmRPE_sim_RLH2_AHconf)$Std_Coefficient
effectsize::standardize_parameters(glmRPE_sim_RLH2_AHconf)$CI_low
effectsize::standardize_parameters(glmRPE_sim_RLH2_AHconf)$CI_high

summary(glmRPE_empdatAHconf)
effectsize::standardize_parameters(glmRPE_empdatAHconf)
effectsize::standardize_parameters(glmRPE_empdatAHconf)$Std_Coefficient
effectsize::standardize_parameters(glmRPE_empdatAHconf)$CI_low
effectsize::standardize_parameters(glmRPE_empdatAHconf)$CI_high

#########
# Arrange Fig 3

# import model comparison figure (which was made in the script '01-model_comparison.R' in this folder)

modcomp_fig <- readRDS("figures/AHconf_justLikes_modcomp.rds") 

# Define a layout matrix
Fig3_layout_matrix <- rbind(
  c(1, NA, 2)
)

# Arrange plots with added space
Fig3 <- grid.arrange(
  modcomp_fig, modfals_fig_prev10, 
  layout_matrix = Fig3_layout_matrix,
  widths = c(1.8, 0.5, 1.1),  # Control relative height of each row (0.2 for the spacer),
  heights = 1
)

ggsave("figures/Fig3.pdf", plot = Fig3, width = 17, height = 8, units = "in")

### Fig3b Source Data
# Note that in the figure, 95% CI is plotted, which can be obtained from the Std. error using the transformation upper / lower CI = beta +/- 1.96 * SE
fig3b_data <- list(
  a_simFP_BetaRPE_fixef_est_prev10      = summary(glmRPE_sim_FP_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simFP_BetaRPE_fixef_stdErr_prev10   = summary(glmRPE_sim_FP_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simCP_BetaRPE_fixef_est_prev10      = summary(glmRPE_sim_CP_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simCP_BetaRPE_fixef_stdErr_prev10   = summary(glmRPE_sim_CP_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simHP_BetaRPE_fixef_est_prev10      = summary(glmRPE_sim_PH_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simHP_BetaRPE_fixef_stdErr_prev10   = summary(glmRPE_sim_PH_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRL1_BetaRPE_fixef_est_prev10     = summary(glmRPE_sim_RL1_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRL1_BetaRPE_fixef_stdErr_prev10  = summary(glmRPE_sim_RL1_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRL2_BetaRPE_fixef_est_prev10     = summary(glmRPE_sim_RL2_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRL2_BetaRPE_fixef_stdErr_prev10  = summary(glmRPE_sim_RL2_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRLH1_BetaRPE_fixef_est_prev10    = summary(glmRPE_sim_RLH1_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRLH1_BetaRPE_fixef_stdErr_prev10 = summary(glmRPE_sim_RLH1_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRLH2_BetaRPE_fixef_est_prev10    = summary(glmRPE_sim_RLH2_AHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRLH2_BetaRPE_fixef_stdErr_prev10 = summary(glmRPE_sim_RLH2_AHconf)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_emp_BetaRPE_fixef_est_prev10    = summary(glmRPE_empdatAHconf)$coefficients["scaled_lag_RPE", "Estimate"],
  a_emp_BetaRPE_fixef_stdErr_prev10 = summary(glmRPE_empdatAHconf)$coefficients["scaled_lag_RPE", "Std. Error"]
  )
max_len_fig3b <- max(lengths(fig3b_data))
fig3b_padded <- lapply(fig3b_data, function(x) { length(x) <- max_len_fig3b; x })
data_fig3b   <- as.data.frame(fig3b_padded)
write_csv(data_fig3b, "source_data/data_fig3b.csv")

##################################
##################################
# Figure 4: Posting Latency
##################################
##################################


##################################
# Stats
AHd_alphR_meantpost_mod <- lm(emp_RLH1_alpha_reward ~ empdat_mean_tpost_days, data = scatterdat_emp_AHdisc)
summary(AHd_alphR_meantpost_mod)
effectsize::standardize_parameters(AHd_alphR_meantpost_mod)$Std_Coefficient
effectsize::standardize_parameters(AHd_alphR_meantpost_mod)$CI_low
effectsize::standardize_parameters(AHd_alphR_meantpost_mod)$CI_high

AHc_habweight_meantpost_mod <- lm(emp_RLH1_habit_weight ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_habweight_meantpost_mod)
effectsize::standardize_parameters(AHc_habweight_meantpost_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_meantpost_mod)$CI_low
effectsize::standardize_parameters(AHc_habweight_meantpost_mod)$CI_high

AHc_alphAc_meantpost_mod <- lm(emp_RLH1_alpha_action ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphAc_meantpost_mod)
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod)$CI_low
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod)$CI_high

AHc_alphR_meantpost_mod <- lm(emp_RLH1_alpha_reward ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphR_meantpost_mod)
effectsize::standardize_parameters(AHc_alphR_meantpost_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphR_meantpost_mod)$CI_low
effectsize::standardize_parameters(AHc_alphR_meantpost_mod)$CI_high


# Meta-analysis
std_AHd <- effectsize::standardize_parameters(AHd_alphR_meantpost_mod)
std_AHc <- effectsize::standardize_parameters(AHc_alphR_meantpost_mod)
meta_alphRLH1_meantpost <- rma(
  yi = c(std_AHd$Std_Coefficient[2],
         std_AHc$Std_Coefficient[2]),
  sei = c(
    (std_AHd$CI_high[2] - std_AHd$CI_low[2]) / (2 * 1.96),
    (std_AHc$CI_high[2] - std_AHc$CI_low[2]) / (2 * 1.96)),
  method = "REML"
)
summary(meta_alphRLH1_meantpost)


## Sensitivity analysis controlling for number of posts
AHc_habweight_meantpost_mod_ctrl <- lm(emp_RLH1_habit_weight ~ empdat_mean_tpost_days + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_habweight_meantpost_mod_ctrl)
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_ctrl)$CI_high

AHc_alphAc_meantpost_mod_ctrl <- lm(emp_RLH1_alpha_action ~ empdat_mean_tpost_days + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_alphAc_meantpost_mod_ctrl)
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_ctrl)$CI_high

AHc_alphR_meantpost_mod_ctrl_nPosts <- lm(emp_RLH1_alpha_reward ~ empdat_mean_tpost_days + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_alphR_meantpost_mod_ctrl_nPosts)
effectsize::standardize_parameters(AHc_alphR_meantpost_mod_ctrl_nPosts)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphR_meantpost_mod_ctrl_nPosts)$CI_low
effectsize::standardize_parameters(AHc_alphR_meantpost_mod_ctrl_nPosts)$CI_high

AHc_alphN_meantpost_mod_ctrl_nPosts <- lm(emp_RLH2_alph_N ~ empdat_mean_tpost_days + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_alphN_meantpost_mod_ctrl_nPosts)
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_ctrl_nPosts)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_ctrl_nPosts)$CI_low
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_ctrl_nPosts)$CI_high


#### Fig 4 in main paper with outliers removed
AHconf_habweight_lims   <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_habit_weight", x_coord_limits = c(0,6), y_coord_limits = c(0, 1))
AHconf_alphAc_lims      <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alpha_action", x_coord_limits = c(0,6), y_coord_limits = c(0, 1))
AHconf_alph_lims        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alpha_reward",x_coord_limits = c(0,6), y_coord_limits = c(0, 1))

#########
# Arrange Fig 4

# Define a layout matrix
Fig4_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3)
)

Fig4 <- grid.arrange(
  AHconf_habweight_lims,
  AHconf_alphAc_lims,
  AHconf_alph_lims,
  layout_matrix = Fig4_layout_matrix,
  widths = c(2.2, 0.5, 2.2, 0.5, 2.2)#,
  )

ggsave("figures/Fig4.pdf", plot = Fig4, width = 20, height = 7, units = "in")

#### Fig4 source data
data_fig4 <- data.frame(
  x_axis_mean_tpost = scatterdat_emp_AHconf$empdat_mean_tpost_days,
  a_habit_weight    = scatterdat_emp_AHconf$emp_RLH1_habit_weight,
  b_alpha_action    = scatterdat_emp_AHconf$emp_RLH1_alpha_action,
  c_alpha_R         = scatterdat_emp_AHconf$emp_RLH1_alpha_reward
)

write_csv(data_fig4, "source_data/data_fig4.csv")


##################################
##################################
# Figure 5: Age, gender and wellbeing
##################################
##################################


##################################
# Stats
# age
AHc_habweight_age_mod <- lm(emp_RLH1_habit_weight ~ age, data = scatterdat_emp_age_AHconf)
summary(AHc_habweight_age_mod)
effectsize::standardize_parameters(AHc_habweight_age_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_age_mod)$CI_low
effectsize::standardize_parameters(AHc_habweight_age_mod)$CI_high

# gender
t_test_result_habweight_gender <- t.test(emp_RLH1_habit_weight ~ gender, data = scatterdat_emp_AHconf)
t_test_result_habweight_gender
effectsize::cohens_d(emp_RLH1_habit_weight ~ gender,
         data = scatterdat_emp_AHconf)$Cohens_d
effectsize::cohens_d(emp_RLH1_habit_weight ~ gender,
                     data = scatterdat_emp_AHconf)$CI_low
effectsize::cohens_d(emp_RLH1_habit_weight ~ gender,
                     data = scatterdat_emp_AHconf)$CI_high


# wellbeing

# 1 / habit weight

AHd_habweight_AHI_mod_quad <- lm(emp_RLH1_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_habweight_AHI_mod_quad)
AHc_habweight_AHI_mod_quad <- lm(emp_RLH1_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_habweight_AHI_mod_quad)
effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad)$CI_low
effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad)$CI_high
# now calculate effect size for quadratic term too
AHc_habweight_AHI_mod_lin <- lm(emp_RLH1_habit_weight ~ empdat_AHscore , data = scatterdat_emp_AHconf)
boot_f2_habweight <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(emp_RLH1_habit_weight ~ empdat_AHscore, data = d)
  mod_quad <- lm(emp_RLH1_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2), data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_conf_habweight <- boot(
  data = scatterdat_emp_AHconf,
  statistic = boot_f2_habweight,
  R = 1000
)
# Point estimate
boot_out_conf_habweight$t0
# 95% percentile CI
boot.ci(boot_out_conf_habweight, type = "perc")

boot_out_disc_habweight <- boot(
  data = scatterdat_emp_AHdisc,
  statistic = boot_f2_habweight,
  R = 1000
)
# Point estimate
boot_out_disc_habweight$t0
# 95% percentile CI
boot.ci(boot_out_disc_habweight, type = "perc")

# 2 / action learning rate

AHd_alphAc_AHI_mod_quad <- lm(emp_RLH1_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_alphAc_AHI_mod_quad)
AHc_alphAc_AHI_mod_quad <- lm(emp_RLH1_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_alphAc_AHI_mod_quad)
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad)$CI_low
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad)$CI_high
# now calculate effect size for quadratic term too
AHc_alphAc_AHI_mod_lin <- lm(emp_RLH1_alpha_action ~ empdat_AHscore , data = scatterdat_emp_AHconf)
boot_f2_alphAc <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(emp_RLH1_alpha_action ~ empdat_AHscore, data = d)
  mod_quad <- lm(emp_RLH1_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2), data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_conf_alphAc <- boot(
  data      = scatterdat_emp_AHconf,
  statistic = boot_f2_alphAc,
  R = 1000
)
# Point estimate
boot_out_conf_alphAc$t0
# 95% percentile CI
boot.ci(boot_out_conf_alphAc, type = "perc")

boot_out_disc_alphAc <- boot(
  data      = scatterdat_emp_AHdisc,
  statistic = boot_f2_alphAc,
  R         = 1000
)
# Point estimate
boot_out_disc_alphAc$t0
# 95% percentile CI
boot.ci(boot_out_disc_alphAc, type = "perc")

# 3 / reward learning rate

AHd_alphR_AHI_mod <- lm(emp_RLH1_alpha_reward ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_alphR_AHI_mod)
AHc_alphR_AHI_mod <- lm(emp_RLH1_alpha_reward ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_alphR_AHI_mod)
effectsize::standardize_parameters(AHc_alphR_AHI_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphR_AHI_mod)$CI_low
effectsize::standardize_parameters(AHc_alphR_AHI_mod)$CI_high

# Meta-analysis
## habit weight
meta_habweight_AHI_linear <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad)$Std_Coefficient[2]
    ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad)$CI_high[2] -  effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad)$CI_high[2] -  effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad)$CI_low[2]) / (2 * 1.96)
    )
)
summary(meta_habweight_AHI_linear)
forest(meta_habweight_AHI_linear)
meta_habweight_AHI_quad <- rma(
  yi = c(
    boot_out_disc_habweight$t0,
    boot_out_conf_habweight$t0
  ),
  sei = c(
    (boot.ci(boot_out_disc_habweight, type = "perc")$percent[5] - boot.ci(boot_out_disc_habweight, type = "perc")$percent[4]) / (2 * 1.96),
    (boot.ci(boot_out_conf_habweight, type = "perc")$percent[5] - boot.ci(boot_out_conf_habweight, type = "perc")$percent[4]) / (2 * 1.96)
  )
)
summary(meta_habweight_AHI_quad)
forest(meta_habweight_AHI_quad)

## action learning rate
meta_alphAc_AHI_linear <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_alphAc_AHI_linear)
forest(meta_alphAc_AHI_linear)

meta_alphAc_AHI_quad <- rma(
  yi = c(
    boot_out_disc_alphAc$t0,
    boot_out_conf_alphAc$t0
  ),
  sei = c(
    (boot.ci(boot_out_disc_alphAc, type = "perc")$percent[5] - boot.ci(boot_out_disc_alphAc, type = "perc")$percent[4]) / (2 * 1.96),
    (boot.ci(boot_out_conf_alphAc, type = "perc")$percent[5] - boot.ci(boot_out_conf_alphAc, type = "perc")$percent[4]) / (2 * 1.96)
  )
)
summary(meta_alphAc_AHI_quad)
forest(meta_alphAc_AHI_quad)


## reward learning rate
meta_alphR_AHI <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphR_AHI_mod)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphR_AHI_mod)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphR_AHI_mod)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphR_AHI_mod)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphR_AHI_mod)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphR_AHI_mod)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_alphR_AHI)
forest(meta_alphR_AHI)


## Sensitivity analysis controlling for number of posts
AHc_habweight_age_mod_ctrl <- lm(emp_RLH1_habit_weight ~ age + empdat_nPosts, data = scatterdat_emp_age_AHconf)
summary(AHc_habweight_age_mod_ctrl)
effectsize::standardize_parameters(AHc_habweight_age_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_age_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_habweight_age_mod_ctrl)$CI_high

AHc_habweight_gender_mod_ctrl <- lm(emp_RLH1_habit_weight ~ gender + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_habweight_gender_mod_ctrl)
effectsize::standardize_parameters(AHc_habweight_gender_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_gender_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_habweight_gender_mod_ctrl)$CI_high

AHc_habweight_AHI_mod_ctrl <- lm(emp_RLH1_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2) + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_habweight_AHI_mod_ctrl)
effectsize::standardize_parameters(AHc_habweight_AHI_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_AHI_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_habweight_AHI_mod_ctrl)$CI_high
# now calculate effect size for quadratic term too
AHc_habweight_AHI_ctrl_mod_lin <- lm(emp_RLH1_habit_weight ~ empdat_AHscore  + empdat_nPosts, data = scatterdat_emp_AHconf)
boot_f2_habweight_AHI_ctrl <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(emp_RLH1_habit_weight ~ empdat_AHscore  + empdat_nPosts, data = d)
  mod_quad <- lm(emp_RLH1_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2) + empdat_nPosts, data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_conf_habweight_AHI_ctrl <- boot(
  data      = scatterdat_emp_AHconf,
  statistic = boot_f2_habweight_AHI_ctrl,
  R = 1000
)
# Point estimate
boot_out_conf_habweight_AHI_ctrl$t0
# 95% percentile CI
boot.ci(boot_out_conf_habweight_AHI_ctrl, type = "perc")


AHc_alphAc_AHI_mod_ctrl <- lm(emp_RLH1_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2)  + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_alphAc_AHI_mod_ctrl)
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_ctrl)$CI_high
# now calculate effect size for quadratic term too
AHc_alphAc_AHI_ctrl_mod_lin <- lm(emp_RLH1_alpha_action ~ empdat_AHscore  + empdat_nPosts, data = scatterdat_emp_AHconf)
boot_f2_alphAc_AHI_ctrl <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(emp_RLH1_alpha_action ~ empdat_AHscore  + empdat_nPosts, data = d)
  mod_quad <- lm(emp_RLH1_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2) + empdat_nPosts, data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_conf_alphAc_AHI_ctrl <- boot(
  data      = scatterdat_emp_AHconf,
  statistic = boot_f2_alphAc_AHI_ctrl,
  R = 1000
)
# Point estimate
boot_out_conf_alphAc_AHI_ctrl$t0
# 95% percentile CI
boot.ci(boot_out_conf_alphAc_AHI_ctrl, type = "perc")


AHc_alphR_AHI_mod_ctrl <- lm(emp_RLH1_alpha_reward ~ empdat_AHscore +  empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_alphR_AHI_mod_ctrl)
effectsize::standardize_parameters(AHc_alphR_AHI_mod_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphR_AHI_mod_ctrl)$CI_low
effectsize::standardize_parameters(AHc_alphR_AHI_mod_ctrl)$CI_high


##################################
# Figure 5
AHconf_habweight_age <- plot_scatter_lm(scatterdat_emp_age_AHconf, xvar = "age", yvar = "emp_RLH1_habit_weight", point_size = 1.15)
AHconf_mean_values     <- scatterdat_emp_AHconf %>%
  group_by(gender) %>%
  summarize(mean_y = mean(emp_RLH1_habit_weight, na.rm = TRUE))
AHconf_habweight_gen <- create_raincloud_plot(scatterdat_emp_AHconf, "gender", "emp_RLH1_habit_weight", AHconf_mean_values, gender_palette)
AHconf_habweight_AH  <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH1_habit_weight", point_size =  0.95)
AHconf_alphAc_AH       <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH1_alpha_action", point_size = 0.95)
AHconf_alphR_AH        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH1_alpha_reward", point_size = 0.95)



#########
# Arrange Fig 5

# Define a layout matrix
Fig5_layout_matrix <- rbind(
  c(1, 1, 2),  # First row: plot 1 spans two columns, plot 2 goes in the third column
  c(NA, NA, NA),
  c(3, 4, 5)   # Second row: plot 3, 4, and 5 each in separate columns
)

# Arrange the plots using grid.arrange with the layout matrix
Fig5 <- grid.arrange(
  AHconf_habweight_age,
  AHconf_habweight_gen,
  AHconf_habweight_AH,
  AHconf_alphAc_AH,
  AHconf_alphR_AH,
  layout_matrix = Fig5_layout_matrix,
  heights = c(1, 0.14, 1)
)

ggsave("figures/Fig5.pdf", plot = Fig5, width = 10, height = 7, units = "in")

#### Fig5 source data
fig5_data <- list(
  a_age                               = scatterdat_emp_age_AHconf$age,
  a_habit_weight_AgeOutliersExcluded  = scatterdat_emp_age_AHconf$emp_RLH1_habit_weight,
  b_gender                            = scatterdat_emp_AHconf$gender,
  b_habit_weight                      = scatterdat_emp_AHconf$emp_RLH1_habit_weight,
  c_AHI                               = scatterdat_emp_AHconf$empdat_AHscore,
  c_alpha_action                      = scatterdat_emp_AHconf$emp_RLH1_alpha_action,
  c_alpha_R                           = scatterdat_emp_AHconf$emp_RLH1_alpha_reward
)
max_len_fig5     <- max(lengths(fig5_data))
fig5_padded <- lapply(fig5_data, function(x) { length(x) <- max_len_fig5; x })
data_fig5   <- as.data.frame(fig5_padded)
write_csv(data_fig5, "source_data/data_fig5.csv")

#-----------------------------------------------------------------------------------#
##########
############## SUPPLEMENTARY RESULTS
##########
#-----------------------------------------------------------------------------------#


##################################
##################################
# Figure S1: Vigour cost constant
##################################
##################################

# Create the data
C_values <- 1:10
T_post <- seq(0.1, 10, length.out = 200)  # Avoid division by zero

data <- expand.grid(C = C_values, T_post = T_post) %>%
  mutate(vigour_cost = C / T_post)

# Generate a brownish-red palette that darkens with C
base_color <- "#A52A2A"  # Brownish red
brown_red_palette <- colorRampPalette(c("#F4C2C2", base_color))(10)

# Plot
FigS1 <- ggplot(data, aes(x = T_post, y = vigour_cost, color = factor(C))) +
  geom_line(size = 1) +
  scale_color_manual(values = brown_red_palette) +
  labs(
    title = "Policy vs. Expected reward per post for different C values",
    x = "Rwd",
    y = "Policy",
    color = "C"
  ) +
  theme_classic()

ggsave("figures/FigS1.pdf", plot = FigS1, width = 7, height = 12, units = "in")


# ** The code for figure S2 can be found in the script 01-model_comparison.R, 
# and the code for figure S3 can be found in the script 02-param_recovery.R, both saved in this folder.

##################################
##################################
# Figure S4: Reward learning behavioural signature across participants
##################################
##################################

################
# Stats

AHc_RPE_habweight_sim_mod <- lm(simgen_RLH1_habit_weight ~ simgen_RLH1_beta_RPE, data = scatterdat_sim_AHconf)
summary(AHc_RPE_habweight_sim_mod )
effectsize::standardize_parameters(AHc_RPE_habweight_sim_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_RPE_habweight_sim_mod)$CI_low
effectsize::standardize_parameters(AHc_RPE_habweight_sim_mod)$CI_high

AHc_RPE_habweight_emp_mod <- lm(emp_RLH1_habit_weight ~ emp_beta_RPE, data = scatterdat_emp_AHconf)
summary(AHc_RPE_habweight_emp_mod)
effectsize::standardize_parameters(AHc_RPE_habweight_emp_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_RPE_habweight_emp_mod)$CI_low
effectsize::standardize_parameters(AHc_RPE_habweight_emp_mod)$CI_high


################
# habweight
# S4a
RPE_habweight_sim_AHconf <- plot_scatter_lm(scatterdat_sim_AHconf, xvar = "simgen_RLH1_beta_RPE", yvar = "simgen_RLH1_habit_weight", y_coord_limits = c(0,1))
# S4b
RPE_habweight_emp_AHconf <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "emp_beta_RPE", yvar = "emp_RLH1_habit_weight",line_colour=color5, y_coord_limits = c(0,1))

#################
# Figure S4

FigS4_layout_matrix <- rbind(
  c(1, NA, 2)  # First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS4 <- grid.arrange(
  RPE_habweight_sim_AHconf,
  RPE_habweight_emp_AHconf,
  nrow=1,
  layout_matrix = FigS4_layout_matrix, 
  widths = c(1, 0.23, 1)
)

ggsave("figures/FigS4.pdf", plot = FigS4, width = 16, height = 7, units = "in")


##################################
##################################
# Figure S5: Relating behavioural signatures to posting latency and age across users
##################################
##################################


################
# Stats

AHc_RPE_meantpost_mod <- lm(empdat_mean_tpost_days ~ emp_beta_RPE + I(emp_beta_RPE^2), data = scatterdat_emp_AHconf)
summary(AHc_RPE_meantpost_mod )
# now calculate effect size for quadratic term too
AHc_RPE_meantpost_mod_lin <- lm(empdat_mean_tpost_days ~ emp_beta_RPE , data = scatterdat_emp_AHconf)
boot_f2_RPE_meantpost <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(empdat_mean_tpost_days ~ emp_beta_RPE, data = d)
  mod_quad <- lm(empdat_mean_tpost_days ~ emp_beta_RPE + I(emp_beta_RPE^2), data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_RPE_meantpost <- boot(
  data = scatterdat_emp_AHconf,
  statistic = boot_f2_RPE_meantpost,
  R = 1000
)
# Point estimate
boot_out_RPE_meantpost$t0
# 95% percentile CI
boot.ci(boot_out_RPE_meantpost, type = "perc")

AHc_RPE_age_mod <- lm(age ~ emp_beta_RPE, data = scatterdat_emp_AHconf)
summary(AHc_RPE_age_mod)
effectsize::standardize_parameters(AHc_RPE_age_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_RPE_age_mod)$CI_low
effectsize::standardize_parameters(AHc_RPE_age_mod)$CI_high

#################
# Figure S5

RPE_meantpost_AHconf <- plot_scatter_quad(scatterdat_emp_AHconf, 
                                     xvar = "emp_beta_RPE", 
                                     yvar = "empdat_mean_tpost_days", 
                                     line_colour = color5,
                                     y_coord_limits = c(0,20))
RPE_age_AHconf <- plot_scatter_lm(scatterdat_emp_age_AHconf,
                                  xvar = "emp_beta_RPE", 
                                  yvar = "age",
                                  line_colour = color5,
                                  y_coord_limits = c(10,80))


FigS5_layout_matrix <- rbind(
  c(1, NA, 2)  # First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS5 <- grid.arrange(
  RPE_meantpost_AHconf,
  RPE_age_AHconf,
  nrow = 1,
  layout_matrix = FigS5_layout_matrix,
  widths = c(1, 0.23, 1)
)

ggsave("figures/FigS5.pdf", plot = FigS5, width = 16, height = 7, units = "in")


#### FigS5 source data
figS5_data <- list(
  a_emp_BetaRPE_ranef  = scatterdat_emp_AHconf$emp_beta_RPE,
  a_emp_mean_tpost     = scatterdat_emp_AHconf$empdat_mean_tpost_days,
  b_emp_BetaRPE_ranef_AgeOutliersExcluded = scatterdat_emp_age_AHconf$emp_beta_RPE,
  b_emp_age            = scatterdat_emp_age_AHconf$age
)
max_len_figS5 <- max(lengths(figS5_data))
figS5_padded <- lapply(figS5_data, function(x) { length(x) <- max_len_figS5; x })
data_figS5   <- as.data.frame(figS5_padded)
write_csv(data_figS5, "source_data/data_figS5.csv")

##################################
##################################
# Figure S6: Model-independent signature of habit
##################################
##################################

## stats
AHc_Auto_habweight_sim_mod <- lm(simgen_RLH1_habit_weight ~ simgen_RLH1_beta_Auto, data = scatterdat_sim_AHconf)
summary(AHc_Auto_habweight_sim_mod)
effectsize::standardize_parameters(AHc_Auto_habweight_sim_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_Auto_habweight_sim_mod)$CI_low
effectsize::standardize_parameters(AHc_Auto_habweight_sim_mod)$CI_high

AHc_Auto_habweight_emp_mod <- lm(emp_RLH1_habit_weight ~ emp_beta_Auto, data = scatterdat_emp_AHconf)
summary(AHc_Auto_habweight_emp_mod)
effectsize::standardize_parameters(AHc_Auto_habweight_emp_mod)$Std_Coefficient
effectsize::standardize_parameters(AHc_Auto_habweight_emp_mod)$CI_low
effectsize::standardize_parameters(AHc_Auto_habweight_emp_mod)$CI_high


# across models
glmAuto_list_AHconf <- list(
  glmAuto_sim_FP_AHconf   = glmAuto_sim_FP_AHconf,
  glmAuto_sim_CP_AHconf   = glmAuto_sim_CP_AHconf,
  glmAuto_sim_PH_AHconf   = glmAuto_sim_PH_AHconf,
  glmAuto_sim_RL1_AHconf  = glmAuto_sim_RL1_AHconf,
  glmAuto_sim_RL2_AHconf  = glmAuto_sim_RL2_AHconf,
  glmAuto_sim_RLH1_AHconf = glmAuto_sim_RLH1_AHconf,
  glmAuto_sim_RLH2_AHconf = glmAuto_sim_RLH2_AHconf, 
  glmAuto_empdatAHconf    = glmAuto_empdatAHconf
)

modfalsAuto_fig <- plot_glm_forest(glmAuto_list_AHconf, dataset_names_AHconf, 
                                    glm_colour_palette, 
                                    pred_varname = "scaled_lag_log_tpost", 
                                    dep_varname  = "scaled_log_tpost")

# across users
Auto_habweight_sim_AHconf <- plot_scatter_lm(scatterdat_sim_AHconf, xvar = "simgen_RLH1_beta_Auto", yvar = "simgen_RLH1_habit_weight", no_xLabel = T, y_coord_limits = c(0,1))
Auto_habweight_emp_AHconf <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "emp_beta_Auto", yvar = "emp_RLH1_habit_weight",line_colour=color5, no_xLabel = T, y_coord_limits = c(0,1))

FigS6_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3)
)

FigS6 <- grid.arrange(
  modfalsAuto_fig,
  Auto_habweight_sim_AHconf,
  Auto_habweight_emp_AHconf,
  nrow=1,
  layout_matrix = FigS6_layout_matrix, 
  widths = c(1.1, 0.23, 1,  0.15, 1)
)

ggsave("figures/FigS6.pdf", plot = FigS6, width = 20, height = 7, units = "in")

#### Source data

figS6_data <- list(
  a_simFP_BetaAuto_fixef_est      = summary(glmAuto_sim_FP_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simFP_BetaAuto_fixef_stdErr   = summary(glmAuto_sim_FP_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_simCP_BetaAuto_fixef_est      = summary(glmAuto_sim_CP_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simCP_BetaAuto_fixef_stdErr   = summary(glmAuto_sim_CP_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_simHP_BetaAuto_fixef_est      = summary(glmAuto_sim_PH_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simHP_BetaAuto_fixef_stdErr   = summary(glmAuto_sim_PH_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_simRL1_BetaAuto_fixef_est     = summary(glmAuto_sim_RL1_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simRL1_BetaAuto_fixef_stdErr  = summary(glmAuto_sim_RL1_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_simRL2_BetaAuto_fixef_est     = summary(glmAuto_sim_RL2_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simRL2_BetaAuto_fixef_stdErr  = summary(glmAuto_sim_RL2_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_simRLH1_BetaAuto_fixef_est    = summary(glmAuto_sim_RLH1_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simRLH1_BetaAuto_fixef_stdErr = summary(glmAuto_sim_RLH1_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_simRLH2_BetaAuto_fixef_est    = summary(glmAuto_sim_RLH2_AHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_simRLH2_BetaAuto_fixef_stdErr = summary(glmAuto_sim_RLH2_AHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  a_emp_BetaAuto_fixef_est        = summary(glmAuto_empdatAHconf)$coefficients["scaled_lag_log_tpost", "Estimate"],
  a_emp_BetaAuto_fixef_stdErr     = summary(glmAuto_empdatAHconf)$coefficients["scaled_lag_log_tpost", "Std. Error"],
  
  b_sim_BetaAuto_ranef            = scatterdat_sim_AHconf$simgen_RLH1_beta_Auto,
  b_sim_BetaAuto_habit_weight     = scatterdat_sim_AHconf$simgen_RLH1_habit_weight,
  c_emp_BetaAuto_ranef            = scatterdat_emp_AHconf$emp_beta_Auto,
  c_emp_BetaAuto_habit_weight     = scatterdat_emp_AHconf$emp_RLH1_habit_weight
  
  )
max_len_figS6 <- max(lengths(figS6_data))
figS6_padded <- lapply(figS6_data, function(x) { length(x) <- max_len_figS6; x })
data_figS6   <- as.data.frame(figS6_padded)
write_csv(data_figS6, "source_data/data_figS6.csv")

# ** The code for figure S7, S8 and S9 can be found in the script 01-model_comparison.R.

##################################
##################################
# Figure S11: Model falsification with different definitions of RPE
##################################
##################################

## reward prediction as previous 5 posts
glmdat_sim_FP_AHconf_prev5   <- mkvars_glm(simgen_FP_AHconf, RPE_type = "prev5")
glmdat_sim_CP_AHconf_prev5   <- mkvars_glm(simgen_CP_AHconf, RPE_type = "prev5")
glmdat_sim_PH_AHconf_prev5   <- mkvars_glm(simgen_PH_AHconf, RPE_type = "prev5")
glmdat_sim_RL1_AHconf_prev5  <- mkvars_glm(simgen_RL1_AHconf, RPE_type = "prev5")
glmdat_sim_RL2_AHconf_prev5  <- mkvars_glm(simgen_RL2_AHconf, RPE_type = "prev5")
glmdat_sim_RLH1_AHconf_prev5 <- mkvars_glm(simgen_RLH1_AHconf, RPE_type = "prev5")
glmdat_sim_RLH2_AHconf_prev5 <- mkvars_glm(simgen_RLH2_AHconf, RPE_type = "prev5")
#### empirical
glmdat_empdatAHconf_prev5    <- mkvars_glm(empdat_AHconf, RPE_type = "prev5")
#### do GLMs
glmRPE_sim_FP_AHconf_prev5   <- glmdat_sim_FP_AHconf_prev5 %>% glm_RPE_deltatpost(.)
glmRPE_sim_CP_AHconf_prev5   <- glmdat_sim_CP_AHconf_prev5 %>% glm_RPE_deltatpost(.)
glmRPE_sim_PH_AHconf_prev5   <- glmdat_sim_PH_AHconf_prev5 %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL1_AHconf_prev5  <- glmdat_sim_RL1_AHconf_prev5 %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL2_AHconf_prev5  <- glmdat_sim_RL2_AHconf_prev5 %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH1_AHconf_prev5 <- glmdat_sim_RLH1_AHconf_prev5 %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH2_AHconf_prev5 <- glmdat_sim_RLH2_AHconf_prev5 %>% glm_RPE_deltatpost(.)
# all empdat
glmRPE_empdatAHconf_prev5    <- glmdat_empdatAHconf_prev5 %>% glm_RPE_deltatpost(.)

glmRPE_list_AHconf_prev5 <- list(
  glmRPE_sim_FP_AHconf   = glmRPE_sim_FP_AHconf_prev5,
  glmRPE_sim_CP_AHconf   = glmRPE_sim_CP_AHconf_prev5,
  glmRPE_sim_PH_AHconf   = glmRPE_sim_PH_AHconf_prev5,
  glmRPE_sim_RL1_AHconf  = glmRPE_sim_RL1_AHconf_prev5,
  glmRPE_sim_RL2_AHconf  = glmRPE_sim_RL2_AHconf_prev5,
  glmRPE_sim_RLH1_AHconf = glmRPE_sim_RLH1_AHconf_prev5,
  glmRPE_sim_RLH2_AHconf = glmRPE_sim_RLH2_AHconf_prev5, 
  glmRPE_empdatAHconf    = glmRPE_empdatAHconf_prev5
)

modfals_fig_prev5 <- plot_glm_forest(glmRPE_list_AHconf_prev5, dataset_names_AHconf, 
                               glm_colour_palette, 
                               pred_varname = "scaled_lag_RPE", 
                               dep_varname  = "scaled_delta_logtpost")
####
## reward prediction as all previous posts
glmdat_sim_FP_AHconf_prevAll   <- mkvars_glm(simgen_FP_AHconf, RPE_type = "prevAll")
glmdat_sim_CP_AHconf_prevAll   <- mkvars_glm(simgen_CP_AHconf, RPE_type = "prevAll")
glmdat_sim_PH_AHconf_prevAll   <- mkvars_glm(simgen_PH_AHconf, RPE_type = "prevAll")
glmdat_sim_RL1_AHconf_prevAll  <- mkvars_glm(simgen_RL1_AHconf, RPE_type = "prevAll")
glmdat_sim_RL2_AHconf_prevAll  <- mkvars_glm(simgen_RL2_AHconf, RPE_type = "prevAll")
glmdat_sim_RLH1_AHconf_prevAll <- mkvars_glm(simgen_RLH1_AHconf, RPE_type = "prevAll")
glmdat_sim_RLH2_AHconf_prevAll <- mkvars_glm(simgen_RLH2_AHconf, RPE_type = "prevAll")
#### empirical
glmdat_empdatAHconf_prevAll    <- mkvars_glm(empdat_AHconf, RPE_type = "prevAll")
#### do GLMs
glmRPE_sim_FP_AHconf_prevAll   <- glmdat_sim_FP_AHconf_prevAll %>% glm_RPE_deltatpost(.)
glmRPE_sim_CP_AHconf_prevAll   <- glmdat_sim_CP_AHconf_prevAll %>% glm_RPE_deltatpost(.)
glmRPE_sim_PH_AHconf_prevAll   <- glmdat_sim_PH_AHconf_prevAll %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL1_AHconf_prevAll  <- glmdat_sim_RL1_AHconf_prevAll %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL2_AHconf_prevAll  <- glmdat_sim_RL2_AHconf_prevAll %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH1_AHconf_prevAll <- glmdat_sim_RLH1_AHconf_prevAll %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH2_AHconf_prevAll <- glmdat_sim_RLH2_AHconf_prevAll %>% glm_RPE_deltatpost(.)
# all empdat
glmRPE_empdatAHconf_prevAll    <- glmdat_empdatAHconf_prevAll %>% glm_RPE_deltatpost(.)

glmRPE_list_AHconf_prevAll <- list(
  glmRPE_sim_FP_AHconf   = glmRPE_sim_FP_AHconf_prevAll,
  glmRPE_sim_CP_AHconf   = glmRPE_sim_CP_AHconf_prevAll,
  glmRPE_sim_PH_AHconf   = glmRPE_sim_PH_AHconf_prevAll,
  glmRPE_sim_RL1_AHconf  = glmRPE_sim_RL1_AHconf_prevAll,
  glmRPE_sim_RL2_AHconf  = glmRPE_sim_RL2_AHconf_prevAll,
  glmRPE_sim_RLH1_AHconf = glmRPE_sim_RLH1_AHconf_prevAll,
  glmRPE_sim_RLH2_AHconf = glmRPE_sim_RLH2_AHconf_prevAll, 
  glmRPE_empdatAHconf    = glmRPE_empdatAHconf_prevAll
)

modfals_fig_prevAll <- plot_glm_forest(glmRPE_list_AHconf_prevAll, dataset_names_AHconf, 
                                     glm_colour_palette, 
                                     pred_varname = "scaled_lag_RPE", 
                                     dep_varname  = "scaled_delta_logtpost")


#########
# Arrange Fig S11
# Arrange plots with added space 
FigS11a <- grid.arrange(
  modcomp_fig, modfals_fig_prevAll, 
  layout_matrix = Fig3_layout_matrix,
  widths = c(1.8, 0.5, 1.1),  # Control relative height of each row (0.2 for the spacer),
  heights = 1
)

# Arrange plots with added space
FigS11b <- grid.arrange(
  modcomp_fig, modfals_fig_prev5, 
  layout_matrix = Fig3_layout_matrix,
  widths = c(1.8, 0.5, 1.1),  # Control relative height of each row (0.2 for the spacer),
  heights = 1
)

ggsave("figures/FigS11a.pdf", plot = FigS11a, width = 17, height = 8, units = "in")
ggsave("figures/FigS11b.pdf", plot = FigS11b, width = 17, height = 8, units = "in")


#### Source data

figS11_data <- list(
  a_simFP_BetaRPE_fixef_est_prevAll      = summary(glmRPE_sim_FP_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simFP_BetaRPE_fixef_stdErr_prevAll   = summary(glmRPE_sim_FP_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simCP_BetaRPE_fixef_est_prevAll      = summary(glmRPE_sim_CP_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simCP_BetaRPE_fixef_stdErr_prevAll   = summary(glmRPE_sim_CP_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simHP_BetaRPE_fixef_est_prevAll      = summary(glmRPE_sim_PH_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simHP_BetaRPE_fixef_stdErr_prevAll   = summary(glmRPE_sim_PH_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRL1_BetaRPE_fixef_est_prevAll     = summary(glmRPE_sim_RL1_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRL1_BetaRPE_fixef_stdErr_prevAll  = summary(glmRPE_sim_RL1_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRL2_BetaRPE_fixef_est_prevAll     = summary(glmRPE_sim_RL2_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRL2_BetaRPE_fixef_stdErr_prevAll  = summary(glmRPE_sim_RL2_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRLH1_BetaRPE_fixef_est_prevAll    = summary(glmRPE_sim_RLH1_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRLH1_BetaRPE_fixef_stdErr_prevAll = summary(glmRPE_sim_RLH1_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_simRLH2_BetaRPE_fixef_est_prevAll    = summary(glmRPE_sim_RLH2_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_simRLH2_BetaRPE_fixef_stdErr_prevAll = summary(glmRPE_sim_RLH2_AHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  a_emp_BetaRPE_fixef_est_prevAll    = summary(glmRPE_empdatAHconf_prevAll)$coefficients["scaled_lag_RPE", "Estimate"],
  a_emp_BetaRPE_fixef_stdErr_prevAll = summary(glmRPE_empdatAHconf_prevAll)$coefficients["scaled_lag_RPE", "Std. Error"],
  
  b_simFP_BetaRPE_fixef_est_prev5      = summary(glmRPE_sim_FP_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simFP_BetaRPE_fixef_stdErr_prev5   = summary(glmRPE_sim_FP_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_simCP_BetaRPE_fixef_est_prev5      = summary(glmRPE_sim_CP_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simCP_BetaRPE_fixef_stdErr_prev5   = summary(glmRPE_sim_CP_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_simHP_BetaRPE_fixef_est_prev5      = summary(glmRPE_sim_PH_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simHP_BetaRPE_fixef_stdErr_prev5   = summary(glmRPE_sim_PH_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_simRL1_BetaRPE_fixef_est_prev5     = summary(glmRPE_sim_RL1_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simRL1_BetaRPE_fixef_stdErr_prev5  = summary(glmRPE_sim_RL1_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_simRL2_BetaRPE_fixef_est_prev5     = summary(glmRPE_sim_RL2_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simRL2_BetaRPE_fixef_stdErr_prev5  = summary(glmRPE_sim_RL2_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_simRLH1_BetaRPE_fixef_est_prev5    = summary(glmRPE_sim_RLH1_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simRLH1_BetaRPE_fixef_stdErr_prev5 = summary(glmRPE_sim_RLH1_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_simRLH2_BetaRPE_fixef_est_prev5    = summary(glmRPE_sim_RLH2_AHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_simRLH2_BetaRPE_fixef_stdErr_prev5 = summary(glmRPE_sim_RLH2_AHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"],
  b_emp_BetaRPE_fixef_est_prev5    = summary(glmRPE_empdatAHconf_prev5)$coefficients["scaled_lag_RPE", "Estimate"],
  b_emp_BetaRPE_fixef_stdErr_prev5 = summary(glmRPE_empdatAHconf_prev5)$coefficients["scaled_lag_RPE", "Std. Error"]
  
)
max_len_figS11 <- max(lengths(figS11_data))
figS11_padded <- lapply(figS11_data, function(x) { length(x) <- max_len_figS11; x })
data_figS11   <- as.data.frame(figS11_padded)
write_csv(data_figS11, "source_data/data_figS11.csv")

##################################
##################################
# Figure S12: RLH2 Posting latency results
##################################
##################################

################
# Stats

AHc_t_test_result_posbiasRLH2 <- t.test(scatterdat_emp_AHconf$emp_RLH2_alph_P, scatterdat_emp_AHconf$emp_RLH2_alph_N, paired = TRUE)
AHc_t_test_result_posbiasRLH2
effectsize::cohens_d(scatterdat_emp_AHconf$emp_RLH2_alph_P, scatterdat_emp_AHconf$emp_RLH2_alph_N, paired = TRUE)$Cohens_d
effectsize::cohens_d(scatterdat_emp_AHconf$emp_RLH2_alph_P, scatterdat_emp_AHconf$emp_RLH2_alph_N, paired = TRUE)$CI_low
effectsize::cohens_d(scatterdat_emp_AHconf$emp_RLH2_alph_P, scatterdat_emp_AHconf$emp_RLH2_alph_N, paired = TRUE)$CI_high

AHc_habweight_meantpost_mod_RLH2 <- lm(emp_RLH2_habit_weight ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_habweight_meantpost_mod_RLH2)
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_RLH2)$CI_high

AHc_alphAc_meantpost_mod_RLH2 <- lm(emp_RLH2_alpha_action ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphAc_meantpost_mod_RLH2)
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_RLH2)$CI_high

Ahd_posbias_meantpost_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_mean_tpost_days, data = scatterdat_emp_AHdisc)
summary(Ahd_posbias_meantpost_mod_RLH2)
effectsize::standardize_parameters(Ahd_posbias_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(Ahd_posbias_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(Ahd_posbias_meantpost_mod_RLH2)$CI_high

Ahc_posbias_meantpost_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(Ahc_posbias_meantpost_mod_RLH2)
effectsize::standardize_parameters(Ahc_posbias_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(Ahc_posbias_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(Ahc_posbias_meantpost_mod_RLH2)$CI_high

AHd_alphP_meantpost_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_mean_tpost_days, data = scatterdat_emp_AHdisc)
summary(AHd_alphP_meantpost_mod_RLH2)
effectsize::standardize_parameters(AHd_alphP_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHd_alphP_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHd_alphP_meantpost_mod_RLH2)$CI_high

AHc_alphP_meantpost_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphP_meantpost_mod_RLH2)
effectsize::standardize_parameters(AHc_alphP_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphP_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_alphP_meantpost_mod_RLH2)$CI_high

AHc_alphN_meantpost_mod_RLH2 <- lm(emp_RLH2_alph_N ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphN_meantpost_mod_RLH2)
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_RLH2)$CI_high

# control for number of posts
AHc_alphN_meantpost_mod_RLH2_ctrl <- lm(emp_RLH2_alph_N ~ empdat_mean_tpost_days + empdat_nPosts, data = scatterdat_emp_AHconf)
summary(AHc_alphN_meantpost_mod_RLH2_ctrl) 
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_RLH2_ctrl)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_RLH2_ctrl)$CI_low
effectsize::standardize_parameters(AHc_alphN_meantpost_mod_RLH2_ctrl)$CI_high



## reward learning rate
meta_meantpost_alphP_RLH2 <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphP_meantpost_mod_RLH2)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphP_meantpost_mod_RLH2)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphP_meantpost_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphP_meantpost_mod_RLH2)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphP_meantpost_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphP_meantpost_mod_RLH2)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_meantpost_alphP_RLH2)
forest(meta_meantpost_alphP_RLH2)

### posbias meta analysis
meta_meantpost_posbias_RLH2 <- rma(
  yi  = c(coef(Ahd_posbias_meantpost_mod_RLH2)["empdat_mean_tpost_days"][[1]], coef(Ahc_posbias_meantpost_mod_RLH2)["empdat_mean_tpost_days"][[1]]),
  sei = c(summary(Ahd_posbias_meantpost_mod_RLH2)$coefficients["empdat_mean_tpost_days", "Std. Error"], summary(Ahc_posbias_meantpost_mod_RLH2)$coefficients["empdat_mean_tpost_days", "Std. Error"])
)
summary(meta_meantpost_posbias_RLH2)
forest(meta_meantpost_posbias_RLH2)


################
# Figure S12

RLH2_df <- pivot_longer(scatterdat_emp_AHconf, 
                        cols = c(emp_RLH2_alph_P, emp_RLH2_alph_N),
                        names_to = "Variable", 
                        values_to = "Value")
RLH2posbias_palette <- c("emp_RLH2_alph_P" = color4, "emp_RLH2_alph_N" = color4)
AHconf_posbias <- plot_violin(RLH2_df,"Variable", "Value", fillvar = "Variable", palette = RLH2posbias_palette  )
AHconf_meantpost_habweight_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_habit_weight", y_coord_limits = c(0,1))
AHconf_meantpost_alphAc_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_alpha_action", y_coord_limits = c(0,1))
AHconf_meantpost_posbias_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_posbias", y_coord_limits = c(-1,1))
AHconf_meantpost_alphN_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_alph_N", y_coord_limits = c(0,1))
AHconf_meantpost_alphP_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_alph_P", y_coord_limits = c(0,1))


FigS12_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3),
  c(NA, NA, NA, NA, NA),
  c(4, NA, 5, NA, 6)
)

FigS12 <- grid.arrange(
  AHconf_posbias, 
  AHconf_meantpost_habweight_RLH2, 
  AHconf_meantpost_alphAc_RLH2,
  AHconf_meantpost_posbias_RLH2,
  AHconf_meantpost_alphP_RLH2, 
  AHconf_meantpost_alphN_RLH2,
  layout_matrix = FigS12_layout_matrix, 
  widths = c(1, 0.23, 1, 0.23, 1), 
  heights = c(1, 0.3, 1)
)

ggsave("figures/FigS12.pdf", plot = FigS12, width = 18, height = 12, units = "in")

#### FigS12 source data
data_figS12 <- data.frame(
  a_RLH2_alpha_N    = scatterdat_emp_AHconf$emp_RLH2_alph_N,
  a_RLH2_alpha_P    = scatterdat_emp_AHconf$emp_RLH2_alph_P,
  b_RLH2_mean_tpost = scatterdat_emp_AHconf$empdat_mean_tpost_days,
  b_RLH2_habit_weight = scatterdat_emp_AHconf$emp_RLH2_habit_weight,
  c_RLH2_alpha_action = scatterdat_emp_AHconf$emp_RLH2_alpha_action,
  d_RLH2_alpha_action = scatterdat_emp_AHconf$emp_RLH2_posbias
)
write_csv(data_figS12, "source_data/data_figS12.csv")

##################################
##################################
# Figure S13: RLH2 Age and gender results
##################################
##################################

##################################
# Stats
AHc_habweight_age_mod_RLH2 <- lm(emp_RLH2_habit_weight ~ age, data = scatterdat_emp_age_AHconf)
summary(AHc_habweight_age_mod_RLH2)
effectsize::standardize_parameters(AHc_habweight_age_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_age_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_habweight_age_mod_RLH2)$CI_high


t_test_result_habweight_gender_RLH2 <- t.test(emp_RLH2_habit_weight ~ gender, data = scatterdat_emp_AHconf)
t_test_result_habweight_gender_RLH2
effectsize::cohens_d(emp_RLH2_habit_weight ~ gender,
                     data = scatterdat_emp_AHconf)$Cohens_d
effectsize::cohens_d(emp_RLH2_habit_weight ~ gender,
                     data = scatterdat_emp_AHconf)$CI_low
effectsize::cohens_d(emp_RLH2_habit_weight ~ gender,
                     data = scatterdat_emp_AHconf)$CI_high




##################################
# Figure S13
AHconf_habweight_age_RLH2 <- plot_scatter_lm(scatterdat_emp_age_AHconf, xvar = "age", yvar = "emp_RLH2_habit_weight")
AHconf_mean_values_RLH2 <- scatterdat_emp_AHconf %>%
  group_by(gender) %>%
  summarize(mean_y = mean(emp_RLH2_habit_weight, na.rm = TRUE))
AHconf_habweight_gen_RLH2 <- create_raincloud_plot(scatterdat_emp_AHconf, "gender", "emp_RLH2_habit_weight", AHconf_mean_values_RLH2, gender_palette)


FigS13_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3)
)

FigS13 <- grid.arrange(
  AHconf_habweight_age_RLH2,
  AHconf_habweight_gen_RLH2,
  layout_matrix = FigS12_layout_matrix, 
  widths = c(1.6, 0.23, 1)
)
ggsave("figures/FigS13.pdf", plot = FigS13, width = 18, height = 7, units = "in")

#### FigS13 source data
figS13_data <- list(
  a_emp_age            = scatterdat_emp_age_AHconf$age,
  a_RLH2_habit_weight_AgeOutliersExcluded = scatterdat_emp_age_AHconf$emp_RLH2_habit_weight,
  b_RLH2_habit_weight = scatterdat_emp_AHconf$emp_RLH2_habit_weight,
  b_gender            = scatterdat_emp_AHconf$gender
  
)
max_len_figS13 <- max(lengths(figS13_data))
figS13_padded <- lapply(figS13_data, function(x) { length(x) <- max_len_figS13; x })
data_figS13   <- as.data.frame(figS13_padded)
write_csv(data_figS13, "source_data/data_figS13.csv")

##################################
# Stats
AHd_habweight_AHI_mod_quad_RLH2 <- lm(emp_RLH2_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_habweight_AHI_mod_quad_RLH2)
effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad_RLH2)$CI_low
effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad_RLH2)$CI_high

AHc_habweight_AHI_mod_quad_RLH2 <- lm(emp_RLH2_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_habweight_AHI_mod_quad_RLH2)
effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad_RLH2)$CI_low
effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad_RLH2)$CI_high

# now calculate effect size for quadratic term too
AHc_habweight_AHI_mod_lin_RLH2 <- lm(emp_RLH2_habit_weight ~ empdat_AHscore , data = scatterdat_emp_AHconf)
boot_f2_habweight_RLH2 <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(emp_RLH2_habit_weight ~ empdat_AHscore, data = d)
  mod_quad <- lm(emp_RLH2_habit_weight ~ empdat_AHscore + I(empdat_AHscore^2), data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_conf_habweight_RLH2 <- boot(
  data = scatterdat_emp_AHconf,
  statistic = boot_f2_habweight_RLH2,
  R = 1000
)
# Point estimate
boot_out_conf_habweight_RLH2$t0
# 95% percentile CI
boot.ci(boot_out_conf_habweight_RLH2, type = "perc")

boot_out_disc_habweight_RLH2 <- boot(
  data = scatterdat_emp_AHdisc,
  statistic = boot_f2_habweight_RLH2,
  R = 1000
)
# Point estimate
boot_out_disc_habweight_RLH2$t0
# 95% percentile CI
boot.ci(boot_out_disc_habweight_RLH2, type = "perc")

AHd_alphAc_AHI_mod_quad_RLH2 <- lm(emp_RLH2_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_alphAc_AHI_mod_quad_RLH2)
effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad_RLH2)$CI_low
effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad_RLH2)$CI_high

AHc_alphAc_AHI_mod_quad_RLH2 <- lm(emp_RLH2_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_alphAc_AHI_mod_quad_RLH2)
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad_RLH2)$CI_low
effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad_RLH2)$CI_high

# now calculate effect size for quadratic term too
AHc_alphAc_AHI_mod_lin_RLH2 <- lm(emp_RLH2_alpha_action ~ empdat_AHscore , data = scatterdat_emp_AHconf)
boot_f2_alphAc_RLH2 <- function(data, indices) {
  
  d <- data[indices, ]
  
  mod_lin  <- lm(emp_RLH2_alpha_action ~ empdat_AHscore, data = d)
  mod_quad <- lm(emp_RLH2_alpha_action ~ empdat_AHscore + I(empdat_AHscore^2), data = d)
  
  get_f2(mod_quad, mod_lin)
}
boot_out_conf_alphAc_RLH2 <- boot(
  data = scatterdat_emp_AHconf,
  statistic = boot_f2_alphAc_RLH2,
  R = 1000
)
# Point estimate
boot_out_conf_alphAc_RLH2$t0
# 95% percentile CI
boot.ci(boot_out_conf_alphAc_RLH2, type = "perc")

boot_out_disc_alphAc_RLH2 <- boot(
  data = scatterdat_emp_AHdisc,
  statistic = boot_f2_alphAc_RLH2,
  R = 1000
)
# Point estimate
boot_out_disc_alphAc_RLH2$t0
# 95% percentile CI
boot.ci(boot_out_disc_alphAc_RLH2, type = "perc")


AHd_alphP_AHI_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_alphP_AHI_mod_RLH2)
effectsize::standardize_parameters(AHd_alphP_AHI_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHd_alphP_AHI_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHd_alphP_AHI_mod_RLH2)$CI_high

AHc_alphP_AHI_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_alphP_AHI_mod_RLH2)
effectsize::standardize_parameters(AHc_alphP_AHI_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphP_AHI_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_alphP_AHI_mod_RLH2)$CI_high

AHd_alphN_AHI_mod_RLH2 <- lm(emp_RLH2_alph_N ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_alphN_AHI_mod_RLH2)
effectsize::standardize_parameters(AHd_alphN_AHI_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHd_alphN_AHI_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHd_alphN_AHI_mod_RLH2)$CI_high

AHc_alphN_AHI_mod_RLH2 <- lm(emp_RLH2_alph_N ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_alphN_AHI_mod_RLH2)
effectsize::standardize_parameters(AHc_alphN_AHI_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphN_AHI_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_alphN_AHI_mod_RLH2)$CI_high

AHd_posbias_AHI_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_posbias_AHI_mod_RLH2)
effectsize::standardize_parameters(AHd_posbias_AHI_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHd_posbias_AHI_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHd_posbias_AHI_mod_RLH2)$CI_high

AHc_posbias_AHI_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_posbias_AHI_mod_RLH2)
effectsize::standardize_parameters(AHc_posbias_AHI_mod_RLH2)$Std_Coefficient
effectsize::standardize_parameters(AHc_posbias_AHI_mod_RLH2)$CI_low
effectsize::standardize_parameters(AHc_posbias_AHI_mod_RLH2)$CI_high

# Meta-analysis
## habit weight
meta_habweight_AHI_linear_RLH2 <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad_RLH2)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad_RLH2)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHd_habweight_AHI_mod_quad_RLH2)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHc_habweight_AHI_mod_quad_RLH2)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_habweight_AHI_linear_RLH2)
forest(meta_habweight_AHI_linear_RLH2)
meta_habweight_AHI_quad_RLH2 <- rma(
  yi = c(
    boot_out_disc_habweight_RLH2$t0,
    boot_out_conf_habweight_RLH2$t0
  ),
  sei = c(
    (boot.ci(boot_out_disc_habweight_RLH2, type = "perc")$percent[5] - boot.ci(boot_out_disc_habweight_RLH2, type = "perc")$percent[4]) / (2 * 1.96),
    (boot.ci(boot_out_conf_habweight_RLH2, type = "perc")$percent[5] - boot.ci(boot_out_conf_habweight_RLH2, type = "perc")$percent[4]) / (2 * 1.96)
  )
)
summary(meta_habweight_AHI_quad_RLH2)
forest(meta_habweight_AHI_quad_RLH2)


## alphAc

meta_alphAc_AHI_linear_RLH2 <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad_RLH2)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad_RLH2)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphAc_AHI_mod_quad_RLH2)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphAc_AHI_mod_quad_RLH2)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_alphAc_AHI_linear_RLH2)
forest(meta_alphAc_AHI_linear_RLH2)

meta_alphAc_AHI_quad_RLH2 <- rma(
  yi = c(
    boot_out_disc_alphAc_RLH2$t0,
    boot_out_conf_alphAc_RLH2$t0
  ),
  sei = c(
    (boot.ci(boot_out_disc_alphAc_RLH2, type = "perc")$percent[5] - boot.ci(boot_out_disc_alphAc_RLH2, type = "perc")$percent[4]) / (2 * 1.96),
    (boot.ci(boot_out_conf_alphAc_RLH2, type = "perc")$percent[5] - boot.ci(boot_out_conf_alphAc_RLH2, type = "perc")$percent[4]) / (2 * 1.96)
  )
)
summary(meta_alphAc_AHI_quad_RLH2)
forest(meta_alphAc_AHI_quad_RLH2)


# alpha P

meta_alphP_AHI_RLH2 <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphP_AHI_mod_RLH2)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphP_AHI_mod_RLH2)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphP_AHI_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphP_AHI_mod_RLH2)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphP_AHI_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphP_AHI_mod_RLH2)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_alphP_AHI_RLH2)
forest(meta_alphP_AHI_RLH2)


# alpha N

meta_alphN_AHI_RLH2 <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphN_AHI_mod_RLH2)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphN_AHI_mod_RLH2)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphN_AHI_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphN_AHI_mod_RLH2)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphN_AHI_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphN_AHI_mod_RLH2)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_alphN_AHI_RLH2)
forest(meta_alphN_AHI_RLH2)


# posbias 

meta_posbias_AHI_RLH2 <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_posbias_AHI_mod_RLH2)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_posbias_AHI_mod_RLH2)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_posbias_AHI_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHd_posbias_AHI_mod_RLH2)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_posbias_AHI_mod_RLH2)$CI_high[2] -  effectsize::standardize_parameters(AHc_posbias_AHI_mod_RLH2)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_posbias_AHI_RLH2)
forest(meta_posbias_AHI_RLH2)


##################################
# Figure S14

AHconf_habweight_AHI_RLH2  <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_habit_weight")
AHconf_alphAc_AHI_RLH2       <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_alpha_action")
AHconf_alphP_AHI_RLH2        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_alph_P")
AHconf_alphN_AHI_RLH2        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_alph_N")
AHconf_posbias_AHI_RLH2      <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_posbias")


FigS14_layout_matrix <- rbind(
  c(1, NA, 2, NA, NA),
  c(NA, NA, NA, NA, NA),
  c(3, NA, 4, NA, 5)
)

FigS14 <- grid.arrange(
  AHconf_habweight_AHI_RLH2,
  AHconf_alphAc_AHI_RLH2,
  AHconf_alphP_AHI_RLH2,
  AHconf_alphN_AHI_RLH2,
  AHconf_posbias_AHI_RLH2,
  
  layout_matrix = FigS14_layout_matrix, 
  heights = c(1, 0.23, 1),
  widths = c(1, 0.23, 1, 0.23, 1)
)
ggsave("figures/FigS14.pdf", plot = FigS14, width = 12.5, height = 7, units = "in")

#### FigS13 source data
data_figS14 <- data.frame(
  x_axis_emp_AHI      = scatterdat_emp_AHconf$empdat_AHscore,
  a_RLH2_habit_weight = scatterdat_emp_AHconf$emp_RLH2_habit_weight,
  b_RLH2_alpha_action = scatterdat_emp_AHconf$emp_RLH2_alpha_action,
  c_RLH2_alpha_P = scatterdat_emp_AHconf$emp_RLH2_alph_P,
  d_RLH2_alpha_N = scatterdat_emp_AHconf$emp_RLH2_alph_N,
  e_RLH2_posbias = scatterdat_emp_AHconf$emp_RLH2_posbias
  
)
write_csv(data_figS14, "source_data/data_figS14.csv")

##################################
##################################
# Figure S15: Posting latency correlations with outliers removed
##################################
##################################

# filter to remove posting latency outliers
## define outliers as 1.5 * interquartile range above or below upper and lower quartiles
## AHdisc
Q1_AHd <- quantile(scatterdat_emp_AHconf$empdat_mean_tpost_days, 0.25)
Q3_AHd <- quantile(scatterdat_emp_AHconf$empdat_mean_tpost_days, 0.75)
lower_bound_AHd <- Q1_AHd - 1.5 * IQR(scatterdat_emp_AHdisc$empdat_mean_tpost_days)
upper_bound_AHd <- Q3_AHd + 1.5 * IQR(scatterdat_emp_AHdisc$empdat_mean_tpost_days)
scatterdat_emp_AHd_tpostOutRemoved <- filter(scatterdat_emp_AHdisc,
                                                  empdat_mean_tpost_days > lower_bound_AHd & 
                                                    empdat_mean_tpost_days < upper_bound_AHd)
## AHconf
Q1_AHc <- quantile(scatterdat_emp_AHconf$empdat_mean_tpost_days, 0.25)
Q3_AHc <- quantile(scatterdat_emp_AHconf$empdat_mean_tpost_days, 0.75)
lower_bound_AHc <- Q1_AHc - 1.5 * IQR(scatterdat_emp_AHconf$empdat_mean_tpost_days)
upper_bound_AHc <- Q3_AHc + 1.5 * IQR(scatterdat_emp_AHconf$empdat_mean_tpost_days)
scatterdat_emp_AHc_tpostOutRemoved <- filter(scatterdat_emp_AHconf,
                                              empdat_mean_tpost_days > lower_bound_AHc & 
                                                empdat_mean_tpost_days < upper_bound_AHc)

##################################
# Stats
AHc_habweight_meantpost_mod_OutRemoved <- lm(emp_RLH1_habit_weight ~ empdat_mean_tpost_days, data = scatterdat_emp_AHc_tpostOutRemoved)
summary(AHc_habweight_meantpost_mod_OutRemoved)
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_OutRemoved)$Std_Coefficient
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_OutRemoved)$CI_low
effectsize::standardize_parameters(AHc_habweight_meantpost_mod_OutRemoved)$CI_high

AHc_alphAc_meantpost_mod_OutRemoved <- lm(emp_RLH1_alpha_action ~ empdat_mean_tpost_days, data = scatterdat_emp_AHc_tpostOutRemoved)
summary(AHc_alphAc_meantpost_mod_OutRemoved)
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_OutRemoved)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_OutRemoved)$CI_low
effectsize::standardize_parameters(AHc_alphAc_meantpost_mod_OutRemoved)$CI_high

AHd_alphR_meantpost_mod_OutRemoved <- lm(emp_RLH1_alpha_reward ~ empdat_mean_tpost_days, data = scatterdat_emp_AHd_tpostOutRemoved)
summary(AHd_alphR_meantpost_mod_OutRemoved)
effectsize::standardize_parameters(AHd_alphR_meantpost_mod_OutRemoved)$Std_Coefficient
effectsize::standardize_parameters(AHd_alphR_meantpost_mod_OutRemoved)$CI_low
effectsize::standardize_parameters(AHd_alphR_meantpost_mod_OutRemoved)$CI_high

AHc_alphR_meantpost_mod_OutRemoved <- lm(emp_RLH1_alpha_reward ~ empdat_mean_tpost_days, data = scatterdat_emp_AHc_tpostOutRemoved)
summary(AHc_alphR_meantpost_mod_OutRemoved)
effectsize::standardize_parameters(AHc_alphR_meantpost_mod_OutRemoved)$Std_Coefficient
effectsize::standardize_parameters(AHc_alphR_meantpost_mod_OutRemoved)$CI_low
effectsize::standardize_parameters(AHc_alphR_meantpost_mod_OutRemoved)$CI_high

# Meta-analysis

meta_alphR_meantpost_OutRemoved <- rma(
  yi  = c(
    effectsize::standardize_parameters(AHd_alphR_meantpost_mod_OutRemoved)$Std_Coefficient[2],
    effectsize::standardize_parameters(AHc_alphR_meantpost_mod_OutRemoved)$Std_Coefficient[2]
  ),
  sei = c(
    ( effectsize::standardize_parameters(AHd_alphR_meantpost_mod_OutRemoved)$CI_high[2] -  effectsize::standardize_parameters(AHd_alphR_meantpost_mod_OutRemoved)$CI_low[2]) / (2 * 1.96),
    ( effectsize::standardize_parameters(AHc_alphR_meantpost_mod_OutRemoved)$CI_high[2] -  effectsize::standardize_parameters(AHc_alphR_meantpost_mod_OutRemoved)$CI_low[2]) / (2 * 1.96)
  )
)
summary(meta_alphR_meantpost_OutRemoved)
forest(meta_alphR_meantpost_OutRemoved)


##################################
# Figure S15

### outliers not removed
AHconf_habweight_OutIn   <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_habit_weight", y_coord_limits = c(0,1))
AHconf_alphAc_OutIn      <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alpha_action", y_coord_limits = c(0,1))
AHconf_alphR_OutIn       <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alpha_reward", y_coord_limits = c(0,1))
# outliers removed
### alpha
AHconf_habweight_OutRemoved   <- plot_scatter_lm(scatterdat_emp_AHc_tpostOutRemoved, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_habit_weight", y_coord_limits = c(0,1))
AHconf_alphAc_OutRemoved      <- plot_scatter_lm(scatterdat_emp_AHc_tpostOutRemoved, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alpha_action", y_coord_limits = c(0,1))
AHconf_alphR_OutRemoved       <- plot_scatter_lm(scatterdat_emp_AHc_tpostOutRemoved, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alpha_reward", y_coord_limits = c(0,1))


FigS15_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3),
  c(NA, NA, NA, NA, NA),
  c(4, NA, 5, NA, 6)
)

FigS15 <- grid.arrange(
  AHconf_habweight_OutIn,
  AHconf_alphAc_OutIn,
  AHconf_alphR_OutIn,
  AHconf_habweight_OutRemoved,
  AHconf_alphAc_OutRemoved,
  AHconf_alphR_OutRemoved,
  
  layout_matrix = FigS15_layout_matrix, 
  heights = c(1, 0.23, 1),
  widths = c(1, 0.23, 1, 0.23, 1)
)
ggsave("figures/FigS15.pdf", plot = FigS15, width = 12.5, height = 7, units = "in")

##########

#### FigS15 source data
figS15_data <- list(
  x_axis_mean_tpost = scatterdat_emp_AHconf$empdat_mean_tpost_days,
  a_habit_weight    = scatterdat_emp_AHconf$emp_RLH1_habit_weight,
  b_alpha_action    = scatterdat_emp_AHconf$emp_RLH1_alpha_action,
  c_alpha_R         = scatterdat_emp_AHconf$emp_RLH1_alpha_reward,
  x_axis_mean_tpost_TpostOutLiersRemoved = scatterdat_emp_AHc_tpostOutRemoved$empdat_mean_tpost_days,
  d_habit_weight_mean_tpost_TpostOutLiersRemoved    = scatterdat_emp_AHc_tpostOutRemoved$emp_RLH1_habit_weight,
  e_alpha_action_mean_tpost_TpostOutLiersRemoved    = scatterdat_emp_AHc_tpostOutRemoved$emp_RLH1_alpha_action,
  f_alpha_R_mean_tpost_TpostOutLiersRemoved         = scatterdat_emp_AHc_tpostOutRemoved$emp_RLH1_alpha_reward

)
max_len_figS15 <- max(lengths(figS15_data))
figS15_padded <- lapply(figS15_data, function(x) { length(x) <- max_len_figS15; x })
data_figS15   <- as.data.frame(figS15_padded)
write_csv(data_figS15, "source_data/data_figS15.csv")
summary(lm(scatterdat_emp_AHc_tpostOutRemoved$emp_RLH1_alpha_reward ~ scatterdat_emp_AHc_tpostOutRemoved$empdat_mean_tpost_days ))
#-----------------------------------------------------------------------------------#
##########
############## REVIEWER-RESPONSE RESULTS
##########
#-----------------------------------------------------------------------------------#

# R2.7 - Habitual users vs. number of posts

# simulation analysis to establish whether nPosts correlates with fitted habit weight (detailed in 'Reviewer Response' document)
# load data fitted to RLH1 simulated dataset, where all simulated agents had the same parameters but different numbers of posts
fitdat_nPosts     <-  read_csv(str_c("./../../data_processed/Twitter/fit/250323-0959RLH1_realPostNumbers/250323-1314RLH11unshortened.csv"))                
scatterdat_nPosts <- data.frame(habit_weight  = fitdat_nPosts$stickiness_weight,
                                alpha              = fitdat_nPosts$alpha,
                                alpha_action       = fitdat_nPosts$alpha_action,
                                cost               = fitdat_nPosts$cost,
                                nPosts             = scatterdat_emp_AHconf$empdat_nPosts
                                )
habweight_nPosts <- lm(habit_weight ~ nPosts, data = scatterdat_nPosts)
summary(habweight_nPosts)
alphaR_nPosts <- lm(alpha ~ nPosts, data = scatterdat_nPosts)
summary(alphaR_nPosts)
alphAc_nPosts <- lm(alpha_action ~ nPosts, data = scatterdat_nPosts)
summary(alphAc_nPosts)
cost_nPosts <- lm(cost ~ nPosts, data = scatterdat_nPosts)
summary(cost_nPosts)

