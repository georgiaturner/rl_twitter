



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
AHconf_data_name_string    <- "241105_241104_241104_240416_AHconf_cleaned_preproc_AICw"
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
   age                       = as.numeric(difftime(timeofquestionnaire_local, DOB, units = "days") / 365.25),
   timeonTwitter             = as.numeric(timeofquestionnaire_local - t_firstpost, units = "days") / 365.25) %>%
 group_by(user_num) %>%
 mutate(mean_tpost  = mean(t_post),
        mean_likes  = mean(likes)) %>%
 ungroup()

empdat_AHconf <- empdat_AHconf %>%
  mutate(
    DOB                       = as.Date(paste(DOB, "-01", sep = "")),
    timeofquestionnaire_local = as.POSIXct(timeofquestionnaire_local, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    age                       = as.numeric(difftime(timeofquestionnaire_local, DOB, units = "days") / 365.25),
    timeonTwitter             = as.numeric(timeofquestionnaire_local - t_firstpost, units = "days") / 365.25) %>%
  group_by(user_num) %>%
  mutate(mean_tpost  = mean(t_post),
         mean_likes  = mean(likes)) %>%
  ungroup()

# # load simgens where they were all simulated with exact parameters of confirmatory sample
simgen_AHconf_path <- str_c("./../../data_processed/Twitter/simgen/", AHconf_data_name_string, "/");
simgen_FP_AHconf   <- read_csv(str_c(simgen_AHconf_path, "241106-0958FP.csv"))
simgen_CP_AHconf   <- read_csv(str_c(simgen_AHconf_path, "241106-0959CP.csv"))
simgen_PH_AHconf   <- read_csv(str_c(simgen_AHconf_path, "241106-1004PH.csv"))
simgen_RL1_AHconf  <- read_csv(str_c(simgen_AHconf_path, "241106-1001RL1.csv"))
simgen_RL2_AHconf  <- read_csv(str_c(simgen_AHconf_path, "241106-1002RL2.csv"))
simgen_RLH1_AHconf <- read_csv(str_c(simgen_AHconf_path, "241106-1006RLH1.csv"))
simgen_RLH2_AHconf <- read_csv(str_c(simgen_AHconf_path, "241106-1009RLH2.csv"))


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#----------------------   PREPARE VARIABLES FOR GLMS      --------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

# make datasets ready for GLMs


#### models
glmdat_sim_FP_AHconf   <- mkvars_glm(simgen_FP_AHconf)
glmdat_sim_CP_AHconf   <- mkvars_glm(simgen_CP_AHconf)
glmdat_sim_PH_AHconf   <- mkvars_glm(simgen_PH_AHconf)
glmdat_sim_RL1_AHconf  <- mkvars_glm(simgen_RL1_AHconf)
glmdat_sim_RL2_AHconf  <- mkvars_glm(simgen_RL2_AHconf)
glmdat_sim_RLH1_AHconf <- mkvars_glm(simgen_RLH1_AHconf)
glmdat_sim_RLH2_AHconf <- mkvars_glm(simgen_RLH2_AHconf)

#### empirical
glmdat_empdatAHdisc   <- mkvars_glm(empdat_AHdisc)
glmdat_empdatAHconf   <- mkvars_glm(empdat_AHconf)


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
glmRPE_sim_RL1_AHconf  <- glmdat_sim_RL1_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_RL2_AHconf  <- glmdat_sim_RL2_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH1_AHconf <- glmdat_sim_RLH1_AHconf %>% glm_RPE_deltatpost(.)
glmRPE_sim_RLH2_AHconf <- glmdat_sim_RLH2_AHconf %>% glm_RPE_deltatpost(.)

# all empdat
glmRPE_empdatAHconf    <- glmdat_empdatAHconf %>% glm_RPE_deltatpost(.)



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

                                    simgen_PH_alphAc             = filter(simgen_PH_AHconf, post_num_pic==1)$alpha_action,

                                    simgen_RL1_alph              = filter(simgen_RL1_AHconf, post_num_pic ==1)$alpha ,
                                    simgen_RL1_cost              = filter(simgen_RL1_AHconf, post_num_pic ==1)$cost ,

                                    simgen_RL2_alph_P            = filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_P,
                                    simgen_RL2_alph_N            = filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_N,
                                    simgen_RL2_cost              = filter(simgen_RL2_AHconf, post_num_pic==1)$cost,

                                    simgen_RLH1_alph             = filter(simgen_RLH1_AHconf, post_num_pic==1)$alpha,
                                    simgen_RLH1_cost             = filter(simgen_RLH1_AHconf, post_num_pic==1)$cost,
                                    simgen_RLH1_alphAc           = filter(simgen_RLH1_AHconf, post_num_pic==1)$alpha_action,
                                    simgen_RLH1_stickweight      = filter(simgen_RLH1_AHconf, post_num_pic==1)$stickiness_weight,

                                    simgen_RLH2_alph_P           = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_P,
                                    simgen_RLH2_alph_N           = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_N,
                                    simgen_RLH2_cost             = filter(simgen_RLH2_AHconf, post_num_pic==1)$cost,
                                    simgen_RLH2_alphAc           = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_action,
                                    simgen_RLH2_stickweight        = filter(simgen_RLH2_AHconf, post_num_pic==1)$stickiness_weight,

                                    simgen_RL2_posbias           = filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_P - filter(simgen_RL2_AHconf, post_num_pic==1)$alpha_N,
                                    simgen_RLH2_posbias          = filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_P - filter(simgen_RLH2_AHconf, post_num_pic==1)$alpha_N,

                                    simgen_RL1_beta_RPE    = as.data.frame(ranef(glmRPE_sim_RL1_AHconf))$condval,
                                    simgen_RLH1_beta_RPE   = as.data.frame(ranef(glmRPE_sim_RLH1_AHconf))$condval,
                                    simgen_RLH2_beta_RPE   = as.data.frame(ranef(glmRPE_sim_RLH2_AHconf))$condval
                                    )


##### now empirical 

scatterdat_emp_AHdisc <- data.frame(emp_FP_pol       = filter(empdat_AHdisc, post_num_pic==1)$policy.fitdat_FP,
                                    
                                    emp_CP_a         = filter(empdat_AHdisc, post_num_pic==1)$a.fitdat_CP,
                                    eemp_CP_b         = filter(empdat_AHdisc, post_num_pic==1)$b.fitdat_CP,
                                    emp_CP_c         = filter(empdat_AHdisc, post_num_pic==1)$c.fitdat_CP,
                                    # 
                                    emp_PH_alphAc    = filter(empdat_AHdisc, post_num_pic==1)$alpha_action.fitdat_PH,
                                    # 
                                    emp_RL1_alph     = filter(empdat_AHdisc, post_num_pic==1)$alpha.fitdat_RL1,
                                    emp_RL1_cost     = filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RL1,
                                        
                                    emp_RL2_alph_P    = filter(empdat_AHdisc, post_num_pic==1)$alpha_P.fitdat_RL2,
                                    emp_RL2_alph_N    = filter(empdat_AHdisc, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RL2_cost      = filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RL2,
                                    
                                    emp_RLH1_alph     = filter(empdat_AHdisc, post_num_pic==1)$alpha.fitdat_RLH1,
                                    emp_RLH1_cost     = filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RLH1,
                                    emp_RLH1_alphAc   = filter(empdat_AHdisc, post_num_pic==1)$alpha_action.fitdat_RLH1,
                                    emp_RLH1_stickweight = filter(empdat_AHdisc, post_num_pic==1)$stickiness_weight.fitdat_RLH1,
                                    
                                    emp_RLH2_alph_P     = filter(empdat_AHdisc, post_num_pic==1)$alpha_P.fitdat_RLH2,
                                    emp_RLH2_alph_N     = filter(empdat_AHdisc, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    emp_RLH2_cost       = filter(empdat_AHdisc, post_num_pic==1)$cost.fitdat_RLH2,
                                    emp_RLH2_alphAc    = filter(empdat_AHdisc, post_num_pic==1)$alpha_action.fitdat_RLH2,
                                    emp_RLH2_stickweight  = filter(empdat_AHdisc, post_num_pic==1)$stickiness_weight.fitdat_RLH2,
                                    
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

scatterdat_emp_AHconf <- data.frame(emp_FP_pol       = filter(empdat_AHconf, post_num_pic==1)$policy.fitdat_FP,
                                    
                                    emp_CP_a         = filter(empdat_AHconf, post_num_pic==1)$a.fitdat_CP,
                                    emp_CP_b         = filter(empdat_AHconf, post_num_pic==1)$b.fitdat_CP,
                                    emp_CP_c         = filter(empdat_AHconf, post_num_pic==1)$c.fitdat_CP,
                                    # 
                                    emp_PH_alphAc    = filter(empdat_AHconf, post_num_pic==1)$alpha_action.fitdat_PH,
                                    
                                    emp_RL1_alph     = filter(empdat_AHconf, post_num_pic==1)$alpha.fitdat_RL1,
                                    emp_RL1_cost     = filter(empdat_AHconf, post_num_pic==1)$cost.fitdat_RL1,
                                    
                                    emp_RL2_alph_P    = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RL2,
                                    emp_RL2_alph_N    = filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RL2_cost      = filter(empdat_AHconf, post_num_pic==1)$cost.fitdat_RL2,
                                    
                                    emp_RLH1_alph     = filter(empdat_AHconf, post_num_pic==1)$alpha.fitdat_RLH1,
                                    emp_RLH1_cost     = filter(empdat_AHconf, post_num_pic==1)$cost.fitdat_RLH1,
                                    emp_RLH1_alphAc  = filter(empdat_AHconf, post_num_pic==1)$alpha_action.fitdat_RLH1,
                                    emp_RLH1_stickweight = filter(empdat_AHconf, post_num_pic==1)$stickiness_weight.fitdat_RLH1,
                                    
                                    emp_RLH2_alph_P     = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RLH2,
                                    emp_RLH2_alph_N     = filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    emp_RLH2_cost       = filter(empdat_AHconf, post_num_pic==1)$cost.fitdat_RLH2,
                                    emp_RLH2_alphAc    = filter(empdat_AHconf, post_num_pic==1)$alpha_action.fitdat_RLH2,
                                    emp_RLH2_stickweight  = filter(empdat_AHconf, post_num_pic==1)$stickiness_weight.fitdat_RLH2,
                                    
                                    emp_RL2_posbias = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RL2 - 
                                      filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RL2,
                                    emp_RLH2_posbias = filter(empdat_AHconf, post_num_pic==1)$alpha_P.fitdat_RLH2 - 
                                      filter(empdat_AHconf, post_num_pic==1)$alpha_N.fitdat_RLH2,
                                    
                                    emp_beta_RPE         = as.data.frame(ranef(glmRPE_empdatAHconf))$condval,
                                    
                                    empdat_AHscore          = filter(empdat_AHconf, post_num_pic==1)$Authentic_Happiness_inventory,
                                    empdat_followercount    = filter(empdat_AHconf, post_num_pic==1)$followers_count,
                                    empdat_mean_tpost_days  = filter(empdat_AHconf, post_num_pic==1)$mean_tpost,
                                    empdat_mean_likes       = filter(empdat_AHconf, post_num_pic==1)$mean_likes,
                                    

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
user1_num <- unique(empdat_AHconf$user_num)[3]; # which user you want to plot
user1 <- ggplot(filter(empdat_AHconf, user_num == user1_num)) +
  geom_line(aes(x = post_num_pic, y = t_post), color = color5, linewidth = 1) +
  geom_point(aes(x = post_num_pic, y = likes, stroke = NA), color = "#FFB347", size = 1.5) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  theme_classic() +
  ggtitle(str_c("User ", user1_num))
user2_num <- unique(empdat_AHconf$user_num)[15]; # which user you want to plot
user2 <- ggplot(filter(empdat_AHconf, user_num == user2_num)) +
  geom_line(aes(x = post_num_pic, y = t_post), color = color5, linewidth = 1) +
  geom_point(aes(x = post_num_pic, y = likes, stroke = NA), color = "#FFB347", size = 1.5) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  theme_classic() +
  ggtitle(str_c("User ", user2_num))
user3_num <- unique(empdat_AHconf$user_num)[13]; # which user you want to plot
user3 <- ggplot(filter(empdat_AHconf, user_num == user3_num)) +
  geom_line(aes(x = post_num_pic, y = t_post), color = color5, linewidth = 1) +
  geom_point(aes(x = post_num_pic, y = likes, stroke = NA), color = "#FFB347", size = 1.5) +
  xlab("Post Number") +
  ylab("Simulated T_Post") + 
  theme_classic() +
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

ggsave("figures/Fig1.png", plot = Fig1, width = 17, height = 10, units = "in")


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
    meanAICw_PH   = mean(AICw_PH),
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
    meanAICw_PH   = mean(AICw_PH),
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

modfals_fig <- plot_glm_forest(glmRPE_list_AHconf, dataset_names_AHconf, 
                                     glm_colour_palette, 
                                     pred_varname = "scaled_lag_RPE", 
                                     dep_varname  = "scaled_delta_logtpost")


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
  modcomp_fig, modfals_fig, 
  layout_matrix = Fig3_layout_matrix,
  widths = c(1.8, 0.5, 1.1),  # Control relative height of each row (0.2 for the spacer),
  heights = 1
)


ggsave("figures/Fig3.png", plot = Fig3, width = 17, height = 8, units = "in")

##################################
##################################
# Figure 4: Posting Latency
##################################
##################################


##################################
# Stats
AHd_alphR_meantpost_mod <- lm(emp_RLH1_alph ~ empdat_mean_tpost_days, data = scatterdat_emp_AHdisc)
summary(AHd_alphR_meantpost_mod)
AHc_stickweight_meantpost_mod <- lm(emp_RLH1_stickweight ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_stickweight_meantpost_mod)
AHc_alphAc_meantpost_mod <- lm(emp_RLH1_alphAc ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphAc_meantpost_mod)
AHc_alphR_meantpost_mod <- lm(emp_RLH1_alph ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphR_meantpost_mod)
# Meta-analysis
meta_alphRLH1_meantpost <- rma(
  yi  = c(coef(AHd_alphR_meantpost_mod)["empdat_mean_tpost_days"][[1]], coef(AHc_alphR_meantpost_mod)["empdat_mean_tpost_days"][[1]]),
  sei = c(summary(AHd_alphR_meantpost_mod)$coefficients["empdat_mean_tpost_days", "Std. Error"], summary(AHc_alphR_meantpost_mod)$coefficients["empdat_mean_tpost_days", "Std. Error"])
)
summary(meta_alphRLH1_meantpost)
forest(meta_alphRLH1_meantpost)

#### Fig 4 in main paper with outliers removed
AHconf_stickweight_lims <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_stickweight", x_coord_limits = c(0,6), y_coord_limits = c(0, 1))
AHconf_alphAc_lims      <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alphAc", x_coord_limits = c(0,6), y_coord_limits = c(0, 1))
AHconf_alph_lims        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alph",x_coord_limits = c(0,6), y_coord_limits = c(0, 1))

#########
# Arrange Fig 4

# Define a layout matrix
Fig4_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3)
)

Fig4 <- grid.arrange(
  AHconf_stickweight_lims,
  AHconf_alphAc_lims,
  AHconf_alph_lims,
  layout_matrix = Fig4_layout_matrix,
  widths = c(2.2, 0.5, 2.2, 0.5, 2.2)#,
  )

ggsave("figures/Fig4.png", plot = Fig4, width = 20, height = 7, units = "in")


##################################
##################################
# Figure 5: Age, gender and wellbeing
##################################
##################################


##################################
# Stats
# age
AHc_stickweight_age_mod <- lm(emp_RLH1_stickweight ~ age, data = scatterdat_emp_age_AHconf)
summary(AHc_stickweight_age_mod)
# gender
t_test_result_stickweight_gender <- t.test(emp_RLH1_stickweight ~ gender, data = scatterdat_emp_AHconf)
t_test_result_stickweight_gender
# wellbeing
AHd_stickweight_AHI_mod <- lm(emp_RLH1_stickweight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_stickweight_AHI_mod)
AHd_alphAc_AHI_mod <- lm(emp_RLH1_alphAc ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_alphAc_AHI_mod)
AHd_alphR_AHI_mod <- lm(emp_RLH1_alph ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_alphR_AHI_mod)
AHc_stickweight_AHI_mod <- lm(emp_RLH1_stickweight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_stickweight_AHI_mod)
AHc_alphAc_AHI_mod <- lm(emp_RLH1_alphAc ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_alphAc_AHI_mod)
AHc_alphR_AHI_mod <- lm(emp_RLH1_alph ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_alphR_AHI_mod)
# Meta-analysis
## stickiness weight
meta_stickweight_AHI_linear <- rma(
  yi  = c(coef(AHd_stickweight_AHI_mod)["empdat_AHscore"][[1]], coef(AHc_stickweight_AHI_mod)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_stickweight_AHI_mod)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_stickweight_AHI_mod)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_stickweight_AHI_linear)
forest(meta_stickweight_AHI_linear)
meta_stickweight_AHI_quad <- rma(
  yi  = c(coef(AHd_stickweight_AHI_mod)["I(empdat_AHscore^2)"][[1]], coef(AHc_stickweight_AHI_mod)["I(empdat_AHscore^2)"][[1]]),
  sei = c(summary(AHd_stickweight_AHI_mod)$coefficients["I(empdat_AHscore^2)", "Std. Error"], summary(AHc_stickweight_AHI_mod)$coefficients["I(empdat_AHscore^2)", "Std. Error"])
)
summary(meta_stickweight_AHI_quad)
forest(meta_stickweight_AHI_quad)

## action learning rate
meta_alphAc_AHI_linear <- rma(
  yi  = c(coef(AHd_alphAc_AHI_mod)["empdat_AHscore"][[1]], coef(AHc_alphAc_AHI_mod)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_alphAc_AHI_mod)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_alphAc_AHI_mod)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_alphAc_AHI_linear)
forest(meta_alphAc_AHI_linear)
meta_alphAc_AHI_quad <- rma(
  yi  = c(coef(AHd_alphAc_AHI_mod)["I(empdat_AHscore^2)"][[1]], coef(AHc_alphAc_AHI_mod)["I(empdat_AHscore^2)"][[1]]),
  sei = c(summary(AHd_alphAc_AHI_mod)$coefficients["I(empdat_AHscore^2)", "Std. Error"], summary(AHc_alphAc_AHI_mod)$coefficients["I(empdat_AHscore^2)", "Std. Error"])
)
summary(meta_alphAc_AHI_quad)
forest(meta_alphAc_AHI_quad)

## reward learning rate
meta_alphR_AHI <- rma(
  yi  = c(coef(AHd_alphR_AHI_mod)["empdat_AHscore"][[1]], coef(AHc_alphR_AHI_mod)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_alphR_AHI_mod)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_alphR_AHI_mod)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_alphR_AHI)
forest(meta_alphR_AHI)

##################################
# Figure 5
AHconf_stickweight_age <- plot_scatter_lm(scatterdat_emp_age_AHconf, xvar = "age", yvar = "emp_RLH1_stickweight", point_size = 1.15)
AHconf_mean_values     <- scatterdat_emp_AHconf %>%
  group_by(gender) %>%
  summarize(mean_y = mean(emp_RLH1_stickweight, na.rm = TRUE))
AHconf_stickweight_gen <- create_raincloud_plot(scatterdat_emp_AHconf, "gender", "emp_RLH1_stickweight", AHconf_mean_values, gender_palette)
AHconf_stickweight_AH  <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH1_stickweight", point_size =  0.95)
AHconf_alphAc_AH       <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH1_alphAc", point_size = 0.95)
AHconf_alphR_AH        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH1_alph", point_size = 0.95)



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
  AHconf_stickweight_age,
  AHconf_stickweight_gen,
  AHconf_stickweight_AH,
  AHconf_alphAc_AH,
  AHconf_alphR_AH,
  layout_matrix = Fig5_layout_matrix,
  heights = c(1, 0.14, 1)
)

ggsave("figures/Fig5.png", plot = Fig5, width = 10, height = 7, units = "in")


#-----------------------------------------------------------------------------------#
##########
############## SUPPLEMENTARY RESULTS
##########
#-----------------------------------------------------------------------------------#

# ** The code for figure S1 can be found in the script 01-model_comparison.R, 
# and the code for figure S2 can be found in the script 02-param_recovery.R, both saved in this folder.

##################################
##################################
# Figure S3: Reward learning behavioural signature across participants
##################################
##################################

################
# Stats

AHc_RPE_stickweight_sim_mod <- lm(simgen_RLH1_stickweight ~ simgen_RLH1_beta_RPE, data = scatterdat_sim_AHconf)
summary(AHc_RPE_stickweight_sim_mod )
AHc_RPE_stickweight_emp_mod <- lm(emp_RLH1_stickweight ~ emp_beta_RPE, data = scatterdat_emp_AHconf)
summary(AHc_RPE_stickweight_emp_mod)

################
# stickweight
# S3a
RPE_stickweight_sim_AHconf <- plot_scatter_lm(scatterdat_sim_AHconf, xvar = "simgen_RLH1_beta_RPE", yvar = "simgen_RLH1_stickweight", y_coord_limits = c(0,1))
# S3b
RPE_stickweight_emp_AHconf <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "emp_beta_RPE", yvar = "emp_RLH1_stickweight",line_colour=color5, y_coord_limits = c(0,1))

#################
# Figure S3

FigS3_layout_matrix <- rbind(
  c(1, NA, 2)  # First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS3 <- grid.arrange(
  RPE_stickweight_sim_AHconf,
  RPE_stickweight_emp_AHconf,
  nrow=1,
  layout_matrix = FigS3_layout_matrix, 
  widths = c(1, 0.23, 1)
)

ggsave("figures/FigS3.png", plot = FigS3, width = 16, height = 7, units = "in")


##################################
##################################
# Figure S4: Relating behavioural signatures to posting latency and age across users
##################################
##################################


################
# Stats

AHc_RPE_meantpost_mod <- lm(empdat_mean_tpost_days ~ emp_beta_RPE + I(emp_beta_RPE^2), data = scatterdat_emp_AHconf)
summary(AHc_RPE_meantpost_mod )
AHc_RPE_age_mod <- lm(age ~ emp_beta_RPE, data = scatterdat_emp_AHconf)
summary(AHc_RPE_age_mod)

#################
# Figure S4

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


FigS4_layout_matrix <- rbind(
  c(1, NA, 2)  # First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS4 <- grid.arrange(
  RPE_meantpost_AHconf,
  RPE_age_AHconf,
  nrow = 1,
  layout_matrix = FigS4_layout_matrix,
  widths = c(1, 0.23, 1)
)

ggsave("figures/FigS4.png", plot = FigS4, width = 16, height = 7, units = "in")


# ** The code for figure S5 can be found in the script 01-model_comparison.R.

##################################
##################################
# Figure S6: RLH2 Posting latency results
##################################
##################################

################
# Stats

AHc_t_test_result_posbiasRLH2 <- t.test(scatterdat_emp_AHconf$emp_RLH2_alph_P, scatterdat_emp_AHconf$emp_RLH2_alph_N, paired = TRUE)
AHc_t_test_result_posbiasRLH2
AHc_stickweight_meantpost_mod_RLH2 <- lm(emp_RLH2_stickweight ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_stickweight_meantpost_mod_RLH2)
AHc_alphAc_meantpost_mod_RLH2 <- lm(emp_RLH2_alphAc ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphAc_meantpost_mod_RLH2)
Ahd_posbias_meantpost_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_mean_tpost_days, data = scatterdat_emp_AHdisc)
summary(Ahd_posbias_meantpost_mod_RLH2)
Ahc_posbias_meantpost_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(Ahc_posbias_meantpost_mod_RLH2)
AHd_alphP_meantpost_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_mean_tpost_days, data = scatterdat_emp_AHdisc)
summary(AHd_alphP_meantpost_mod_RLH2)
AHc_alphP_meantpost_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphP_meantpost_mod_RLH2)
AHc_alphN_meantpost_mod_RLH2 <- lm(emp_RLH2_alph_N ~ empdat_mean_tpost_days, data = scatterdat_emp_AHconf)
summary(AHc_alphN_meantpost_mod_RLH2)
### alpha P meta analysis
meta_meantpost_alphP_RLH2 <- rma(
  yi  = c(coef(AHd_alphP_meantpost_mod_RLH2)["empdat_mean_tpost_days"][[1]], coef(AHc_alphP_meantpost_mod_RLH2)["empdat_mean_tpost_days"][[1]]),
  sei = c(summary(AHd_alphP_meantpost_mod_RLH2)$coefficients["empdat_mean_tpost_days", "Std. Error"], summary(AHc_alphP_meantpost_mod_RLH2)$coefficients["empdat_mean_tpost_days", "Std. Error"])
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
# Figure S6

RLH2_df <- pivot_longer(scatterdat_emp_AHconf, 
                        cols = c(emp_RLH2_alph_P, emp_RLH2_alph_N),
                        names_to = "Variable", 
                        values_to = "Value")
RLH2posbias_palette <- c("emp_RLH2_alph_P" = color4, "emp_RLH2_alph_N" = color4)
AHconf_posbias <- plot_violin(RLH2_df,"Variable", "Value", fillvar = "Variable", palette = RLH2posbias_palette  )
AHconf_meantpost_stickweight_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_stickweight", y_coord_limits = c(0,1))
AHconf_meantpost_alphAc_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_alphAc", y_coord_limits = c(0,1))
AHconf_meantpost_posbias_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_posbias", y_coord_limits = c(-1,1))
AHconf_meantpost_alphN_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_alph_N", y_coord_limits = c(0,1))
AHconf_meantpost_alphP_RLH2 <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH2_alph_P", y_coord_limits = c(0,1))


FigS6_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3),
  c(NA, NA, NA, NA, NA),
  c(4, NA, 5, NA, 6)
)

FigS6 <- grid.arrange(
  AHconf_posbias, 
  AHconf_meantpost_stickweight_RLH2, 
  AHconf_meantpost_alphAc_RLH2,
  AHconf_meantpost_posbias_RLH2,
  AHconf_meantpost_alphP_RLH2, 
  AHconf_meantpost_alphN_RLH2,
  layout_matrix = FigS6_layout_matrix, 
  widths = c(1, 0.23, 1, 0.23, 1), 
  heights = c(1, 0.3, 1)
)

ggsave("figures/FigS6.png", plot = FigS6, width = 18, height = 12, units = "in")


##################################
##################################
# Figure S7: RLH2 Age and gender results
##################################
##################################

##################################
# Stats
AHc_stickweight_age_mod_RLH2 <- lm(emp_RLH2_stickweight ~ age, data = scatterdat_emp_age_AHconf)
summary(AHc_stickweight_age_mod_RLH2)
t_test_result_stickweight_gender_RLH2 <- t.test(emp_RLH2_stickweight ~ gender, data = scatterdat_emp_AHconf)
t_test_result_stickweight_gender_RLH2


##################################
# Figure S7
AHconf_stickweight_age_RLH2 <- plot_scatter_lm(scatterdat_emp_age_AHconf, xvar = "age", yvar = "emp_RLH2_stickweight")
AHconf_mean_values_RLH2 <- scatterdat_emp_AHconf %>%
  group_by(gender) %>%
  summarize(mean_y = mean(emp_RLH2_stickweight, na.rm = TRUE))
AHconf_stickweight_gen_RLH2 <- create_raincloud_plot(scatterdat_emp_AHconf, "gender", "emp_RLH2_stickweight", AHconf_mean_values_RLH2, gender_palette)


FigS7_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3)
)

FigS7 <- grid.arrange(
  AHconf_stickweight_age_RLH2,
  AHconf_stickweight_gen_RLH2,
  layout_matrix = FigS7_layout_matrix, 
  widths = c(1.6, 0.23, 1)
)
ggsave("figures/FigS7.png", plot = FigS7, width = 18, height = 7, units = "in")




##################################
# Stats
AHd_stickweight_AHI_mod_RLH2 <- lm(emp_RLH2_stickweight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_stickweight_AHI_mod_RLH2)
AHc_stickweight_AHI_mod_RLH2 <- lm(emp_RLH2_stickweight ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_stickweight_AHI_mod_RLH2)
AHd_alphAc_AHI_mod_RLH2 <- lm(emp_RLH2_alphAc ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHdisc)
summary(AHd_alphAc_AHI_mod_RLH2)
AHc_alphAc_AHI_mod_RLH2 <- lm(emp_RLH2_alphAc ~ empdat_AHscore + I(empdat_AHscore^2), data = scatterdat_emp_AHconf)
summary(AHc_alphAc_AHI_mod_RLH2)
AHd_alphP_AHI_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_alphP_AHI_mod_RLH2)
AHc_alphP_AHI_mod_RLH2 <- lm(emp_RLH2_alph_P ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_alphP_AHI_mod_RLH2)
AHd_alphN_AHI_mod_RLH2 <- lm(emp_RLH2_alph_N ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_alphN_AHI_mod_RLH2)
AHc_alphN_AHI_mod_RLH2 <- lm(emp_RLH2_alph_N ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_alphN_AHI_mod_RLH2)
AHd_posbias_AHI_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_AHscore, data = scatterdat_emp_AHdisc)
summary(AHd_posbias_AHI_mod_RLH2)
AHc_posbias_AHI_mod_RLH2 <- lm(emp_RLH2_posbias ~ empdat_AHscore, data = scatterdat_emp_AHconf)
summary(AHc_posbias_AHI_mod_RLH2)
# Meta-analysis
## stickweight
meta_stickweight_AHI_linear_RLH2 <- rma(
  yi  = c(coef(AHd_stickweight_AHI_mod_RLH2)["empdat_AHscore"][[1]], coef(AHc_stickweight_AHI_mod_RLH2)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_stickweight_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_stickweight_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_stickweight_AHI_linear_RLH2)
forest(meta_stickweight_AHI_linear_RLH2)
meta_stickweight_AHI_quad_RLH2 <- rma(
  yi  = c(coef(AHd_stickweight_AHI_mod_RLH2)["I(empdat_AHscore^2)"][[1]], coef(AHc_stickweight_AHI_mod_RLH2)["I(empdat_AHscore^2)"][[1]]),
  sei = c(summary(AHd_stickweight_AHI_mod_RLH2)$coefficients["I(empdat_AHscore^2)", "Std. Error"], summary(AHc_stickweight_AHI_mod_RLH2)$coefficients["I(empdat_AHscore^2)", "Std. Error"])
)
summary(meta_stickweight_AHI_quad_RLH2)
forest(meta_stickweight_AHI_quad_RLH2)

## alphAc
meta_alphAc_AHI_linear_RLH2 <- rma(
  yi  = c(coef(AHd_alphAc_AHI_mod_RLH2)["empdat_AHscore"][[1]], coef(AHc_alphAc_AHI_mod_RLH2)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_alphAc_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_alphAc_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_alphAc_AHI_linear_RLH2)
forest(meta_alphAc_AHI_linear)
meta_alphAc_AHI_quad_RLH2 <- rma(
  yi  = c(coef(AHd_alphAc_AHI_mod_RLH2)["I(empdat_AHscore^2)"][[1]], coef(AHc_alphAc_AHI_mod_RLH2)["I(empdat_AHscore^2)"][[1]]),
  sei = c(summary(AHd_alphAc_AHI_mod_RLH2)$coefficients["I(empdat_AHscore^2)", "Std. Error"], summary(AHc_alphAc_AHI_mod_RLH2)$coefficients["I(empdat_AHscore^2)", "Std. Error"])
)
summary(meta_alphAc_AHI_quad_RLH2)
forest(meta_alphAc_AHI_quad_RLH2)

# alpha P
meta_alphP_AHI_RLH2 <- rma(
  yi  = c(coef(AHd_alphP_AHI_mod_RLH2)["empdat_AHscore"][[1]], coef(AHc_alphP_AHI_mod_RLH2)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_alphP_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_alphP_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_alphP_AHI_RLH2)
forest(meta_alphP_AHI_RLH2)

# alpha N
meta_alphN_AHI_RLH2 <- rma(
  yi  = c(coef(AHd_alphN_AHI_mod_RLH2)["empdat_AHscore"][[1]], coef(AHc_alphN_AHI_mod_RLH2)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_alphN_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_alphN_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_alphN_AHI_RLH2)
forest(meta_alphN_AHI_RLH2)

# posbias 
meta_posbias_AHI_RLH2 <- rma(
  yi  = c(coef(AHd_posbias_AHI_mod_RLH2)["empdat_AHscore"][[1]], coef(AHc_posbias_AHI_mod_RLH2)["empdat_AHscore"][[1]]),
  sei = c(summary(AHd_posbias_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"], summary(AHc_posbias_AHI_mod_RLH2)$coefficients["empdat_AHscore", "Std. Error"])
)
summary(meta_posbias_AHI_RLH2)
forest(meta_posbias_AHI_RLH2)


##################################
# Figure S8

AHconf_stickweight_AHI_RLH2  <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_stickweight")
AHconf_alphAc_AHI_RLH2       <- plot_scatter_quad(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_alphAc")
AHconf_alphP_AHI_RLH2        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_alph_P")
AHconf_alphN_AHI_RLH2        <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_alph_N")
AHconf_posbias_AHI_RLH2      <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_AHscore", yvar = "emp_RLH2_posbias")


FigS8_layout_matrix <- rbind(
  c(1, NA, 2, NA, NA),
  c(NA, NA, NA, NA, NA),
  c(3, NA, 4, NA, 5)
)

FigS8 <- grid.arrange(
  AHconf_stickweight_AHI_RLH2,
  AHconf_alphAc_AHI_RLH2,
  AHconf_alphP_AHI_RLH2,
  AHconf_alphN_AHI_RLH2,
  AHconf_posbias_AHI_RLH2,
  
  layout_matrix = FigS8_layout_matrix, 
  heights = c(1, 0.23, 1),
  widths = c(1, 0.23, 1, 0.23, 1)
)
ggsave("figures/FigS8.png", plot = FigS8, width = 12.5, height = 7, units = "in")


##################################
##################################
# Figure S9: Posting latency correlations with outliers removed
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
AHc_stickweight_meantpost_mod_OutRemoved <- lm(emp_RLH1_stickweight ~ empdat_mean_tpost_days, data = scatterdat_emp_AHc_tpostOutRemoved)
summary(AHc_stickweight_meantpost_mod_OutRemoved)
AHc_alphAc_meantpost_mod_OutRemoved <- lm(emp_RLH1_alphAc ~ empdat_mean_tpost_days, data = scatterdat_emp_AHc_tpostOutRemoved)
summary(AHc_alphAc_meantpost_mod_OutRemoved)
AHd_alphR_meantpost_mod_OutRemoved <- lm(emp_RLH1_alph ~ empdat_mean_tpost_days, data = scatterdat_emp_AHd_tpostOutRemoved)
summary(AHd_alphR_meantpost_mod_OutRemoved)
AHc_alphR_meantpost_mod_OutRemoved <- lm(emp_RLH1_alph ~ empdat_mean_tpost_days, data = scatterdat_emp_AHc_tpostOutRemoved)
summary(AHc_alphR_meantpost_mod_OutRemoved)
# Meta-analysis
meta_alphR_meantpost_OutRemoved <- rma(
  yi  = c(coef(AHd_alphR_meantpost_mod_OutRemoved)["empdat_mean_tpost_days"][[1]], coef(AHc_alphR_meantpost_mod_OutRemoved)["empdat_mean_tpost_days"][[1]]),
  sei = c(summary(AHd_alphR_meantpost_mod_OutRemoved)$coefficients["empdat_mean_tpost_days", "Std. Error"], summary(AHc_alphR_meantpost_mod_OutRemoved)$coefficients["empdat_mean_tpost_days", "Std. Error"])
)
summary(meta_alphR_meantpost_OutRemoved)
forest(meta_alphR_meantpost_OutRemoved)

##################################
# Figure S9

### outliers not removed
AHconf_stickweight_OutIn <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_stickweight", y_coord_limits = c(0,1))
AHconf_alphAc_OutIn <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alphAc", y_coord_limits = c(0,1))
AHconf_alphR_OutIn <- plot_scatter_lm(scatterdat_emp_AHconf, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alph", y_coord_limits = c(0,1))
# outliers removed
### alpha
AHconf_stickweight_OutRemoved <- plot_scatter_lm(scatterdat_emp_AHc_tpostOutRemoved, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_stickweight", y_coord_limits = c(0,1))
AHconf_alphAc_OutRemoved <- plot_scatter_lm(scatterdat_emp_AHc_tpostOutRemoved, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alphAc", y_coord_limits = c(0,1))
AHconf_alphR_OutRemoved <- plot_scatter_lm(scatterdat_emp_AHc_tpostOutRemoved, xvar = "empdat_mean_tpost_days", yvar = "emp_RLH1_alph", y_coord_limits = c(0,1))


FigS9_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3),
  c(NA, NA, NA, NA, NA),
  c(4, NA, 5, NA, 6)
)

FigS9 <- grid.arrange(
  AHconf_stickweight_OutIn,
  AHconf_alphAc_OutIn,
  AHconf_alphR_OutIn,
  AHconf_stickweight_OutRemoved,
  AHconf_alphAc_OutRemoved,
  AHconf_alphR_OutRemoved,
  
  layout_matrix = FigS9_layout_matrix, 
  heights = c(1, 0.23, 1),
  widths = c(1, 0.23, 1, 0.23, 1)
)
ggsave("figures/FigS9.png", plot = FigS9, width = 12.5, height = 7, units = "in")

##########

