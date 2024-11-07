



#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#-------------------------------------      01-MODEL_COMPARISON.R         --------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#


## This script compares different models on the same dataset by their AICw, and plots it.
## this script is flexible for multiple different datasets which have had the same models fitted, 
## therefore it begins by defining the relevant filepaths 
## and then the script is the same for each dataset
## It can be used for model comparison in empirical data or for model recovery of simulated datasets.

# By Georgia Turner, 2024 < georgianjt@gmail.com >


################################################################################

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------                  SET UP         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


rm(list = ls())
seed <- 1
set.seed(seed)

library(here)
library(tidyverse)
library(lubridate)
library(geiger)
library(ggridges)
library(gridExtra)
setwd(here("03-stats_and_visualisation"))
source('00-functions.R')

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#--------------------       AICW FOR MODEL RECOVERY      ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

# load fitted mods

##### for the Norm80
modnames_Norm80 <- list("Norm80_SimFP",
                        "Norm80_SimCP",
                        "Norm80_SimPH",
                        "Norm80_SimRL1",
                        "Norm80_SimRL2",
                        "Norm80_SimRLH1",
                        "Norm80_SimRLH2")
plot_modelrecovery80 <- plot_modrec(modnames_Norm80, fig_title = "Model Recovery for 80 posts")
modnames_Norm1000 <- list("Norm1000_SimFP",
                          "Norm1000_SimCP",
                          "Norm1000_SimPH",
                          "Norm1000_SimRL1",
                          "Norm1000_SimRL2",
                          "Norm1000_SimRLH1",
                          "Norm1000_SimRLH2")
plot_modelrecovery1000 <- plot_modrec(modnames_Norm1000, fig_title = "Model Recovery for 1000 posts")

# Arrange Fig S1

FigS1_layout_matrix <- rbind(
  c(1,2)
)

FigS1 <- grid.arrange(plot_modelrecovery80, 
                      plot_modelrecovery1000, 
                      nrow = 1,
                      layout_matrix = FigS1_layout_matrix, 
                      widths = c(1, 1))

ggsave("figures/FigS1.png", plot = FigS1, width = 21, height = 10, units = "in")


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#--------------------       AICW FOR EMPIRICAL DATA      ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
 
# here we load, calculate and plot AICw for empirical data, then resave a new dataframe with AICw information added for use in further analyses

##### Make figures
##### Fig 3a (to be saved then loaded and combined with 3b in the script '03-results_stats.R')
# load data and get AICws
AHconf_justLikes_dat <- generate_AICw_plot("AHconf_justLikes", to_return = "AICw_plot")
saveRDS(AHconf_justLikes_dat, file = str_c("figures/AHconf_justLikes_modcomp.rds"))

##### Fig S5
AHconf_justRTs_dat      <- generate_AICw_plot("AHconf_justRTs", to_return = "AICw_plot")
AHconf_LikesPlusRTs_dat <- generate_AICw_plot("AHconf_LikesPlusRTs", to_return = "AICw_plot")

FigS5_layout_matrix <- rbind(
  c(1),
  c(NA),
  c(2)# First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS5 <- grid.arrange(
  AHconf_LikesPlusRTs_dat,
  AHconf_justRTs_dat,
  layout_matrix = FigS5_layout_matrix,
  heights = c(1, 0.2, 1)
)

ggsave("figures/FigS5.png", plot = FigS5, width = 9, height = 16, units = "in")


#--------------------   ADD INFO BACK TO EMPDAT FOR SAVING    ---------------------------#

########################################
##### load empirical data
########################################


which_empdat    <- "AHconf_justLikes14" # or AHdisc_justLikes

if (which_empdat == "AHconf_justLikes14") {
  data_name_string <- "241104_241104_240416_AHconf_cleaned_preproc"
  empdat_path      <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
  empdat           <- read_csv(paste(empdat_path,  data_name_string, ".csv", sep = ""));
  df_AICw          <- generate_AICw_plot(which_empdat,  to_return = "df_AICw")
  tpost_unit = "seconds"
  
} else if (which_empdat == "AHdisc_justLikes") {
  data_name_string <- "241104_241104_240228_AHdisc_cleaned_preproc"
  empdat_path      <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"
  empdat           <- read_csv(paste(empdat_path,  data_name_string, ".csv", sep = ""))
  df_AICw          <- generate_AICw_plot(which_empdat,  to_return = "df_AICw")
  tpost_unit = "seconds"
  
} 

fitted_dat       <- load_modfits(which_empdat)

# convert tpost to days as model fitting code assumes this unit with various constraints
if (tpost_unit == "seconds") {
  empdat$t_post <- empdat$t_post/ (3600*24)
  tpost_unit <- "days"
}
fitdat_FP       <- fitted_dat$fitdat_FP
fitdat_CP       <- fitted_dat$fitdat_CP
fitdat_PH       <- fitted_dat$fitdat_PH
fitdat_RL1      <- fitted_dat$fitdat_RL1
fitdat_RL2      <- fitted_dat$fitdat_RL2
fitdat_RLH1     <- fitted_dat$fitdat_RLH1
fitdat_RLH2     <- fitted_dat$fitdat_RLH2


if (which_empdat == "AHconf_justLikes14" | which_empdat == "AHdisc_justLikes") {
  ### here we add the AICw info back into empdat to save a new df with all the info which can be used 
  # for plotting in the '03-results_stats' script saved in this folder
  
  # rename columns for entry into empdat
  column_mapping <- c("user_num"         = "user_num", 
                      "a_fitdat_FP"      = "AICw_FP",
                      "b_fitdat_CP"      = "AICw_CP", 
                      "c_fitdat_PH"      = "AICw_PH",
                      "d_fitdat_RL1"     = "AICw_RL1",
                      "e_fitdat_RL2"     = "AICw_RL2",
                      "f_fitdat_RLH1"    = "AICw_RLH1",
                      "g_fitdat_RLH2"    = "AICw_RLH2")
  
  df_AICW_tomerge      <- df_AICw
  fitdat_FP_tomerge    <- fitdat_FP
  fitdat_CP_tomerge    <- fitdat_CP
  fitdat_PH_tomerge    <- fitdat_PH
  fitdat_RL1_tomerge   <- fitdat_RL1
  fitdat_RL2_tomerge   <- fitdat_RL2
  fitdat_RLH1_tomerge  <- fitdat_RLH1
  fitdat_RLH2_tomerge  <- fitdat_RLH2
  # Rename the columns in df_AIC using the mapping
  colnames(df_AICW_tomerge)   <- column_mapping[match(colnames(df_AICw), names(column_mapping))]
  # and for FP
  suffix_fitdat_FP            <- ".fitdat_FP"
  colnames_fitdat_FP          <- paste0(colnames(fitdat_FP), suffix_fitdat_FP)
  colnames(fitdat_FP_tomerge) <- colnames_fitdat_FP
  # and for CP
  suffix_fitdat_CP            <- ".fitdat_CP"
  colnames_fitdat_CP          <- paste0(colnames(fitdat_CP), suffix_fitdat_CP)
  colnames(fitdat_CP_tomerge) <- colnames_fitdat_CP
  # and for PH
  suffix_fitdat_PH            <- ".fitdat_PH"
  colnames_fitdat_PH          <- paste0(colnames(fitdat_PH), suffix_fitdat_PH)
  colnames(fitdat_PH_tomerge) <- colnames_fitdat_PH
  # RL1
  suffix_fitdat_RL1           <- ".fitdat_RL1"
  colnames_fitdat_RL1         <- paste0(colnames(fitdat_RL1), suffix_fitdat_RL1)
  colnames(fitdat_RL1_tomerge)<- colnames_fitdat_RL1
  # RL2
  suffix_fitdat_RL2           <- ".fitdat_RL2"
  colnames_fitdat_RL2         <- paste0(colnames(fitdat_RL2), suffix_fitdat_RL2)
  colnames(fitdat_RL2_tomerge)<- colnames_fitdat_RL2
  # RLH1
  suffix_fitdat_RLH1          <- ".fitdat_RLH1"
  colnames_fitdat_RLH1        <- paste0(colnames(fitdat_RLH1), suffix_fitdat_RLH1)
  colnames(fitdat_RLH1_tomerge)<- colnames_fitdat_RLH1
  # RLH2
  suffix_fitdat_RLH2          <- ".fitdat_RLH2"
  colnames_fitdat_RLH2        <- paste0(colnames(fitdat_RLH2), suffix_fitdat_RLH2)
  colnames(fitdat_RLH2_tomerge)<- colnames_fitdat_RLH2
  
  
  # merge
  empdat_AICw_mods <- merge(empdat,  df_AICW_tomerge, by = "user_num", all.x = TRUE) %>% 
    merge(fitdat_FP_tomerge, by.x = "user_num", by.y = "user_num.fitdat_FP", all.x = TRUE) %>%
    merge(fitdat_CP_tomerge, by.x = "user_num", by.y = "user_num.fitdat_CP", all.x = TRUE) %>%
    merge(fitdat_PH_tomerge, by.x ="user_num", by.y = "user_num.fitdat_PH", all.x = TRUE) %>%
    merge(fitdat_RL1_tomerge, by.x = "user_num", by.y = "user_num.fitdat_RL1", all.x = TRUE) %>%
    merge(fitdat_RL2_tomerge, by.x = "user_num", by.y = "user_num.fitdat_RL2", all.x = TRUE) %>%
    merge(fitdat_RLH1_tomerge, by.x = "user_num", by.y = "user_num.fitdat_RLH1", all.x = TRUE) %>% 
    merge(fitdat_RLH2_tomerge, by.x = "user_num",  by.y = "user_num.fitdat_RLH2", all.x = TRUE) 
  # save empdat_AICw
  write_csv(empdat_AICw_mods, str_c(empdat_path,  format(Sys.time(), "%y%m%d_"), data_name_string,  "_AICw.csv"))
  
}

