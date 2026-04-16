



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
#source('00-functions_with_Lindstrom.R')
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#--------------------       AICW FOR MODEL RECOVERY      ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

# load fitted mods

##### for the Norm80
modnames_Norm80 <- list("Norm80_SimFP",
                        "Norm80_SimCP",
                        "Norm80_SimHP",
                        "Norm80_SimRL1",
                        "Norm80_SimRL2",
                        "Norm80_SimRLH1",
                        "Norm80_SimRLH2")
plot_modelrecovery80 <- plot_modrec(modnames_Norm80, fig_title = "Model Recovery for 80 posts")
modnames_Norm1000 <- list("Norm1000_SimFP",
                          "Norm1000_SimCP",
                          "Norm1000_SimHP",
                          "Norm1000_SimRL1",
                          "Norm1000_SimRL2",
                          "Norm1000_SimRLH1",
                          "Norm1000_SimRLH2")

plot_modelrecovery1000 <- plot_modrec(modnames_Norm1000, fig_title = "Model Recovery for 1000 posts")


# Arrange Fig S2

FigS2_layout_matrix <- rbind(
  c(1,2)
)

FigS2 <- grid.arrange(plot_modelrecovery80, 
                      plot_modelrecovery1000, 
                      nrow = 1,
                      layout_matrix = FigS2_layout_matrix, 
                      widths = c(1, 1))

ggsave("figures/FigS2.pdf", plot = FigS2, width = 21, height = 10, units = "in")


### FigS2 source data

data_modrec80   <- plot_modrec(modnames_Norm80, fig_title = "Model Recovery for 80 posts", to_return = "df_AICws")
data_FigS2a     <- get_modrec_sourcedata(data_modrec80)
data_modrec1000 <- plot_modrec(modnames_Norm1000, fig_title = "Model Recovery for 1000 posts", to_return = "df_AICws")
data_FigS2b     <- get_modrec_sourcedata(data_modrec1000)

write_csv(data_FigS2a, "./../03-stats_and_visualisation/source_data/data_figS2a.csv")
write_csv(data_FigS2b, "./../03-stats_and_visualisation/source_data/data_figS2b.csv")

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
saveRDS(AHconf_justLikes_dat, file = str_c("figures/AHconf_justLikes_modcomp.rds")) # save this one as RDS so can reload in 03-results_stats
#### Fig 3a Source Data
data_fig3a <- get_modcomp_source_data("AHconf_justLikes")
write_csv(data_fig3a, "./../03-stats_and_visualisation/source_data/data_fig3a.csv")


##### Fig S10 with Lindstrom model ##### - to use this one have to go in to the functions script and uncomment all the Lindstrom parts - these can be located using find & replace with 'Lind'
AHconf_justLikes_withLind_dat <-  generate_AICw_plot("AHconf_justLikes_WithLindstromModel", to_return = "AICw_plot")
AHconf_justLikes_withLind_dat <-  generate_AICw_plot("AHconf_justLikes", to_return = "AICw_plot")
#ggsave("figures/FigS10_Lind_All.pdf", plot = AHconf_justLikes_withLind_dat, width = 13, height = 10, units = "in")
#ggsave("figures/FigS10_FP.pdf", plot = AHconf_justLikes_withLind_dat, width = 11, height = 10, units = "in")
### get numbers
#AHconf_Lind    <- load_modfits("AHconf_justLikes_WithLindstromModel")
#df_AICw_LindAll <- make_df_AICw(AHconf_Lind)
#sapply(df_AICw_LindAll, function(x) {
#  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
#})

##### Fig S10 Source Data
#data_figS10a      <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10b_RLH1  <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10b_RLH2  <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
 
data_figS10c_FP    <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10c_CP    <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10c_HP    <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10c_RL1   <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10c_RL2   <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")
data_figS10c_RLH1  <- get_modcomp_source_data("AHconf_justLikes_WithLindstromModel")

write_csv(data_figS10b_RLH1, "./../03-stats_and_visualisation/source_data/data_figS10b_RLH1.csv")
write_csv(data_figS10b_RLH2, "./../03-stats_and_visualisation/source_data/data_figS10b_RLH2.csv")
write_csv(data_figS10c_FP, "./../03-stats_and_visualisation/source_data/data_figS10c_FP.csv")
write_csv(data_figS10c_CP, "./../03-stats_and_visualisation/source_data/data_figS10c_CP.csv")
write_csv(data_figS10c_HP, "./../03-stats_and_visualisation/source_data/data_figS10c_HP.csv")
write_csv(data_figS10c_RL1, "./../03-stats_and_visualisation/source_data/data_figS10c_RL1.csv")
write_csv(data_figS10c_RL2, "./../03-stats_and_visualisation/source_data/data_figS10c_RL2.csv")


##### Fig S7
AHconf_justRTs_dat      <- generate_AICw_plot("AHconf_justRTs", to_return = "AICw_plot")
AHconf_LikesPlusRTs_dat <- generate_AICw_plot("AHconf_LikesPlusRTs", to_return = "AICw_plot")

## get values for plots too 
fitted_dat_justRTs <- load_modfits("AHconf_justRTs")
df_AICw_justRTs <- make_df_AICw(fitted_dat_justRTs)
sapply(df_AICw_justRTs, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
fitted_dat_LikesPlusRTs <- load_modfits("AHconf_LikesPlusRTs")
df_AICw_LikesPlusRTs <- make_df_AICw(fitted_dat_LikesPlusRTs)
sapply(df_AICw_LikesPlusRTs, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})

FigS7_layout_matrix <- rbind(
  c(1),
  c(NA),
  c(2)# First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS7 <- grid.arrange(
  AHconf_LikesPlusRTs_dat,
  AHconf_justRTs_dat,
  layout_matrix = FigS7_layout_matrix,
  heights = c(1, 0.2, 1)
)

ggsave("figures/FigS7.pdf", plot = FigS7, width = 9, height = 16, units = "in")
#### Fig S7 Source Data
data_figS7a <- get_modcomp_source_data("AHconf_LikesPlusRTs")
write_csv(data_figS7a, "./../03-stats_and_visualisation/source_data/data_figS7a.csv")
data_figS7b <- get_modcomp_source_data("AHconf_justRTs")
write_csv(data_figS7b, "./../03-stats_and_visualisation/source_data/data_figS7b.csv")


##### Fig S8 ##### 

AHconf_NormalDistribition_dat <- generate_AICw_plot("AHconf_justLikes_NormalDist", to_return = "AICw_plot")
AHconf_GammaDistributiondat   <- generate_AICw_plot("AHconf_justLikes_GammaDist", to_return = "AICw_plot")

## get values for plots too 
fitted_dat_Norm <- load_modfits("AHconf_justLikes_NormalDist")
df_AICw_Norm <- make_df_AICw(fitted_dat_Norm)
sapply(df_AICw_Norm, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
fitted_dat_Gam <- load_modfits("AHconf_justLikes_GammaDist")
df_AICw_Gam <- make_df_AICw(fitted_dat_Gam)
sapply(df_AICw_Gam, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})


FigS8_layout_matrix <- rbind(
  c(1),
  c(NA),
  c(2)# First row: plot 1 spans two columns, plot 2 goes in the third column
)

FigS8 <- grid.arrange(
  AHconf_NormalDistribition_dat,
  AHconf_GammaDistributiondat,
  layout_matrix = FigS8_layout_matrix,
  heights = c(1, 0.2, 1)
)

ggsave("figures/FigS8.pdf", plot = FigS8, width = 9, height = 16, units = "in")

### Fig S8 source data
data_figS8a <- get_modcomp_source_data("AHconf_justLikes_NormalDist")
write_csv(data_figS8a, "./../03-stats_and_visualisation/source_data/data_fig8a.csv")
data_figS8b <- get_modcomp_source_data("AHconf_justLikes_GammaDist")
write_csv(data_figS8b, "./../03-stats_and_visualisation/source_data/data_fig8b.csv")



##### Fig S9 ##### 

AHconf_Initial_Rmean     <- generate_AICw_plot("AHconf_Initial_Rmean", to_return = "AICw_plot")
AHconf_Initial_R20mean   <- generate_AICw_plot("AHconf_Initial_R20mean", to_return = "AICw_plot")
AHconf_Initial_Rmedian   <- generate_AICw_plot("AHconf_Initial_Rmedian", to_return = "AICw_plot")
AHconf_Initial_R20median <- generate_AICw_plot("AHconf_Initial_R20median", to_return = "AICw_plot")
AHconf_Initial_Pmean     <- generate_AICw_plot("AHconf_Initial_Pmean", to_return = "AICw_plot")
AHconf_Initial_P20mean   <- generate_AICw_plot("AHconf_Initial_P20mean", to_return = "AICw_plot")
AHconf_Initial_Pmedian   <- generate_AICw_plot("AHconf_Initial_Pmedian", to_return = "AICw_plot")
AHconf_Initial_P20median <- generate_AICw_plot("AHconf_Initial_P20mean", to_return = "AICw_plot")

FigS9_layout_matrix <- rbind(
  c(1, NA, 2, NA, 3, NA, 4),
  c(NA, NA, NA, NA, NA, NA, NA),
  c(5, NA, 6, NA, 7, NA, 8)# First row: plot 1 spans two columns, plot 2 goes in the third column
)
FigS9 <- grid.arrange(
  AHconf_Initial_Rmean,
  AHconf_Initial_R20mean,
  AHconf_Initial_Rmedian,
  AHconf_Initial_R20median,
  AHconf_Initial_Pmean,
  AHconf_Initial_P20mean,
  AHconf_Initial_Pmedian,
  AHconf_Initial_P20median,
  layout_matrix = FigS9_layout_matrix,
  widths = c(5, 0.1, 5, 0.1, 5, 0.1, 5),
  heights = c(1, 0.2, 1)
)
ggsave("figures/FigS9.pdf", plot = FigS9, width = 30, height = 16, units = "in")

# get numbers
AHconf_Initial_Original <- load_modfits("AHconf_justLikes")
AHconf_Initial_Rmean <- load_modfits("AHconf_Initial_Rmean")
AHconf_Initial_R20mean <- load_modfits("AHconf_Initial_R20mean")
AHconf_Initial_R20median <- load_modfits("AHconf_Initial_R20median")
AHconf_Initial_Rmedian <- load_modfits("AHconf_Initial_Rmedian")
AHconf_Initial_Pmean <- load_modfits("AHconf_Initial_Pmean")
AHconf_Initial_P20mean <- load_modfits("AHconf_Initial_P20mean")
AHconf_Initial_Pmedian <- load_modfits("AHconf_Initial_Pmedian")
AHconf_Initial_P20median <- load_modfits("AHconf_Initial_P20median")


df_AICw_Rmean <- make_df_AICw(AHconf_Initial_Rmean)
sapply(df_AICw_Rmean, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_R20mean <- make_df_AICw(AHconf_Initial_R20mean)
sapply(df_AICw_R20mean, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_Rmedian <- make_df_AICw(AHconf_Initial_Rmedian)
sapply(df_AICw_Rmedian, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_R20median <- make_df_AICw(AHconf_Initial_R20median)
sapply(df_AICw_R20median, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_Pmean <- make_df_AICw(AHconf_Initial_Pmean)
sapply(df_AICw_Pmean, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_P20mean <- make_df_AICw(AHconf_Initial_P20mean)
sapply(df_AICw_P20mean, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_Pmedian <- make_df_AICw(AHconf_Initial_Pmedian)
sapply(df_AICw_Pmedian, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})
df_AICw_P20median <- make_df_AICw(AHconf_Initial_P20median)
sapply(df_AICw_P20median, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
})

### Fig S9 source data
data_figS9 <- data.frame(get_modcomp_source_data("AHconf_Initial_Rmean"),
                         get_modcomp_source_data("AHconf_Initial_R20mean"),
                         get_modcomp_source_data("AHconf_Initial_R20median"),
                         get_modcomp_source_data("AHconf_Initial_Rmedian"),
                         get_modcomp_source_data("AHconf_Initial_Pmean"),
                         get_modcomp_source_data("AHconf_Initial_P20mean"),
                         get_modcomp_source_data("AHconf_Initial_Pmedian"),
                         get_modcomp_source_data("AHconf_Initial_P20median")
                         )
write_csv(data_figS9, "./../03-stats_and_visualisation/source_data/data_figS9.csv")


############## REVIEWER-RESPONSE RESULTS - R3.2: check correlations between fitted alpha with different initializations

df_alpha <- data.frame(
  OriginalInitialisation_alph   = AHconf_Initial_Original$fitdat_RLH1$alpha_reward,
  PMean_Initialisation_alph     = AHconf_Initial_Pmean$fitdat_RLH1$alpha,
  RMean_Initialisation_alph     = AHconf_Initial_Rmean$fitdat_RLH1$alpha,
  P20Mean_Initialisation_alph   = AHconf_Initial_P20mean$fitdat_RLH1$alpha,
  R20Mean_Initialisation_alph   = AHconf_Initial_R20mean$fitdat_RLH1$alpha,
  PMedian_Initialisation_alph   = AHconf_Initial_Pmedian$fitdat_RLH1$alpha,
  RMedian_Initialisation_alph   = AHconf_Initial_Rmedian$fitdat_RLH1$alpha,
  P20Median_Initialisation_alph = AHconf_Initial_P20median$fitdat_RLH1$alpha,
  R20Median_Initialisation_alph = AHconf_Initial_R20median$fitdat_RLH1$alpha,
  
  OriginalInitialisation_alphP  = AHconf_Initial_Original$fitdat_RLH2$alpha_P,
  PMean_Initialisation_alphP    = AHconf_Initial_Pmean$fitdat_RLH2$alpha_P,
  RMean_Initialisation_alphP    = AHconf_Initial_Rmean$fitdat_RLH2$alpha_P,
  P20Mean_Initialisation_alphP  = AHconf_Initial_P20mean$fitdat_RLH2$alpha_P,
  R20Mean_Initialisation_alphP  = AHconf_Initial_R20mean$fitdat_RLH2$alpha_P,
  PMedian_Initialisation_alphP  = AHconf_Initial_Pmedian$fitdat_RLH2$alpha_P,
  RMedian_Initialisation_alphP  = AHconf_Initial_Rmedian$fitdat_RLH2$alpha_P,
  P20Median_Initialisation_alphP  = AHconf_Initial_P20median$fitdat_RLH2$alpha_P,
  R20Median_Initialisation_alphP  = AHconf_Initial_R20median$fitdat_RLH2$alpha_P,
  
  OriginalInitialisation_alphN  = AHconf_Initial_Original$fitdat_RLH2$alpha_N,
  PMean_Initialisation_alphN    = AHconf_Initial_Pmean$fitdat_RLH2$alpha_N,
  RMean_Initialisation_alphN    = AHconf_Initial_Rmean$fitdat_RLH2$alpha_N,
  P20Mean_Initialisation_alphN  = AHconf_Initial_P20mean$fitdat_RLH2$alpha_N,
  R20Mean_Initialisation_alphN  = AHconf_Initial_R20mean$fitdat_RLH2$alpha_N,
  PMedian_Initialisation_alphN  = AHconf_Initial_Pmedian$fitdat_RLH2$alpha_N,
  RMedian_Initialisation_alphN  = AHconf_Initial_Rmedian$fitdat_RLH2$alpha_N,
  P20Median_Initialisation_alphN  = AHconf_Initial_P20median$fitdat_RLH2$alpha_N,
  R20Median_Initialisation_alphN  = AHconf_Initial_R20median$fitdat_RLH2$alpha_N
)

# check statistical correlation between alpha in original initialisation and each of the variations. 
mod <- lm(OriginalInitialisation_alphN  ~ RMedian_Initialisation_alphN, data = df_alpha )
summary(mod)

#--------------------   ADD INFO BACK TO EMPDAT FOR SAVING    ---------------------------#

########################################
##### load empirical data
########################################


which_empdat    <- "AHconf_justLikes" # or AHdisc_justLikes

if (which_empdat == "AHconf_justLikes") {
  data_name_string <- "251105_251105_240416_AHconf_cleaned_preproc"
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
fitdat_HP       <- fitted_dat$fitdat_HP
fitdat_RL1      <- fitted_dat$fitdat_RL1
fitdat_RL2      <- fitted_dat$fitdat_RL2
fitdat_RLH1     <- fitted_dat$fitdat_RLH1
fitdat_RLH2     <- fitted_dat$fitdat_RLH2


if (which_empdat == "AHconf_justLikes" | which_empdat == "AHdisc_justLikes") {
  ### here we add the AICw info back into empdat to save a new df with all the info which can be used 
  # for plotting in the '03-results_stats' script saved in this folder
  
  # rename columns for entry into empdat
  column_mapping <- c("user_num"         = "user_num", 
                      "a_fitdat_FP"      = "AICw_FP",
                      "b_fitdat_CP"      = "AICw_CP", 
                      "c_fitdat_HP"      = "AICw_HP",
                      "d_fitdat_RL1"     = "AICw_RL1",
                      "e_fitdat_RL2"     = "AICw_RL2",
                      "f_fitdat_RLH1"    = "AICw_RLH1",
                      "g_fitdat_RLH2"    = "AICw_RLH2")
  
  df_AICW_tomerge      <- df_AICw
  fitdat_FP_tomerge    <- fitdat_FP
  fitdat_CP_tomerge    <- fitdat_CP
  fitdat_HP_tomerge    <- fitdat_HP
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
  # and for HP
  suffix_fitdat_HP            <- ".fitdat_HP"
  colnames_fitdat_HP          <- paste0(colnames(fitdat_HP), suffix_fitdat_HP)
  colnames(fitdat_HP_tomerge) <- colnames_fitdat_HP
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
    merge(fitdat_HP_tomerge, by.x ="user_num", by.y = "user_num.fitdat_HP", all.x = TRUE) %>%
    merge(fitdat_RL1_tomerge, by.x = "user_num", by.y = "user_num.fitdat_RL1", all.x = TRUE) %>%
    merge(fitdat_RL2_tomerge, by.x = "user_num", by.y = "user_num.fitdat_RL2", all.x = TRUE) %>%
    merge(fitdat_RLH1_tomerge, by.x = "user_num", by.y = "user_num.fitdat_RLH1", all.x = TRUE) %>% 
    merge(fitdat_RLH2_tomerge, by.x = "user_num",  by.y = "user_num.fitdat_RLH2", all.x = TRUE) 
  # save empdat_AICw
  write_csv(empdat_AICw_mods, str_c(empdat_path,  format(Sys.time(), "%y%m%d_"), data_name_string,  "_AICw.csv"))
  
}


