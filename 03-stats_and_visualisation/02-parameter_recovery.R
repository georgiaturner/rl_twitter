


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------                  SET UP         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


rm(list = ls())
seed <- 1
set.seed(seed)

library(here)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)
setwd(here("03-stats_and_visualisation"))
source('00-functions.R')

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------             LOAD DATA           ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

########################################
##### load data
########################################

###### define paths
fitdat_path       <- str_c("./../../data_processed/Twitter/fit/")
simdat_path       <- str_c("./../../data_processed/Twitter/simgen/241104_241104_241104_240228_AHdisc_cleaned_preproc_AICw/");

### Load simulated data

# FP
################
fitdat_name_FP80   <- "241104-2020FP"
fit_FP80           <- read_csv(str_c(fitdat_path, fitdat_name_FP80,"/241104-2052FP1.csv"))
simgen_FP80        <- read_csv(str_c(simdat_path, fitdat_name_FP80, ".csv")) %>% filter(post_num_pic==1)
# CP
################
fitdat_name_CP80   <- "241104-2023CP"
fit_CP80           <- read_csv(str_c(fitdat_path, fitdat_name_CP80, "/241104-2131CP1.csv"))
simgen_CP80        <- read_csv(str_c(simdat_path, fitdat_name_CP80, ".csv")) %>% filter(post_num_pic==1)
# PH
################
fitdat_name_PH80   <- "241104-2031PH"
fit_PH80           <- read_csv(str_c(fitdat_path, fitdat_name_PH80, "/241104-2056PH1.csv"))
simgen_PH80        <- read_csv(str_c(simdat_path, fitdat_name_PH80,  ".csv"))%>% filter(post_num_pic==1)
# RL1
################
fitdat_name_RL180  <- "241104-2025RL1"
fit_RL180          <- read_csv(str_c(fitdat_path, fitdat_name_RL180, "/241104-2110RL11.csv"))
simgen_RL180       <- read_csv(str_c(simdat_path, fitdat_name_RL180, ".csv"))%>% filter(post_num_pic==1)
# RL2
################
fitdat_name_RL280  <- "241104-2027RL2"
fit_RL280          <- read_csv(str_c(fitdat_path, fitdat_name_RL280, "/241106-0958RL21.csv"))
simgen_RL280       <- read_csv(str_c(simdat_path, fitdat_name_RL280, ".csv"))%>% filter(post_num_pic==1)
# RLH1
fitdat_name_RLH180 <- "241104-2037RLH1"
fit_RLH180         <- read_csv(str_c(fitdat_path, fitdat_name_RLH180, "/241104-2228RLH11.csv")) 
simgen_RLH180      <- read_csv(str_c(simdat_path, fitdat_name_RLH180, ".csv")) %>% filter(post_num_pic==1)
# RLH2
fitdat_name_RLH280 <- "241104-2043RLH2"
fit_RLH280         <- read_csv(str_c(fitdat_path, fitdat_name_RLH280, "/241104-2249RLH21.csv")) 
simgen_RLH280      <- read_csv(str_c(simdat_path, fitdat_name_RLH280, ".csv")) %>% filter(post_num_pic==1)
  
### Load 1000 posts fitted

# FP
################
fitdat_name_FP1000   <- "241104-2020FP"
fit_FP1000           <- read_csv(str_c(fitdat_path, fitdat_name_FP1000,"/241104-2204FP1.csv"))
simgen_FP1000        <- read_csv(str_c(simdat_path, fitdat_name_FP1000, ".csv")) %>% filter(post_num_pic==1)
# CP
################
fitdat_name_CP1000   <- "241104-2023CP"
fit_CP1000           <- read_csv(str_c(fitdat_path, fitdat_name_CP1000, "/241105-0051CP1.csv"))
simgen_CP1000        <- read_csv(str_c(simdat_path, fitdat_name_CP1000, ".csv"))%>% filter(post_num_pic==1)
# Pure habit
################
fitdat_name_PH1000   <- "241104-2031PH"
fit_PH1000           <- read_csv(str_c(fitdat_path, fitdat_name_PH1000, "/241104-2147PH1.csv"))
simgen_PH1000        <- read_csv(str_c(simdat_path, fitdat_name_PH1000, ".csv"))%>% filter(post_num_pic==1)
# RL1
################
fitdat_name_RL11000 <- "241104-2025RL1"
fit_RL11000         <- read_csv(str_c(fitdat_path, fitdat_name_RL11000, "/241104-2337RL11.csv"))
simgen_RL11000      <- read_csv(str_c(simdat_path, fitdat_name_RL11000, ".csv"))%>% filter(post_num_pic==1)
# RL2
################
fitdat_name_RL21000 <- "241104-2027RL2"
fit_RL21000         <- read_csv(str_c(fitdat_path, fitdat_name_RL21000, "/241105-0626RL21.csv"))
simgen_RL21000      <- read_csv(str_c(simdat_path, fitdat_name_RL21000, ".csv"))%>% filter(post_num_pic==1)
# RLH1
fitdat_name_RLH11000 <- "241104-2037RLH1"
fit_RLH11000         <- read_csv(str_c(fitdat_path, fitdat_name_RLH11000, "/241105-1229RLH11.csv")) 
simgen_RLH11000      <- read_csv(str_c(simdat_path, fitdat_name_RLH11000, ".csv")) %>% filter(post_num_pic==1)
# RLH2
fitdat_name_RLH21000 <- "241104-2043RLH2"
fit_RLH21000         <- read_csv(str_c(fitdat_path, fitdat_name_RLH21000, "/241105-1813RLH21.csv")) 
simgen_RLH21000      <- read_csv(str_c(simdat_path, fitdat_name_RLH21000, ".csv")) %>% filter(post_num_pic==1)



#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------              PLOT DATA          ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


#################
##### do separate plots
#################

plt_parrec(fit_FP80, simgen_FP80, "fixedpol", "policy", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_FP1000, simgen_FP1000, "fixedpol", "policy", "fitted parameters", "original simulated parameters", 1000)

plt_parrec(fit_CP80, simgen_CP80, "changpol", "a", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_CP80, simgen_CP80, "changpol", "b", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_CP80, simgen_CP80, "changpol", "c", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_CP1000, simgen_CP1000, "changpol", "a", "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_CP1000, simgen_CP1000, "changpol", "b", "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_CP1000, simgen_CP1000, "changpol", "c", "fitted parameters", "original simulated parameters", 1000)

plt_parrec(fit_PH80, simgen_PH80, "purehab", "alpha_action", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_PH1000, simgen_PH1000, "purehab", "alpha_action", "fitted parameters", "original simulated parameters", 1000)

plt_parrec(fit_RL180, simgen_RL180, "RL1", "alpha", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RL180, simgen_RL180, "RL1", "cost", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RL11000, simgen_RL11000, "RL1", "alpha", "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RL11000, simgen_RL11000, "RL1", "cost", "fitted parameters", "original simulated parameters", 1000)

plt_parrec(fit_RL280, simgen_RL280, "RL2", "alpha_P", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RL280, simgen_RL280, "RL2", "alpha_N", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RL280, simgen_RL280, "RL2", "cost", "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RL21000, simgen_RL21000, "RL2", "alpha_P", "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RL21000, simgen_RL21000, "RL2", "alpha_N", "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RL21000, simgen_RL21000, "RL2", "cost", "fitted parameters", "original simulated parameters", 1000)

plt_parrec(fit_RLH180, simgen_RLH180, "RLH1", "alpha",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH180, simgen_RLH180, "RLH1", "cost",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH180, simgen_RLH180, "RLH1", "alpha_action",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH180, simgen_RLH180, "RLH1", "stickiness_weight",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH11000, simgen_RLH11000, "RLH1", "alpha",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH11000, simgen_RLH11000, "RLH1", "cost",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH11000, simgen_RLH11000, "RLH1", "alpha_action",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH11000, simgen_RLH11000, "RLH1", "stickiness_weight",  "fitted parameters", "original simulated parameters", 1000)

plt_parrec(fit_RLH280, simgen_RLH280, "RLH2", "alpha_P",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH280, simgen_RLH280, "RLH2", "alpha_N",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH280, simgen_RLH280, "RLH2", "cost",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH280, simgen_RLH280, "RLH2", "alpha_action",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH280, simgen_RLH280, "RLH2", "stickiness_weight",  "fitted parameters", "original simulated parameters", 80)
plt_parrec(fit_RLH21000, simgen_RLH21000, "RLH2", "alpha_P",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH21000, simgen_RLH21000, "RLH2", "alpha_N",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH21000, simgen_RLH21000, "RLH2", "cost",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH21000, simgen_RLH21000, "RLH2", "alpha_action",  "fitted parameters", "original simulated parameters", 1000)
plt_parrec(fit_RLH21000, simgen_RLH21000, "RLH2", "stickiness_weight",  "fitted parameters", "original simulated parameters", 1000)

#################
##### all together in a plot
#################

#### for 80

# Define columns 
columns80 <- list(
  FP80    = "policy",
  CP80    = c('a', 'b', 'c'),
  PH80    = 'alpha_action',
  RL180   = c('alpha', 'cost'),
  RL280   = c('alpha_P', 'alpha_N', 'cost'),
  RLH180  = c('alpha', 'cost', 'alpha_action', 'stickiness_weight'),
  RLH280  = c('alpha_P', 'alpha_N', 'cost', 'alpha_action', 'stickiness_weight')
)

fit_columns80    <- columns80
simgen_columns80 <- columns80

# Create an empty list to store correlation matrices
correlation_matrices80 <- list()

# Calculate correlations for each pair of fit and simgen data frames
for (key in names(fit_columns80)) {
  fit_cols80    <- fit_columns80[[key]]
  simgen_cols80 <- simgen_columns80[[key]]
  
  correlation_matrices80[[key]] <- calculate_correlations(
    fit_df      = get(paste("fit_", key, sep="")),
    simgen_df   = get(paste("simgen_", key, sep="")),
    fit_cols    = fit_cols80,
    simgen_cols = simgen_cols80
  )
}

# Plot each correlation matrix

FP80   <- plot_correlation_grid(correlation_matrices80[["FP80"]])
CP80   <- plot_correlation_grid(correlation_matrices80[["CP80"]])
PH80   <- plot_correlation_grid(correlation_matrices80[["PH80"]])
RL180  <- plot_correlation_grid(correlation_matrices80[["RL180"]])
RL280  <- plot_correlation_grid(correlation_matrices80[["RL280"]])
RLH180 <- plot_correlation_grid(correlation_matrices80[["RLH180"]])
RLH280 <- plot_correlation_grid(correlation_matrices80[["RLH280"]])



#### for 1000

# Define columns 
columns1000 <- list(
  FP1000    = "policy",
  CP1000    = c('a', 'b', 'c'),
  PH1000    = 'alpha_action',
  RL11000   = c('alpha', 'cost'),
  RL21000   = c('alpha_P', 'alpha_N', 'cost'),
  RLH11000  = c('alpha', 'cost', 'alpha_action', 'stickiness_weight'),
  RLH21000  = c('alpha_P', 'alpha_N', 'cost', 'alpha_action', 'stickiness_weight')
)

fit_columns1000    <- columns1000
simgen_columns1000 <- columns1000

# Create an empty list to store correlation matrices
correlation_matrices1000 <- list()

# Calculate correlations for each pair of fit and simgen data frames
for (key in names(fit_columns1000)) {
  fit_cols1000    <- fit_columns1000[[key]]
  simgen_cols1000 <- simgen_columns1000[[key]]
  
  correlation_matrices1000[[key]] <- calculate_correlations(
    fit_df      = get(paste("fit_", key, sep="")),
    simgen_df   = get(paste("simgen_", key, sep="")),
    fit_cols    = fit_cols1000,
    simgen_cols = simgen_cols1000
  )
}

# Plot each correlation matrix

FP1000   <- plot_correlation_grid(correlation_matrices1000[["FP1000"]])
CP1000   <- plot_correlation_grid(correlation_matrices1000[["CP1000"]])
PH1000   <- plot_correlation_grid(correlation_matrices1000[["PH1000"]])
RL11000  <- plot_correlation_grid(correlation_matrices1000[["RL11000"]])
RL21000  <- plot_correlation_grid(correlation_matrices1000[["RL21000"]])
RLH11000 <- plot_correlation_grid(correlation_matrices1000[["RLH11000"]])
RLH21000 <- plot_correlation_grid(correlation_matrices1000[["RLH21000"]])


#################
##### Figure S2
#################

FigS2 <- grid.arrange(FP80, CP80, PH80, RL180, RL280, RLH180, RLH280,
             FP1000, CP1000, PH1000, RL11000, RL21000, RLH11000, RLH21000,
             heights = c(1,1),
             nrow = 2)



ggsave("figures/FigS2.png", plot = FigS2, width = 21, height = 10, units = "in")





