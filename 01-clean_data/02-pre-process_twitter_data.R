
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#-------------------------------------    01-PRE-PROCESS_TWITTER_DATA.R   --------------------------------------------------#
#-------------------------------------                                    --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# This script preprocesses data from the Authentic Happiness dataset, scraped from Twitter, 
# after it has gone through initial cleaning. 
#                                                                                                                            
# It loads the datasets that have already been cleaned by the script '01-clean_twitter_data.R' in the same folder.
# It preprocesses the datasets further by applying exclusion criteria of time periods and number of posts.

# By Georgia Turner, 2024 < georgianjt@gmail.com >

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#-----------------------------------------  NAVIGATE TO & SETUP WORKSPACE --------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

rm(list=ls())  
set.seed(1)


library(here)
library(tidyverse)
library(lubridate)
library(gt)

setwd(here("01-clean_data"))
source('00-functions.R')


#####################################################################################
##### LOAD DATASETS
#####################################################################################


data_path        <- "./../../../../../../data/2022_EichstaedtTwitter/AH/"

data_type <- "AHconf" # whether to load discovery or confirmatory sample

if (data_type == "AHdisc") {
  moddat_name   <- "241104_240228_AHdisc_cleaned" # discovery sample
} else if (data_type == "AHconf") {
  moddat_name   <- "241104_240416_AHconf_cleaned"           # confirmatory sample
}

moddat        <- read_csv(paste(data_path,  moddat_name, ".csv", sep = ""));

#####################################################################################
##### Add in other variables
#####################################################################################

moddat <- moddat %>%
  group_by(user_num) %>%
  ### add in their oldest post so we can calculate time spent on Twitter for the final analyses
  mutate(t_firstpost = first(datetime)) 


#####################################################################################
##### FILTER DATASET ACCORDING TO VARIOUS CRITERIA
#####################################################################################

# define number of months for which will keep each person's data, and minimum number of posts to retain a person's data
nmonths_total <- 12
nposts_min    <- 80

#####################################################################################
### FILTER BY TIME
# filter training dataset to contain only the 'nmonths_total' months centred around when users completed the questionnaire.

moddat_qnr  <- filt_around_qnr(moddat, nmonths_total, "timeofquestionnaire_local")

# have a look at number of posts per user in the new truncated dataset
moddat_qnr_timelim_count <- moddat_qnr %>%
  group_by(user_num) %>%
  summarise(nposts = n())
moddat_qnr_timelim_count

visualise_npostdist(moddat_qnr_timelim_count, str_c("number of months = ", nmonths_total), bwidth = 10)

#####################################################################################
### FILTER BY NUMBER OF POSTS
##### filter to participants with > 'nposts_min' posts

moddat_filt   <- filt_by_nposts(moddat_qnr, nposts_min)

#####################################################################################
##### Add record of preprocessing parameters
#####################################################################################

moddat_filt$min_userposts <- nposts_min
moddat_filt$nmonths_total <- nmonths_total

#####################################################################################
##### SAVE PREPROCESSED DATASET
#####################################################################################

write_csv(moddat_filt,
          str_c(data_path, format(Sys.time(),  "%y%m%d_"), # save with date / time to not overwrite previous versions
                moddat_name,
                "_preproc.csv"))

