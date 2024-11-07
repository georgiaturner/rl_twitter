#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#----------------------------------------    01-CLEAN_TWITTER_DATA.R        ------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# This script cleans data from the Authentic Happiness dataset, scraped from Twitter, 
# and puts it into a format which can be used directly with the computational models.                                                   
#                                                                                                                            
# Note that this script provides the MINIMAL pre-processing necessary to put the code in the correct format,
# and make models converge.
# Further pre-processing decisions (e.g. limiting to a year-long time period) are added in the script saved in same folder, 
# '02-pre-process_twitter_data.R'.

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


#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#-----------------------------------------     IMPORT AH SAMPLE           --------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# if originally creating the discovery or confirmatory sample, set either 'making_AHdisc' or 'making_AHconf' true respectively
# otherwise make them both FALSE and skip to next step.

data_path        <- "./../../../../../../data/2022_EichstaedtTwitter/AH/" # input wherever raw data is kept

############

making_AHdisc   <- FALSE
making_AHconf   <- FALSE


if (making_AHdisc) {
  
  ### import entire dataset
  data_name_string <- "20230803_entire_dataset"
  raw_df_all       <- read_csv(paste(data_path, data_name_string, ".csv", sep = ""));
  
  ### filter to only users with a certain number of posts, from which will draw the subset
  minposts         <- 500;
  filt_raw_df_all  <- raw_df_all %>%
    group_by(hashed_user_id) %>%
    filter(n() >= minposts) %>% 
    ungroup()
  
  ### get list of unique users and count
  unique_users     <- unique(filt_raw_df_all$hashed_user_id)
  length(unique_users)
  
  ### import 1000 subset that have already looked at, as will make the discovery sample 3000 people including these.
  subset1000_name <- "2023-08-06_AHrandsubset1000"
  subset1000      <- read_csv(paste(data_path, subset1000_name, ".csv", sep = ""));
  
  # get the rest of the df that do not include the 1000, to get 2000 more and then add them together
  filt_raw_df_all_new <- filter(filt_raw_df_all, !(hashed_user_id %in% subset1000$hashed_user_id))
  
  ### get list of unique users and count
  unique_users_new     <- unique(filt_raw_df_all_new$hashed_user_id)
  length(unique_users_new)
  
  ### randomly select subset for initial modelling.
  extra_subset_size      <- 2000
  subset_size            <- extra_subset_size + 1000
  # get subset
  user_subset_new      <- sample(unique_users_new, subset_size, replace = FALSE)
  length(user_subset_new)
  subset_df_new       <- subset(filt_raw_df_all_new, hashed_user_id %in% user_subset_new)
  
  ### combine new with old subset
  subset3000  <- rbind(subset_df_new, subset1000)
  subset_size <- length(unique(subset3000$hashed_user_id))
  
  # save subset
  write_csv(subset3000, str_c(data_path, format(Sys.time(), "%y-%m-%d"), "_AHrandsubset", subset_size, ".csv"))
  
} else if (making_AHconf) {
  
  ### import entire dataset
  data_name_string <- "20230803_entire_dataset"
  raw_df_all       <- read_csv(paste(data_path, data_name_string, ".csv", sep = ""));
  
  ### get rid of the n=3000 subset
  subset3000_name  <- "2024-02-28_AHrandsubset3000"
  subset3000       <- read_csv(paste(data_path, subset3000_name, ".csv", sep = ""))

  # get the rest of the df that do not include the discovery sample (subset3000)
  raw_df_all_new <- filter(raw_df_all, !(hashed_user_id %in% subset3000$hashed_user_id))
  
  # save confirmatory dataset
  write_csv(raw_df_all_new, str_c(data_path, format(Sys.time(), "%y-%m-%d"), "_AHconf.csv"))
  
  
}


#####################################################################################################################
###### Import training dataset.
#####################################################################################################################

# choose whether discovery or confirmatory sample
#dat_name <- "240228_AHdisc" 
dat_name <- "240416_AHconf"

dat      <- read_csv(paste(data_path, dat_name, ".csv", sep = ""));
raw_df   <- dat; # create a new copy so that can always go back easily to the originally loaded one.

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#----------------------------------------            INSPECT DATA          -------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------------#
###### check basic attributes of raw dataset without making changes yet
#---------------------------------------------------------------------------------------------------------------------------#

#all columns have unique names
paste0(length(distinct(raw_df))/ncol(raw_df) * 100, "% of columns are unique")

#check columns for missing values. We expect missing values in 'favourite_count' and 'retweet_count' but not any other columns.
raw_df %>%
  sapply(., function(y) sum(length(which(is.na(y))))) %>%
  data.frame(.)

## check for weird irregularities before proceeding
if (any(is.na(raw_df$hashed_message_id)) | any(is.na(raw_df$created_at_utc)) | any(is.na(raw_df$hashed_user_id))) {
  stop("NAs in user ID, message ID or created_at_utc - check dataset before proceeding.")
}

if(any(!grepl("^20", raw_df$created_at_utc))) {
  stop("Some tweet dates not created in 20, error given that data was from 2000s - check dataset before proceeding.")
}

# Check if all retweets have 0 likes 
# check_result will be TRUE if condition is satisfied
RTs_have_no_likes    <- all(raw_df$is_retweet == FALSE | (raw_df$is_retweet == TRUE & is.na(raw_df$favorite_count)))
print(RTs_have_no_likes)
# Check if all retweets have 0 RTs
RTs_have_no_retweets <- all(raw_df$is_retweet == FALSE | (raw_df$is_retweet == TRUE & is.na(raw_df$retweet_count)))
print(RTs_have_no_retweets) # weirdly, the API stores the cumulative RTs for both the RT and original, but 0 of the likes. We just remove all RTs from this dataset.

# inspect follower numbers for each user
follower_nums <- raw_df %>% group_by(hashed_user_id) %>% slice(1) %>%ungroup() %>% dplyr::select(followers_count, dob)
format_tab <- function(tab, tittab) {
  gt(tab) %>%
    tab_header(title = tittab) %>%  # add a title to the table
    tab_style(style = list(cell_text(weight = "bold"),  # make the header cells bold
                           cell_fill(color = "#ECECEC")),  # add background color to the cells
              locations = cells_body())   # apply the style to the body cells
}
tab_descs <- follower_nums %>% format_tab("Descriptives: AH sample")
tab_descs

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#-----------------------------------------    CLEAN ANONYMISED DATASET    --------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------------#
###### create new dataset to change raw
#---------------------------------------------------------------------------------------------------------------------------#

raw_df_clean <- raw_df 

# re-organise so that posts for each user are in chronological order. 
raw_df_clean <- raw_df_clean %>% 
  mutate(datetime = ymd_hms(created_at_utc)) %>%  # this line may need to be changed to a different function depending on the format of created_at_utc in a given dataset
  arrange(hashed_user_id, datetime) ## in some datasets, created_at_utc is already in correct format, but we keep this here to be flexible to if it's not

# replaces numeric columns where 0s were initially coded as NAs for later computations, with 0s again
raw_df_clean <- raw_df_clean %>% 
  mutate_at(c("retweet_count","favorite_count"), ~replace_na(.,0)) %>% 
  filter(is_retweet == FALSE) # remove retweets as these do not store the numbers of likes.

#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#-----------------------------------------    CREATE MODELLING DF         --------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

colnames      <- c("user_num", 
                   "likes", 
                   "retweet_count", 
                   "t_post", 
                   "post_num_pic", 
                   "weekday",
                   "followers_count", 
                   "friends_count", 
                   "Authentic_Happiness_inventory", 
                   "gender", 
                   "datetime", 
                   "DOB", 
                   "timeofquestionnaire_local");
mod_df        <- as_tibble(matrix(nrow = nrow(raw_df_clean), ncol = length(colnames)), .name_repair = ~ colnames);

############### Fill the modelling tibble with data. ############### 

mod_df["user_num"]           <- raw_df_clean$hashed_user_id;
mod_df["likes"]              <- as.numeric(raw_df_clean$favorite_count);
mod_df["retweet_count"]      <- as.numeric(raw_df_clean$retweet_count);
raw_df_clean                 <- raw_df_clean %>% 
  mutate(lag_datetime = lag(datetime),
         t_post       = as.numeric(datetime - lag_datetime)) 
mod_df["t_post"]             <- raw_df_clean$t_post;
mod_df %<>% 
  group_by(user_num) %>% 
  mutate(post_num_pic = row_number()) %>% 
  ungroup();
## set t_post for first pic for each user to .004 - this is arbitrary and is never used.
mod_df[which(mod_df$post_num_pic==1),"t_post"] <- 0.004;
### add some other info to df, which is not needed for modelling but will be useful to have together with other info later, when we want to do correlation analyses etc
mod_df["weekday"]            <- lubridate::wday(raw_df_clean$datetime, label = TRUE);
mod_df["followers_count"]    <- raw_df_clean$followers_count;
mod_df["friends_count"]      <- raw_df_clean$friends_count;
mod_df["Authentic_Happiness_inventory"] <- raw_df_clean$Authentic_Happiness_inventory
mod_df["gender"]             <- raw_df_clean$gender
mod_df["datetime"]           <- raw_df_clean$datetime;
mod_df["DOB"]                <- raw_df_clean$dob;
mod_df["timeofquestionnaire_local"]<- raw_df_clean$timeofquestionnaire_local;

## check t_posts are not negative or nan
if (any(mod_df$t_post < 0, na.rm = TRUE)) {
  stop("At least one negative t_post")
}
if (any(is.nan(mod_df$t_post))){
  stop("NA t_posts - check dataset.")
}


############### Apply initial pre-processing. ############### 

# Note: the script 'pre-processing_twitter_data.R' has further preproc steps.
# these are only the necessary minimal preprocessing steps which are needed for the models to converge.

mod_df_preproc <- mod_df %>% rowid_to_column("row_index")

##########################################
## Group together groups of posts which were made in quick succession. 
# IMPORTANTLY, as this step can reduce the number of posts per user, it must come 
# BEFORE THE preprocessing step of deleting all users with less than 10 posts,
# so that any user that now loses enough of their posts to become below 10, is 
# then deleted.
## Crucially, the models do not converge if t_post is too small.
## Also, conceptually, due to the nature of Twitter, sometimes 
## groups of posts can be considered as one 'post'.
## This is especially because if posts come after a short latency, the preceding 
## post will likely not have yet accrued likes so they are unlikely to be
## reacting to the number of likes. Here, this is done by defining clumps 
## where consecutive posts all have a short t_post. These clumps are re-defined as a 
## single post, with the t_post as the first t_post in the clump (i.e., the last
## one before the first small t_post).
## Currently it has 'max' for 'maximum'.

# Define clumping parameters
grouping_latency                         <- 300; # defined the latency to 'group' posts
grouping_latency_unit                    <- "s"; # unit of grouping latency & t_post, 's' = seconds, 'm' = minutes.
mod_df_preproc %>% 
  group_by(user_num) %>% 
  count(t_post < grouping_latency) %>% ungroup(); # view how many short t_posts for each user

# Apply changes to clumps: 
# Here, we clump them by the 'max' 
# as this is the number of likes people are likely to remember and thus be motivated by.
mod_df_keep <- preprocess_clump(mod_df_preproc, max, grouping_latency) %>%
  dplyr::select(row_index, 
                user_num,
                likes, 
                retweet_count,
                t_post, 
                post_num_pic, 
                weekday, 
                followers_count,
                friends_count, 
                Authentic_Happiness_inventory, 
                gender, 
                datetime, 
                DOB, 
                timeofquestionnaire_local,
                ### add preprocessing params
                is_clump,     # whether that tweet was originally part of a clump of nearby tweets that were grouped together
                clumping_fun  # the function which combines the likes on the tweets in the original clump, to the final grouped tweet
                )

### Sanity check - view to check clumping worked
mod_df_check <- mod_df_preproc %>% left_join(mod_df_keep, mod_df_preproc, by = "row_index")
# Check mod_df_check. Specifically, visually check that the cumulative number of 
# likes/t_post/rts in left-out rows is combined using clumping_fun to result in the 
# total in the clumped row just above the left-out clump. If it does, proceed to make mod_df_save.
#View(mod_df_check)


##########################################
## Remove users with too few posts 
# important to do this after the clumping step (above) as that may remove many posts of a single user, making them be below the min post threshold.
min_userposts    = 10; # minimum number of posts for a user to be kept in the dataset. For Lindstrom 2021 Nat Comms, it was 10.
postcount_df    <- mod_df_preproc %>%  # get the data of the users to be removed
  count(user_num) %>% 
  mutate(insufficient_data = n < min_userposts) %>% 
  filter(insufficient_data == TRUE);
users_remove    <- postcount_df$user_num; # get the list of users to be removed
print(str_c("Users to remove: ", length(users_remove)))
mod_df_keep  <- mod_df_keep %>% filter(!(user_num %in% users_remove))

##### check how many users we have left now after all the preprocessing
n_unique_users_mod <- length(unique(mod_df_keep$user_num))
n_unique_users_mod

##########################################
# make final modelling df...
mod_df_save_colnames      <- c("user_num", 
                   "likes", 
                   "retweet_count", 
                   "t_post", 
                   "post_num_pic", 
                   "weekday",
                   "followers_count", 
                   "friends_count", 
                   "Authentic_Happiness_inventory", 
                   "gender", 
                   "datetime", 
                   "DOB", 
                   "timeofquestionnaire_local",
                   ### preprocessing params
                   "is_clump",
                   "clumping_fun",
                   "min_userposts", 
                   paste("mintpost_", as.character(grouping_latency_unit), sep = "")
                   );

mod_df_save                    <- as_tibble(matrix(nrow = nrow(mod_df_keep), ncol = length(mod_df_save_colnames)), .name_repair = ~ mod_df_save_colnames);
mod_df_save$user_num           <- mod_df_keep$user_num;
mod_df_save$likes              <- mod_df_keep$likes;
mod_df_save$retweet_count      <- mod_df_keep$retweet_count;
mod_df_save$t_post             <- mod_df_keep$t_post;
mod_df_save %<>% 
  group_by(user_num) %>% 
  mutate(post_num_pic = row_number()) %>% 
  ungroup();
mod_df_save$weekday            <- mod_df_keep$weekday
mod_df_save$followers_count    <- mod_df_keep$followers_count
mod_df_save$friends_count      <- mod_df_keep$friends_count
mod_df_save$Authentic_Happiness_inventory <- mod_df_keep$Authentic_Happiness_inventory
mod_df_save$gender             <- mod_df_keep$gender
mod_df_save$datetime           <- mod_df_keep$datetime
mod_df_save$DOB                <- mod_df_keep$DOB
mod_df_save$timeofquestionnaire_local <- mod_df_keep$timeofquestionnaire_local
### add preprocessing params
mod_df_save$is_clump           <- mod_df_keep$is_clump;
mod_df_save$clumping_fun       <- mod_df_keep$clumping_fun;
mod_df_save$min_userposts      <- min_userposts;
mod_df_save[paste("mintpost_", grouping_latency_unit, sep = "")] <- grouping_latency;


#Save the modelling dataset, including saving preprocessing parameters in name.
write_csv(mod_df_save, paste(data_path, format(Sys.time(), "%y%m%d"),  "_", dat_name, "_cleaned.csv", sep = ""))

