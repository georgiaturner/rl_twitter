
#---------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#-----------------------------------------    00-FUNCTIONS.R              --------------------------------------------------#
#-----------------------------------------                                --------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------#

# This script contains functions to be used to clean the Twitter data, which are called in either '01-clean_twitter_data.R' 
# or '02-pre-process_twitter_data.R'

# By Georgia Turner, 2024 < georgianjt@gmail.com >

#####################################################################################################################
###### Functions for 01-clean_twitter_data.R
#####################################################################################################################

preprocess_clump <- function(df, clumping_fun, grouping_latency) {
  ## define function which will identify 'clumps' of t_post which are below the minimum
  ## t_post, include in each clump the final long-enough t_post before the short
  ## one(s) start, and then apply clumping_fun to the clump (e.g. max, min, mean, sum),
  ## to combine all the likes/RTs in the clump according to the function in clumping_fun.
  ## It then deletes all the consecutive lines in the clump apart from the first, 
  ## thereby only retaining the long-enough t_post
  ## with the amount of likes and RTs on this long t_post the same amount as the combined
  ## number within the clump, combined according to clumping_fun. 
  ## it retains columns containing info about whether it was a clump in the first place ('is_clump'), and what the
  ## clumping function was ('clumping_fun').
  ## It is an open question what to do with likes and RTs in the 'clump'.
  ## Here, a function 'preprocess_clump' is defined which takes as an input the 
  ## function you want to apply within the clump, such as 'max', 'mean', or 'sum'.
  
  df <- df %>%
    # make groups for each clump with consecutive low tposts
    mutate(below_mintpost = (t_post <= grouping_latency & post_num_pic!=1), # add the post_num_pic constraint so the first post for each person isn't just clumped with the person before
           grp = cumsum(!below_mintpost)) %>% 
    group_by(grp) %>%
    mutate(clump_likes         = ifelse(any(t_post <= grouping_latency), 
                                        clumping_fun(likes), NA), # get the max/mean/sum values in each clump
           clump_retweet_count = ifelse(any(t_post <= grouping_latency), 
                                        clumping_fun(retweet_count), NA),
           ## store info about this preprocessing:
           is_clump = any(t_post <= grouping_latency) & !(post_num_pic == 1 & n() == 1) # create a variable to store whether it was a clump.
           # it excludes clumps of length 1 where it was the first post, as then this wasn't a clump it was just the first post.
    ) %>% 
    ungroup() %>%
    mutate(old_likes     = likes, # even though don't keep at end, made these extra cols for visual checking during function making
           old_rt        = retweet_count,
           likes         = ifelse(is.na(clump_likes), likes, clump_likes),
           retweet_count = ifelse(is.na(clump_retweet_count), retweet_count, clump_retweet_count)) %>%
    group_by(grp) %>% slice(1) %>% ungroup()
  
  ## store info about what clumping function was
  df$clumping_fun <- deparse(substitute(clumping_fun))
  
  df
} 

#####################################################################################################################
###### Functions for 02-pre-process_twitter_data.R
#####################################################################################################################

# filter dataset to the amount of months required,centered around questionnaire date
filt_around_qnr <- function(dataset, nmonths_total, timeofquestionnaire_col) {
  timeofquestionnaire_col_sym <- sym(timeofquestionnaire_col)
  
  moddat_filt <- dataset %>%
    mutate(time_col = !!timeofquestionnaire_col_sym) %>%
    filter(datetime >= (time_col - months(nmonths_total / 2)) &
             datetime <= (time_col + months(nmonths_total / 2))) %>%
    arrange(user_num, datetime) %>%
    group_by(user_num) %>%
    mutate(post_num_pic = row_number()) %>%
    ungroup() %>%
    select(-time_col)  # Remove the temporary column
  
  return(moddat_filt)
}

# visualise the distributions of number of posts 
visualise_npostdist <- function(dataset, title, bwidth) {
  # Calculate the median
  median_val <- median(dataset$nposts)
  # Create a hist
  ggplot(dataset, aes(x = nposts)) +
    geom_histogram(binwidth = bwidth, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = median_val, linetype = "dotted", color = "red") +
    theme_classic()+
    annotate("text", x = median_val, y = Inf, label = paste("Median =", median_val),
             vjust = 1, hjust = 0.5, color = "red", size = 4) +
    labs(title = str_c("Histogram of ", title, " nposts"), x = "Number of Posts", y = "Frequency")
}

# filter dataset to only include users with minimum number of posts
filt_by_nposts <- function(dataset, nposts_min) {
  dat_filtered <- dataset %>% 
    group_by(user_num) %>%
    filter(n() >= nposts_min)
  dat_filtered 
}





