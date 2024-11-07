


#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------        GENERAL SETTINGS         ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#



library(viridis)

#### colour palette for each model

plasma_palette <- viridis(4, option = "viridis")
# Extract the colors
color1 <- plasma_palette[1]  # First color
color2 <- plasma_palette[2]  # Second color
color3 <- plasma_palette[3]  # Third color
mix_colors <- function(color1, color2) {
  rgb1 <- col2rgb(color1)
  rgb2 <- col2rgb(color2)
  mixed_rgb <- (rgb1 + rgb2) / 2
  mixed_color <- rgb(mixed_rgb[1,], mixed_rgb[2,], mixed_rgb[3,], maxColorValue = 255)
  return(mixed_color)
}
color4 <- mix_colors(color2, color3)
color5 <- "#FF8AAE"

colour_palette <- c(
  a_fitdat_FP                = color1,
  b_fitdat_CP                = color1,
  c_fitdat_PH                = color2,
  d_fitdat_RL1               = color3,
  e_fitdat_RL2               = color3,
  f_fitdat_RLH1              = color4,
  g_fitdat_RLH2              = color4
)

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------            FUNCTIONS            ---------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

####################################################################################### 
####################################################################################### 
##############                Model comparison functions                 ##############            
####################################################################################### 
####################################################################################### 

########################################
##### function to load all model fitted parameters for a given dataset
########################################

load_modfits <- function(dat_fitted_to) {
  
  ############## EMPIRICAL DATASETS ############## 
  
  if (dat_fitted_to == "AHconf_justLikes") {
    
    data_name_string         <- "241104_241104_240416_AHconf_cleaned_preproc"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241105-0527FP14.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0634CP14.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241105-0527PH14.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-0612RL114.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0743RL214.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-0920RLH114.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241105-1012RLH214.csv"))
    
  } else if (dat_fitted_to == "AHdisc_justLikes") {
    
    data_name_string         <- "241104_241104_240228_AHdisc_cleaned_preproc"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-1412FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-1509CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-1411PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-1451RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-1605RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-1731RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-1824RLH21.csv"))
    
  } else if (dat_fitted_to == "AHconf_LikesPlusRTs") {
    
    data_name_string         <- "241104_241104_240416_AHconf_cleaned_preproc"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241106-0826FP14.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-1459CP14.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241106-0826PH14.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-1436RL114.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-1612RL214.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1804RLH114.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241105-1907RLH214.csv"))
    
  } else if (dat_fitted_to == "AHconf_justRTs") {
    
    data_name_string <- "241104_241104_240416_AHconf_cleaned_preproc"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241105-1344FP14.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-1501CP14.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241105-1344PH14.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-1455RL114.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-1616RL214.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1808RLH114.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241105-1910RLH214.csv"))
    
############## SIMUALTED DATASETS WITH 80 POSTS ############## 
    
  } else if (dat_fitted_to == "Norm80_SimFP") {
    
    data_name_string         <- "241104-2020FP"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2052FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-2118CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2052PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2107RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-2140RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-2231RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-2250RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm80_SimCP") {
    
    data_name_string         <- "241104-2023CP"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2054FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-2131CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2055PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2118RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-2153RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-2227RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-2248RLH21.csv"))
    
    
  } else if (dat_fitted_to == "Norm80_SimPH") {
    
    data_name_string         <- "241104-2031PH"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2057FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-2125CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2056PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2113RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-2145RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-2218RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-2235RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm80_SimRL1") {
    
    data_name_string         <- "241104-2025RL1"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2059FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-2124CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2059PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2110RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-2135RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-2215RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-2235RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm80_SimRL2") {
    
    data_name_string         <- "241104-2027RL2"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241106-0917FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241106-0944CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241106-0917PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241106-0928RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241106-0958RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241106-1045RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241106-1107RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm80_SimRLH1") {
    
    data_name_string         <- "241104-2037RLH1"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2102FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-2135CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2103PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2120RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-2151RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-2228RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-2259RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm80_SimRLH2") {
    
    data_name_string         <- "241104-2043RLH2"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2106FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241104-2139CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2106PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2123RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241104-2150RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241104-2223RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241104-2249RLH21.csv"))
    
    ############## SIMULATED DATASETS WITH 1000 POSTS ############## 
    
  } else if (dat_fitted_to == "Norm1000_SimFP") {
    
    data_name_string         <- "241104-2020FP"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2204FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-1612CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2211PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-2031RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241106-1821RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241107-0513RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241107-0853RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm1000_SimCP") {
    
    data_name_string         <- "241104-2023CP"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2153FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0051CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2203PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-0151RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0708RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1326RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241105-1711RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm1000_SimPH") {
    
    data_name_string         <- "241104-2031PH"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2155FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0158CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2147PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-0140RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0813RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-2228RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241106-2007RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm1000_SimRL1") {
    
    data_name_string         <- "241104-2025RL1"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2146FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0044CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2150PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2337RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0431RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1300RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241106-1637RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm1000_SimRL2") {
    
    data_name_string         <- "241104-2027RL2"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2201FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0046CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2151PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241104-2356RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0626RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1330RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241106-1846RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm1000_SimRLH1") {
    
    data_name_string         <- "241104-2037RLH1"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2205FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0147CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2207PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-0110RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0655RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1229RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241105-1749RLH21.csv"))
    
  } else if (dat_fitted_to == "Norm1000_SimRLH2") {
    
    data_name_string         <- "241104-2043RLH2"
    fitdat_path              <- str_c("./../../data_processed/Twitter/fit/", data_name_string, "/")
    fitdat_FP                <- read_csv(str_c(fitdat_path, "241104-2208FP1.csv"))
    fitdat_CP                <- read_csv(str_c(fitdat_path, "241105-0153CP1.csv"))
    fitdat_PH                <- read_csv(str_c(fitdat_path, "241104-2207PH1.csv"))
    fitdat_RL1               <- read_csv(str_c(fitdat_path, "241105-0120RL11.csv"))
    fitdat_RL2               <- read_csv(str_c(fitdat_path, "241105-0656RL21.csv"))  
    fitdat_RLH1              <- read_csv(str_c(fitdat_path, "241105-1325RLH11.csv"))
    fitdat_RLH2              <- read_csv(str_c(fitdat_path, "241105-1813RLH21.csv"))
  }
  
  fitted_mods <- list(fitdat_FP       = fitdat_FP,
                      fitdat_CP       = fitdat_CP,
                      fitdat_PH       = fitdat_PH,
                      fitdat_RL1      = fitdat_RL1,
                      fitdat_RL2      = fitdat_RL2,
                      fitdat_RLH1     = fitdat_RLH1,
                      fitdat_RLH2     = fitdat_RLH2)
  
}


########################################
##### function to get AICw from fitted parameter dfs
#######################################

make_df_AICw <- function(fitted_dat) { 
  
  # fitted_dat is a list of all the different model fits for a given dataset, which can be outputted by the function load_modfits
  
  ## Get Akaike Weights (AICws)
  #################################################################################
  
  fitdat_FP   <- fitted_dat$fitdat_FP
  fitdat_CP   <- fitted_dat$fitdat_CP
  fitdat_PH   <- fitted_dat$fitdat_PH
  fitdat_RL1  <- fitted_dat$fitdat_RL1
  fitdat_RL2  <- fitted_dat$fitdat_RL2
  fitdat_RLH1 <- fitted_dat$fitdat_RLH1
  fitdat_RLH2 <- fitted_dat$fitdat_RLH2
  
  # get df with only AICs of all models
  df_AIC <- data.frame(a_fitdat_FP   = fitdat_FP$AIC,
                       b_fitdat_CP   = fitdat_CP$AIC,
                       c_fitdat_PH   = fitdat_PH$AIC,
                       d_fitdat_RL1  = fitdat_RL1$AIC,
                       e_fitdat_RL2  = fitdat_RL2$AIC,
                       f_fitdat_RLH1 = fitdat_RLH1$AIC,
                       g_fitdat_RLH2 = fitdat_RLH2$AIC
  )
  
  # apply aicw function to get weights, and reformat
  aicw_list         <- apply(df_AIC, 1, aicw) %>% lapply(., function(list) list$w)
  # initialise empty df for AICw (same shape and format as df_AIC)
  df_AICw           <- as.data.frame(matrix(NA, 
                                            nrow = dim(df_AIC)[1],
                                            ncol = dim(df_AIC)[2]))
  colnames(df_AICw) <- colnames(df_AIC)
  # fill in empty df with AICws
  for (i in seq_along(aicw_list)) {
    df_AICw[i, ] <- aicw_list[[i]]
  }
  # add back user_nums 
  df_AIC$user_num  <- as.factor(fitdat_FP$user_num)
  df_AICw$user_num <- as.factor(fitdat_FP$user_num);
  df_AICw
  
}


########################################
##### function to plot model recovery figure
########################################

plot_modrec <- function(modnames, fig_title) {
  
  fitted_mods_SimFP    <- load_modfits(modnames[[1]])
  fitted_mods_SimCP    <- load_modfits(modnames[[2]])
  fitted_mods_SimPH    <- load_modfits(modnames[[3]])
  fitted_mods_SimRL1   <- load_modfits(modnames[[4]])
  fitted_mods_SimRL2   <- load_modfits(modnames[[5]])
  fitted_mods_SimRLH1  <- load_modfits(modnames[[6]])
  fitted_mods_SimRLH2  <- load_modfits(modnames[[7]])
  
  df_AICw_SimFP       <- make_df_AICw(fitted_mods_SimFP) 
  df_AICw_SimCP       <- make_df_AICw(fitted_mods_SimCP)
  df_AICw_SimPH       <- make_df_AICw(fitted_mods_SimPH)
  df_AICw_SimRL1      <- make_df_AICw(fitted_mods_SimRL1)
  df_AICw_SimRL2      <- make_df_AICw(fitted_mods_SimRL2)
  df_AICw_SimRLH1     <- make_df_AICw(fitted_mods_SimRLH1)
  df_AICw_SimRLH2     <- make_df_AICw(fitted_mods_SimRLH2)

  # get row of mean df_AICw for each fitted model to that model
  meandf_AICw_SimFP   <- df_AICw_SimFP[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))
  meandf_AICw_SimCP   <- df_AICw_SimCP[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))
  meandf_AICw_SimPH   <- df_AICw_SimPH[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))
  meandf_AICw_SimRL1  <- df_AICw_SimRL1[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))
  meandf_AICw_SimRL2  <- df_AICw_SimRL2[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))
  meandf_AICw_SimRLH1 <- df_AICw_SimRLH1[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))
  meandf_AICw_SimRLH2 <- df_AICw_SimRLH2[1:7] %>% drop_na(.) %>% summarise(across(everything(), mean))

  combined_df <- list("a_sim_FP"   = meandf_AICw_SimFP,
                      "b_sim_CP"   = meandf_AICw_SimCP,
                      "c_sim_PH"   = meandf_AICw_SimPH,
                      "d_sim_RL1"  = meandf_AICw_SimRL1,
                      "e_sim_RL2"  = meandf_AICw_SimRL2,
                      "f_sim_RLH1" = meandf_AICw_SimRLH1,
                      "g_sim_RLH2" = meandf_AICw_SimRLH2) %>% bind_rows(., .id = "row_id")
  
  long_df <- combined_df %>%
    gather(key = "variable", value = "value", -row_id)
  ggplot(long_df, aes(x = as.factor(variable), 
                      y = as.factor(row_id
                      ), fill = value)) +
    geom_tile() +
    #scale_fill_gradient(low = "white", high = "black") +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 0.58)) +  # Set scale from 0 to 1
    
    labs(title = fig_title, x = "variable", y = "Row", fill = "Value") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          axis.text.y = element_blank(),      # Remove y-axis text labels
          axis.title.y = element_blank(),      # Remove y-axis title 
         # legend.position = "none"             # Remove legend
          )
  
}


########################################
##### function to plot AICw figure
#######################################


plot_AICw <- function(dataset, dataset_name) {
  AICw_plot <- ggplot(data = dataset, aes(Model, AICw, fill = Model, colour = Model)) +
    geom_jitter(alpha = 0.12, stroke = 0, width = 0.3) +  # Adjust the width of geom_jitter
    geom_boxplot(
      aes(fill = Model, colour = Model), 
      alpha = 0.16, 
      outlier.shape = NA, 
      width = 0.65,
      position = position_dodge(width = 0.75)) +
    geom_point(stat = "summary", fun = "mean", size = 4, shape = 21, aes(fill = Model), color = "black", position = position_dodge(width = 0.1)) +
    ylab("AICw") +
    ggtitle(str_c("Models fitted on ", dataset_name, " data:\nN = ", length(unique(dataset[["user_num"]])))) + 
    scale_x_discrete(limits = unique(dataset$Model)) +
    theme_classic() +
    scale_colour_manual(values = colour_palette) +
    scale_fill_manual(values = colour_palette) +
    theme(legend.position = "none") +
    theme(
      axis.text.x = element_blank(),        # Remove x-axis tick labels
      axis.title.x = element_blank(),       # Remove x-axis label
      plot.title = element_text(hjust = 0.5)
    ) 
  AICw_plot
}


generate_AICw_plot <- function(which_empdat, to_return) {
  # Load the model fit data
  data <- load_modfits(which_empdat)
  
  # Create the AICw DataFrame
  df_AICw <- make_df_AICw(data)
  
  # Transform to long format for plotting
  df_AICw_long <- gather(df_AICw, key = "Model", value = "AICw", -user_num)
  
  # Generate the plot
  AICw_plot <- plot_AICw(df_AICw_long, which_empdat)
  
  # Return the plot
  
  if (to_return == "df_AICw") {
    return(df_AICw)
  } else if (to_return == "AICw_plot") {
    return(AICw_plot)
  }
  
}


########################################
##### function to save figure
#######################################

## in each case, save the filename with the date too so that future days don't just write over and lose the figs.
save_fig <- function(fig, subfolder, filename) {
  path <- file.path(subfolder, paste(format(Sys.time(), "%Y-%m-%d_"), filename, sep = ""))
  ggsave(path, fig, dpi = 300, width = 5, height = 4, units = "in")
}



#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------       Parameter recovery functions       ------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

####################################################################
### generic function for plotting parameter recovery


### generic function for correlations of param recovery
plt_parrec <- function(data1, data2, modname, varname, xname, yname, nposts) {
  # Extract the specified variables from the datasets
  x <- data1[[varname]]
  y <- data2[[varname]]
  
  # Create a data frame for the ggplot
  plot_data <- data.frame(x = x, y = y) %>% na.omit()
  
  # Calculate the correlation coefficient
  pearson_correlation_coefficient <- cor(plot_data$x, plot_data$y)
  spearman_correlation_coefficient <- cor(plot_data$x, plot_data$y, method = "spearman")
  
  # Determine the axis limits based on the combined range of both variables
  limits <- range(c(x, y))
  
  # Create the scatterplot with a line of best fit
  gg <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = str_c("Param recovery for \n", modname, nposts, " \n", varname),
      x = xname,
      y = yname,
      caption = paste("Spearman Correlation coefficient: ", round(spearman_correlation_coefficient, 2),
                      "\nPearson Correlation coefficient: ", round(pearson_correlation_coefficient, 2))
    ) +
    theme_classic() +
    scale_x_continuous(limits = limits) +
    scale_y_continuous(limits = limits)
  
  
  return(gg)
}


# Function to calculate correlations and return a matrix
calculate_correlations <- function(fit_df, simgen_df, fit_cols, simgen_cols) {
  correlation_matrix <- matrix(NA, nrow = length(fit_cols), ncol = length(simgen_cols),
                               dimnames = 
                                 list(lapply(fit_cols, function(x) {
                                   if (is.character(x)) paste0("fit_", x) else x
                                 }), 
                                 lapply(simgen_cols, function(x) {
                                   if (is.character(x)) paste0("simgen_", x) else x
                                 })))
  
  for (fit_col in fit_cols) {
    for (simgen_col in simgen_cols) {
      correlation_matrix[str_c("fit_", fit_col), str_c("simgen_",simgen_col)] <- cor(fit_df[[fit_col]], simgen_df[[simgen_col]], use = "complete.obs", method = "spearman")
    }
  }
  
  return(correlation_matrix)
}


plot_correlation_grid <- function(correlation_matrix) {
  # Convert the correlation matrix to a long format data frame
  melt_matrix <- reshape2::melt(correlation_matrix, na.rm = TRUE)
  
  # Create the heatmap plot using ggplot2
  ggplot(melt_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), 
                         name = "Correlation") +
    theme_minimal() +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      axis.text = element_blank(),      # Remove tick labels
      axis.ticks = element_blank(),      # Remove tick marks
      axis.title = element_blank(),       # Remove axis titles,
      panel.grid.major = element_blank(),     # Remove major grid lines
      panel.grid.minor = element_blank(),      # Remove minor grid lines
      
     # legend.position = "none"
    ) +
    coord_fixed(ratio=1) # Ensures the tiles are square
}



#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#
#-----------------          Results stats functions         ------------------------#
#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#


####################################################################################### 
############################# function to make variables
####################################################################################### 
 
mkvars_glm <- function(rawdat) {

  #####
  ## t_post
  rawdat$log_tpost             <- log(rawdat$t_post)
  rawdat$lag_log_tpost         <- lag(rawdat$log_tpost)

  ######
  ## delta t_post
  rawdat$delta_logtpost        <- log(rawdat$t_post) - log(lag(rawdat$t_post))
  rawdat$delta_logtpost[which(rawdat$post_num_pic==1)] <- NaN # get rid of it for the first post of each person

  #####
  ## reward prediction
  # Create lagged features
  rawdat <- rawdat %>%
    mutate(likes_lag1 = lag(likes, 1),
           likes_lag2 = lag(likes, 2),
           likes_lag3 = lag(likes, 3),
           likes_lag4 = lag(likes, 4),
           likes_lag5 = lag(likes, 5),
           likes_lag6 = lag(likes, 6),
           likes_lag7 = lag(likes, 7),
           likes_lag8 = lag(likes, 8),
           likes_lag9 = lag(likes, 9),
           likes_lag10 = lag(likes, 10)
           )
  
  ## reward prediction including last 10 likes
  rawdat$rwd_pred_last10p        <- rowMeans(select(rawdat, starts_with("likes_lag")), na.rm = TRUE)
  rawdat                         <- select(rawdat, -starts_with("likes_lag"))
  #####
  ## prediction error
  rawdat$RPE      <- rawdat$likes - rawdat$rwd_pred_last10p
  rawdat$lag_RPE  <- lag(rawdat$RPE)
  rawdat <- rawdat %>%
    group_by(user_num) %>%
    mutate(scaled_lag_RPE        = scale(lag_RPE)) %>%
    mutate(scaled_delta_logtpost = scale(delta_logtpost)) %>%
    ungroup()

  #####
  
  # return
  rawdat

}


# ####################################################################################### 
# ############################# function to do GLMs
# ####################################################################################### 
# 

# # function to look at beta RPE within a single dataset of one model 
glm_RPE_deltatpost <- function(dat) {
  glm_model <- lmer(scaled_delta_logtpost ~
                      scaled_lag_RPE + (0 + scaled_lag_RPE | user_num),
                    data = dat)
}

# ####################################################################################### 
# ############################# functions to plot results
# ####################################################################################### 

plot_vardist <- function(dataset, varname, x_limits = NULL) {
  x <- dataset[[varname]]
  p <- ggplot(dataset, aes(x = x)) +
    geom_density(fill = "black", color = "black", alpha = 0.5) +
    theme_classic() +
    labs(title = str_c("Distribution of ", varname),
         x = varname,
         y = "Density")

  # Add x-axis limits if provided
  if (!is.null(x_limits)) {
    p <- p + xlim(x_limits)
  }

  return(p)
}

plot_glm_forest <- function(dataset_list, dataset_names, colour_palette, pred_varname, dep_varname) {

  # Extract the fixed effect (Beta) coefficients and standard errors from the GLM results
  beta_values <- lapply(dataset_list, function(glm_result) {
    coef_summary <- summary(glm_result)$coefficients
    beta_value <- coef_summary[pred_varname, "Estimate"]
    se <- coef_summary[pred_varname, "Std. Error"]
    lower_ci <- beta_value - 1.96 * se  # 95% confidence interval
    upper_ci <- beta_value + 1.96 * se  # 95% confidence interval
    return(c(beta_value, lower_ci, upper_ci))
  })

  # Create a data frame for the forest plot
  forest_data <- data.frame(
    Dataset = dataset_names,
    Beta = sapply(beta_values, `[`, 1),
    Lower = sapply(beta_values, `[`, 2),
    Upper = sapply(beta_values, `[`, 3)
  )

  # Create a forest plot using ggplot2 with the custom color palette
  ggplot(forest_data, aes(x = Beta, y = Dataset)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Add red vertical line at x = 0
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
    geom_point(aes(fill = Dataset), size = 3.8, shape = 21, color = "black", stroke = 0.8) +  # Add black outline to points
    labs(title = str_c("Fixed Effects of \n", pred_varname, " on", dep_varname),
         x = "Beta Values",
         y = "Datasets") +
    scale_fill_manual(values = colour_palette) +  # Set the custom color palette
    theme_classic() +
    theme(
      axis.text.x = element_blank(),        # Remove x-axis tick labels
      axis.title.x = element_blank()       # Remove x-axis label
    ) +
    theme(legend.position = "none")
}

# ###### scatter plot with just a linear line

plot_scatter_lm <- function(dat, xvar, yvar, line_colour = color4, x_coord_limits = NULL, y_coord_limits = NULL, point_size = 2) {
  p <- ggplot(dat, aes(x = !!sym(xvar), y = !!sym(yvar))) +
    geom_point(alpha = 0.4, size = point_size, color = "darkgrey", stroke = 0) +
    geom_smooth(method = "lm",
                color = line_colour,
                fill = line_colour) +
    labs(title = str_c("Relationship between ", xvar, "\n and ", yvar ),
         x = xvar,
         y = yvar) +
    theme_classic()

  if (!is.null(y_coord_limits) & !is.null(y_coord_limits)) {
    p <- p + coord_cartesian(xlim = x_coord_limits, ylim = y_coord_limits )
  }

  return(p)
}

###### scatter plot with  quadratic terms
plot_scatter_quad <- function(dat, xvar, yvar, line_colour = color4, y_coord_limits = NULL, point_size = 2) {
  p <- ggplot(dat, aes(x = !!sym(xvar), y = !!sym(yvar))) +
    geom_point(alpha = 0.4, size = point_size, color = "darkgrey", stroke = 0) +
    #   geom_smooth(method = "lm", lor = colorRampPalette(c("lightgreen", "darkgreen"))(8)[8],
    #                fill = colorRampPalette(c("lightgreen", "darkgreen"))(8)[3]) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2),
                color = line_colour,
                fill = line_colour) +

    labs(title = str_c("Relationship between ", xvar, "\n and ", yvar ),
         x = xvar,
         y = yvar) +
    theme_classic()
  # Apply coord_cartesian if coord_limits are provided
  if (!is.null(y_coord_limits)) {
    p <- p + coord_cartesian( ylim = y_coord_limits )
    return(p)
  }
  p
}


create_raincloud_plot <- function(data, gender_var, weight_var, mean_values, gender_palette, title_prefix = "Raincloud Plot of Gender by Habit Weight") {
  p <- ggplot(data, aes_string(x = gender_var, y = weight_var, fill = gender_var)) +
    ggdist::stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA, alpha = 0.5, stroke = 0) +
    geom_boxplot(width = .13, outlier.shape = NA, aes_string(color = gender_var), alpha = 0.3) +
    gghalves::geom_half_point(side = "m", range_scale = .4, alpha = .1, stroke =0, aes_string(color = gender_var)) +
    geom_line(data = mean_values, aes_string(x = gender_var, y = "mean_y", group = 1), color = "black") +
    geom_point(data = mean_values, aes_string(x = gender_var, y = "mean_y", fill = gender_var), shape = 21, size = 2, color = "black") +
    scale_color_manual(values = gender_palette) +
    scale_fill_manual(values = gender_palette) +  # Apply fill colors from the gender palette
    labs(
      title = paste0(title_prefix, "\nAge range = ", round(min(data$age)), " to ", round(max(data$age))),
      x = gender_var,
      y = weight_var
    ) +
    scale_x_discrete(expand = expansion(add = c(0.45, 0.8))) +
    theme_classic() +
    theme(legend.position = "none")  # Remove the legend
  
}
# 
# # plot a correlation matrix
plot_corr_matrix <- function(data, varList, title) {
  cor_variables <- data[, varList]
  correlation_matrix <- cor(cor_variables, use = "complete.obs")
  cor_df <- as.data.frame(as.table(correlation_matrix))
  colnames(cor_df) <- c("Var1", "Var2", "value")
  ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          axis.text.y = element_text(angle = 0, hjust=1)) +
    labs(title =title)
}

plot_violin <- function(dat,xvar,yvar, fillvar, palette) {
  mean_values <- dat %>%
    group_by(!!sym(fillvar)) %>%
    summarize(mean_y = mean(!!sym(yvar), na.rm = TRUE))
  ggplot(dat, aes(x = !!sym(xvar), y = !!sym(yvar), fill = !!sym(fillvar), color = !!sym(fillvar))) +
    geom_violin(alpha = 0.5) +
    geom_point(data = mean_values, aes(x = !!sym(xvar), y = mean_y), shape = 20, size = 3, color = "black", position = position_dodge(width = 0.75) )+
    geom_line(data = mean_values, aes(x = !!sym(xvar), y = mean_y, group = 1), color = "black") +
    scale_color_manual(values = palette) +  # Specify outline colors
    scale_fill_manual(values = palette) +
    labs(title = str_c("Violin Plot of ",yvar, " by ", xvar, "\nAge range = ", round(min(dat$age)), " to ", round(max(dat$age))),
         x = xvar,
         y = yvar) +
    theme_classic() +
    theme(legend.position = "none")
}



