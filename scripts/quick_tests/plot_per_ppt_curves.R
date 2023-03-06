## --------------------------------
## Purpose of script: make some plots of trial paths
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: uses per-trial data from omnnibus_bl_corrected.csv 
##
## --------------------------------

rm(list = ls()) # clean environment

source("src/helper_funcs.R")

library(data.table)
library(tidyverse)
library(future) # for parallel processing
library(zoo) # for rolling mean

##### Test #####
plot_one_ppt <- function(df, ppt){
  # plot the velocity of one trial over time
  ppt_df <- df %>% 
    filter(ppid == ppt) 

  # get the 4th and 5th character of the ppid
  this_exp_rotation <- as.numeric(substr(ppt, 4, 5))


  
  f <- ppt_df %>% 
    ggplot(aes(x = trial_num, y = obj_angle_3cm_move)) +
    geom_line(alpha = 0.2) +
    geom_point(aes(color = obj_shape))

  # if the dual_rotation is this_exp_rotation, make the background blue
  f <- f +
    geom_rect(data = ppt_df %>% filter(dual_rotation == this_exp_rotation),
              aes(xmin = trial_num - 0.5, xmax = trial_num + 0.5, ymin = -Inf, ymax = Inf),
              fill = "blue", alpha = 0.2) +
    geom_rect(data = ppt_df %>% filter(dual_rotation == this_exp_rotation * -1),
              aes(xmin = trial_num - 0.5, xmax = trial_num + 0.5, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.2)
    
    
  f <- f + theme_bw() +
    labs(x = "Trial Number", y = "Angle (deg)", title = "Angle of Object at 3cm Move")
  
  # add legend
  f <- f + theme(legend.position = "right")

  # save the plot
  ggsave(filename = paste("plots/", ppt, "_angle.png", sep = ""), plot = f)
  
}

do_test <- function(experiment, csv_dir_path){

  df <- fread(csv_dir_path)

  # filter for the experiment
  df <- df %>% filter(exp == experiment)

  # get unique ppids
  ppids <- unique(df$ppid)
  
  # make an empty list of length ppids
  # this will be used to store the future objects
  plot_list <- vector("list", length(ppids))

  # loop through ppids and make a future object for each
  for (i in 1:length(ppids)){
    ppt <- ppids[i]

    plot_list[[i]] <- future(plot_one_ppt(df, ppt))
  }

  # evaluate the futures
  plot_list <- value(plot_list)

  print("done")
}


##### Variables #####
csv_dir_path <- "data/preprocessed/omnibus/omnibus_bl_corrected.csv"
exp_versions <- c("single_rotation", "dual_30_deg", "dual_60_deg")
exp_short <- c("sr_30", "dr_30", "dr_60")


exp_index <- 2
experiment <- exp_short[exp_index]

plan(multisession)
do_test(experiment, csv_dir_path)
