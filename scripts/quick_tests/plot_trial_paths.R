## --------------------------------
## Purpose of script: make some plots of trial paths
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: uses frame data from preprocessed/per_ppt
##
## --------------------------------

rm(list = ls()) # clean environment

source("src/helper_funcs.R")

library(data.table)
library(tidyverse)
library(future) # for parallel processing
library(zoo) # for rolling mean


##### Test #####
plot_trials <- function(trial, track_df){
  # plot the velocity of one trial over time
  trial_df <- track_df %>% 
    filter(trial_num == trial) 
  
  f <- trial_df %>% 
    ggplot(aes(x = corrected_time, y = angle_change)) +
    geom_line(color = "purple", alpha = 0.2) +
    geom_line(aes(x = corrected_time, y = angle), color = "blue", alpha = 0.2) +
    geom_line(aes(x = corrected_time, y = angle_2nd_deriv), color = "red", alpha = 0.2) +
    geom_line(aes(x = corrected_time, y = smoothed_abs_angle_2nd_deriv), color = "green", alpha = 0.2) +
    geom_line(aes(x = corrected_time, y = norm_speed * 200), color = "orange", alpha = 1) +
    geom_line(aes(x = corrected_time, y = norm_speed_1st_deriv * 2000), color = "black", alpha = 0.5) +
    geom_line(aes(x = corrected_time, y = norm_speed_1st_deriv_smoothed * 2000), color = "black", alpha = 1) +
    theme_bw() +
    labs(x = "Time (s)", y = "angle + derivatives", title = "Speed of Cursor Over Time")
  
  # add a vertical line where event_start_move = TRUE
  f <- f + geom_vline(aes(xintercept = corrected_time), data = trial_df %>% filter(event_start_move == TRUE), color = "green")
  # add a vertical line where event_max_vel = TRUE
  f <- f + geom_vline(aes(xintercept = corrected_time), data = trial_df %>% filter(event_max_vel == TRUE), color = "orange")
  # add a vertical line where event_max_accel = TRUE
  f <- f + geom_vline(aes(xintercept = corrected_time), data = trial_df %>% filter(event_max_accel == TRUE), color = "red")
  
  # add legend
  f <- f + theme(legend.position = "right")
  
  # save the plot
  ggsave(filename = paste("trial_", trial, "_angle.png", sep = ""), plot = f, path = "plots")
  
  
  # plot the x and z positions of the cursor
  f2 <- trial_df %>% 
    ggplot(aes(x = corrected_pos_x, y = corrected_pos_z, color = angle)) +
    geom_point() +
    theme_bw() +
    labs(x = "X Position (m)", y = "Z Position (m)", title = "Cursor Position Over Time")
  
  # use a color palette that highlights 0
  f2 <- f2 + scale_color_gradient(low = "blue", high = "red",
                                  limits = c(-90, 90), 
                                  breaks = c(-80, 0, 80),
                                  labels = c("-90", "0", "90"))

  # add a dot for event_start_move and event_max_vel
  f2 <- f2 + 
    geom_point(aes(x = corrected_pos_x, y = corrected_pos_z), data = trial_df %>% filter(event_start_move == TRUE), color = "green", size = 3) + 
    geom_point(aes(x = corrected_pos_x, y = corrected_pos_z), data = trial_df %>% filter(event_max_vel == TRUE), color = "orange", size = 3) +
    geom_point(aes(x = corrected_pos_x, y = corrected_pos_z), data = trial_df %>% filter(event_max_accel == TRUE), color = "red", size = 3) +
    geom_point(aes(x = corrected_pos_x, y = corrected_pos_z), data = trial_df %>% filter(event_3cm_move == TRUE), color = "black", size = 3)

  # add vertical and horizontal lines at the origin
  f2 <- f2 + geom_vline(xintercept = 0, color = "black", linetype = "dashed")
  f2 <- f2 + geom_hline(yintercept = 0, color = "black", linetype = "dashed")

  # add text for angle_max_vel
  f2 <- f2 + geom_text(aes(x = 0.05, y = 0.03, label = round(angle_max_vel, 0)), data = trial_df %>% filter(event_max_vel == TRUE), color = "orange", size = 3) +
    geom_text(aes(x = 0.05, y = 0.01, label = round(angle_max_accel, 0)), data = trial_df %>% filter(event_max_accel == TRUE), color = "red", size = 3) +
    geom_text(aes(x = 0.05, y = 0.05, label = round(angle_3cm_move, 0)), data = trial_df %>% filter(event_3cm_move == TRUE), color = "black", size = 3)


  # set x and y limits
  f2 <- f2 + xlim(-0.20, 0.20) + ylim(-0.05, 0.20)
  
  # save the plot
  ggsave(filename = paste("trial_", trial, "_pos.png", sep = ""), plot = f2, path = "plots")
  
}

do_test <- function(trial_range){

  for (trial in trial_range){
    var <- paste("P", trial, sep = "")

    # load in track_df
    track_df <- fread(paste(path, "/object_track_df.csv", sep = ""))
      
    assign(var, future({plot_trials(trial, track_df)}))
  }
}


##### Variables #####
processed_dir_path <- "data/preprocessed/per_ppt"
exp_versions <- c("single_rotation", "dual_30_deg", "dual_60_deg")
exp_short <- c("sr_30", "dr_30", "dr_60")


exp_index <- 1
ppt <- "7"
trial_range <-  seq(48, 62, 1)
path <- paste(processed_dir_path, "/", exp_short[exp_index], "_", ppt, sep = "")

plan(multisession)
do_test(trial_range)