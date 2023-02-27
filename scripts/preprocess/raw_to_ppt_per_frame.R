## --------------------------------
## Purpose of script: Make long (per_frame) CSVs for trackerholder (the real hand),
## and not the cursorObject (the object)
##
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: 
##
## --------------------------------

rm(list = ls()) # clean environment

source("src/helper_funcs.R")

library(data.table)
library(tidyverse)
library(future) # for parallel processing
library(zoo) # for rolling mean

##### Variables #####
raw_dir_path <- "data/raw"
processed_dir_path <- "data/preprocessed/per_ppt"
exp_versions <- c("single_rotation", "dual_30_deg", "dual_60_deg")
exp_short <- c("sr_30", "dr_30", "dr_60")
# test_ppt <- "data/raw/single_rotation/1/S001/trial_results.csv"

##### Helper Functions #####
# create a directory if it doesn't exist
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

# load trial_results.csv
load_trial_result_csv <- function(path, exp_index, ppt) {
  # creat csv path
  csv_path <- paste(path, ppt, "S001", "trial_results.csv", sep = "/")

  # read in the csv
  df <- fread(csv_path, stringsAsFactors = FALSE)

  # fix the ppid
  new_ppid <- paste(exp_short[exp_index], ppt, sep = "_")

  df$ppid <- NULL # get rid of ppid to avoid type coercion errors
  df$ppid <- new_ppid
  return(df)
}

# load the cursorobjecttracker_complete.csv
load_cursorobjecttracker_csv <- function(path, ppt) {
  # create csv path
  csv_path <- paste(path, ppt, "S001", "cursorobjecttracker_complete.csv", sep = "/")

  # read in the csv
  df <- fread(csv_path, stringsAsFactors = FALSE)

  return(df)
}

# load the trackerholderobject_complete.csv
load_trackerholderobject_csv <- function(path, ppt) {
  # create csv path
  csv_path <- paste(path, ppt, "S001", "trackerholderobject_complete.csv", sep = "/")

  # read in the csv
  df <- fread(csv_path, stringsAsFactors = FALSE)

  return(df)
}

## TESTING ##
exp_index <- 1

##### All Experiments #####
make_per_frame_files <- function(exp_index) {
  path <- paste(raw_dir_path, exp_versions[exp_index], sep = "/")
  
  # loop through the files in path
  for (ppt in list.files(path = path)) {
    # load the trial results
    trial_df <- load_trial_result_csv(path, exp_index, ppt)

    # if there is a column names pick_up_time, rename it
    if ("pick_up_time" %in% colnames(trial_df)) {
      trial_df <- trial_df %>% rename(step_time = pick_up_time)
    }

    # load the cursor object tracker
    hand_track_df <- load_trackerholderobject_csv(path, ppt)
    
    # non equi join using data table (not sure dplyr can do this)
    hand_track_df <- hand_track_df[trial_df, on = .(time >= start_time, time < end_time), 
                           nomatch = 0, .(pos_x, pos_y, pos_z, x.time, 
                                          start_time, end_time, step_time, 
                                          trial_num, block_num, trial_num_in_block, targetAngle, cursor_rotation,
                                          type, hand, obj_shape, ppid)]

    # remove rows where type = "instruction"
    hand_track_df <- hand_track_df %>% filter(type != "instruction")

    # add a column for the step number
    hand_track_df <- hand_track_df %>% 
      mutate(step_num = case_when(x.time < step_time ~ 0,
                                  TRUE ~ 1))

    # keep only rows where step_num = 0
    hand_track_df <- hand_track_df %>% 
      filter(step_num == 0)

    # remove the step_num column
    hand_track_df$step_num <- NULL

    # remove rows where type = "instruction"
    hand_track_df <- hand_track_df %>% filter(type != "instruction")

    # remove the first row of each trial -- this is always set to 0
    hand_track_df <- hand_track_df %>% 
      group_by(trial_num) %>% 
      slice(-1)
             
    hand_track_df <- hand_track_df %>% 
      group_by(trial_num) %>% 
      mutate(first_pos_x = first(pos_x),
             first_pos_z = first(pos_z),
             pos_x = pos_x - first_pos_x,
             pos_z = pos_z - first_pos_z,
             dist = sqrt(pos_x^2 + pos_z^2),
             pos_x_change = pos_x - lag(pos_x),
             pos_z_change = pos_z - lag(pos_z),
             angle = atan2_2d(pos_x, pos_z, targetAngle),
             angle_change = atan2_2d(pos_x_change, pos_z_change, targetAngle),
             angle_2nd_deriv = angle_change - lag(angle_change),
             abs_angle_2nd_deriv = abs(angle_2nd_deriv),
             smoothed_abs_angle_2nd_deriv = rollmean(abs_angle_2nd_deriv, 5, fill = NA),
             pos_change_dist = sqrt((pos_x_change)^2 + (pos_z_change)^2),
             dist_change = dist - lag(dist),
             time_change = x.time - lag(x.time),
             speed = abs(pos_change_dist / time_change),
             smoothed_speed = rollmean(speed, 5, fill = NA),
             max_smoothed_speed = max(smoothed_speed, na.rm = TRUE),
             norm_speed = smoothed_speed / max_smoothed_speed,
             start_move_event = norm_speed >= 0.20 &
               !duplicated(norm_speed >= 0.20))

    # save the file
    new_dir_path <- paste(processed_dir_path, "/", exp_short[exp_index], "_", ppt, sep = "")
    create_dir(new_dir_path)
    fwrite(hand_track_df, file = paste(new_dir_path, "hand_track_df.csv", sep = "/"))


    # load the cursor object tracker
    object_track_df <- load_cursorobjecttracker_csv(path, ppt)

    # non equi join using data table (not sure dplyr can do this)
    object_track_df <- object_track_df[trial_df, on = .(time >= start_time, time < end_time), 
                           nomatch = 0, .(pos_x, pos_y, pos_z, x.time, 
                                          start_time, end_time, step_time, 
                                          trial_num, block_num, trial_num_in_block, targetAngle, cursor_rotation,
                                          type, hand, obj_shape, ppid)]

    # remove rows where type = "instruction"
    object_track_df <- object_track_df %>% filter(type != "instruction")

    # remove the first row of each trial -- this is always set to 0
    object_track_df <- object_track_df %>% 
      group_by(trial_num) %>% 
      slice(-1)
             
    object_track_df <- object_track_df %>% 
      group_by(trial_num) %>% 
      mutate(first_pos_x = first(pos_x),
             first_pos_z = first(pos_z),
             pos_x = pos_x - first_pos_x,
             pos_z = pos_z - first_pos_z,
             dist = sqrt(pos_x^2 + pos_z^2),
             pos_x_change = pos_x - lag(pos_x),
             pos_z_change = pos_z - lag(pos_z),
             angle = atan2_2d(pos_x, pos_z, targetAngle),
             angle_change = angle - lag(angle),
             angle_2nd_deriv = angle_change - lag(angle_change),
             abs_angle_2nd_deriv = abs(angle_2nd_deriv),
             smoothed_abs_angle_2nd_deriv = rollmean(abs_angle_2nd_deriv, 5, fill = NA),
             pos_change_dist = sqrt((pos_x_change)^2 + (pos_z_change)^2),
             dist_change = dist - lag(dist),
             time_change = x.time - lag(x.time),
             speed = abs(pos_change_dist / time_change),
             smoothed_speed = rollmean(speed, 5, fill = NA),
             max_smoothed_speed = max(smoothed_speed, na.rm = TRUE),
             norm_speed = smoothed_speed / max_smoothed_speed,
             start_move_event = norm_speed >= 0.20 &
               !duplicated(norm_speed >= 0.20))

    # save the file
    fwrite(object_track_df, file = paste(new_dir_path, "object_track_df.csv", sep = "/"))
  }
}

##### Main #####

# print(Sys.time())
# 
# make_per_frame_files(1)
# make_per_frame_files(2)
# make_per_frame_files(3)
# 
# print(Sys.time())

# run the functions in parallel
make_files_parallel <- function() {
  require(future)

  res1 %<-% make_per_frame_files(1) %seed% TRUE
  res2 %<-% make_per_frame_files(2) %seed% TRUE
  res3 %<-% make_per_frame_files(3) %seed% TRUE
}

plan(multisession)
make_files_parallel()

##### Test #####
plot_trials <- function(trial){
  # plot the velocity of one trial over time
  trial_df <- object_track_df %>% 
    filter(trial_num == trial) 
  
  f <- trial_df %>% 
    ggplot(aes(x = x.time, y = angle_change)) +
    geom_line(color = "purple", alpha = 0.2) +
    geom_line(aes(x = x.time, y = angle), color = "blue", alpha = 0.2) +
    geom_line(aes(x = x.time, y = angle_2nd_deriv), color = "red", alpha = 0.2) +
    geom_line(aes(x = x.time, y = smoothed_abs_angle_2nd_deriv), color = "green", alpha = 1) +
    geom_line(aes(x = x.time, y = norm_speed * 200), color = "yellow", alpha = 0.5)
    theme_bw() +
    labs(x = "Time (s)", y = "angle + derivatives", title = "Speed of Cursor Over Time")
  
  # add a vertical line where start_move_event = TRUE
  f <- f + geom_vline(aes(xintercept = x.time), data = trial_df %>% filter(start_move_event == TRUE), color = "grey")
  
  # add legend
  f <- f + theme(legend.position = "right")
  
  # save the plot
  ggsave(filename = paste("trial_", trial, "_angle.png", sep = ""), plot = f, path = "plots")
  
  
  # 
  # #### Position
  # f <- trial_df %>% 
  #   ggplot(aes(x = x.time, y = angle_change)) +
  #   geom_line(color = "purple", alpha = 0.2) +
  #   geom_line(aes(x = x.time, y = angle), color = "blue", alpha = 0.2) +
  #   geom_line(aes(x = x.time, y = angle_2nd_deriv), color = "red", alpha = 0.2) +
  #   geom_line(aes(x = x.time, y = smoothed_abs_angle_2nd_deriv), color = "green", alpha = 1) +
  #   theme_bw() +
  #   labs(x = "Time (s)", y = "position + derivatives", title = "Speed of Cursor Over Time")
  # 
  # # add a vertical line where start_move_event = TRUE
  # f <- f + geom_vline(aes(xintercept = x.time), data = trial_df %>% filter(start_move_event == TRUE), color = "grey")
  # 
  # # add legend
  # f <- f + theme(legend.position = "right")
  # 
  # # save the plot
  # ggsave(filename = paste("trial_", trial, "_position.png", sep = ""), plot = f, path = "plots")
  # 

  
  
  
  # plot the x and z positions of the cursor
  f2 <- trial_df %>% 
    ggplot(aes(x = pos_x, y = pos_z, color = angle_2nd_deriv)) +
    geom_point() +
    theme_bw() +
    labs(x = "X Position (m)", y = "Z Position (m)", title = "Cursor Position Over Time")
  
  # use a color palette that highlights 0
  f2 <- f2 + scale_color_gradient(low = "blue", high = "red")
  
  # add a dot where start_move_event = TRUE
  f2 <- f2 + geom_point(aes(x = pos_x, y = pos_z), data = trial_df %>% filter(start_move_event == TRUE), color = "red", size = 3)

  # add vertical and horizontal lines at the origin
  f2 <- f2 + geom_vline(xintercept = 0, color = "black", linetype = "dashed")
  f2 <- f2 + geom_hline(yintercept = 0, color = "black", linetype = "dashed")

  # set x and y limits
  f2 <- f2 + xlim(-0.20, 0.20) + ylim(-0.05, 0.20)
  
  # save the plot
  ggsave(filename = paste("trial_", trial, "_pos.png", sep = ""), plot = f2, path = "plots")
  
}

do_test <- function(){
  # make a range from 139 - 149 (rotated trials)
  rotated_trials <- seq(139, 158, 1)

  for (trial in rotated_trials){
    var <- paste("P", trial, sep = "")
      
    assign(var, future({plot_trials(trial)}))
  }
}

exp_index <- 2
path <- paste(raw_dir_path, exp_versions[exp_index], sep = "/")
ppt <- "10"

# do_test()
