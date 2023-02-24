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

##### Single Rotation Experiments #####
make_single_rot_file <- function(exp_index) {
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

    # calculate the distance using pos_x and pos_z
    object_track_df <- object_track_df %>% 
      mutate(dist = sqrt(pos_x^2 + pos_z^2))
    
    # calculate the magnitude of the change in position
    object_track_df <- object_track_df %>% 
      mutate(pos_change = sqrt((pos_x - lag(pos_x))^2 + (pos_z - lag(pos_z))^2))

    # add a column for the change in dist, group by trial_num
    object_track_df <- object_track_df %>% 
      group_by(trial_num) %>% 
      mutate(dist_change = dist - lag(dist),
             time_change = x.time - lag(x.time))

    # add a column for speed (dist_change / time_change)
    object_track_df <- object_track_df %>% 
      mutate(speed = abs(pos_change / time_change))

    # add a column for smoothed velocity
    object_track_df <- object_track_df %>% 
      group_by(trial_num) %>% 
      mutate(smoothed_speed = rollmean(speed, 5, fill = NA))
    
    # add a column that is the maximum smoothed speed
    object_track_df <- object_track_df %>% 
      group_by(trial_num) %>% 
      mutate(max_smoothed_speed = max(smoothed_speed, na.rm = TRUE))

    # add a normalized speed column
    object_track_df <- object_track_df %>% 
      mutate(norm_speed = smoothed_speed / max_smoothed_speed)

    # per trial, find the first time the smoothed speed is greater than 0.1
    object_track_df <- object_track_df %>% 
      group_by(trial_num) %>% 
      mutate(start_move_event = norm_speed >= 0.15 &
               !duplicated(norm_speed >= 0.15))

    # save the file
    fwrite(object_track_df, file = paste(new_dir_path, "object_track_df.csv", sep = "/"))
  }
}

##### Dual 30 Rotation Experiments #####


##### Dual 60 Rotation Experiments #####


##### Main #####

# print(Sys.time())
# 
# make_single_rot_file(1)
# make_single_rot_file(2)
# make_single_rot_file(3)
# 
# print(Sys.time())

# run the functions in parallel
make_files_parallel <- function() {
  require(future)

  res1 %<-% make_single_rot_file(1) %seed% TRUE
  res2 %<-% make_single_rot_file(2) %seed% TRUE
  res3 %<-% make_single_rot_file(3) %seed% TRUE
}

plan(multisession)
make_files_parallel()

##### Test #####

# make a range from 139 - 149 (rotated trials)
rotated_trials <- seq(139, 149, 1)


for (trial in rotated_trials){
  # plot the velocity of one trial over time
  trial_df <- object_track_df %>% 
    filter(trial_num == trial) 
  
  f <- trial_df %>% 
    ggplot(aes(x = x.time, y = norm_speed)) +
    geom_line() +
    theme_bw() +
    labs(x = "Time (s)", y = "Speed (normalized)", title = "Speed of Cursor Over Time")

  # add a vertical line where start_move_event = TRUE
  f <- f + geom_vline(aes(xintercept = x.time), data = trial_df %>% filter(start_move_event == TRUE), color = "red")
  
  # save the plot
  ggsave(filename = paste("trial_", trial, "_speed.png", sep = ""), plot = f, path = "plots")

  # plot the x and z positions of the cursor
  f2 <- trial_df %>% 
    ggplot(aes(x = pos_x, y = pos_z, color = norm_speed)) +
    geom_point() +
    theme_bw() +
    labs(x = "X Position (m)", y = "Z Position (m)", title = "Cursor Position Over Time")

  # add a dot where start_move_event = TRUE
  f2 <- f2 + geom_point(aes(x = pos_x, y = pos_z), data = trial_df %>% filter(start_move_event == TRUE), color = "red", size = 3)
  
  # save the plot
  ggsave(filename = paste("trial_", trial, "_pos.png", sep = ""), plot = f2, path = "plots")
}