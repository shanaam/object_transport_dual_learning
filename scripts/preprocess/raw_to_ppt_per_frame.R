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
library(future)

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

##### Single Rotation Experiments #####
make_single_rot_file <- function(exp_index) {
  path <- paste(raw_dir_path, exp_versions[exp_index], sep = "/")
  
  # loop through the files in path
  for (ppt in list.files(path = path)) {
    # load the trial results
    trial_df <- load_trial_result_csv(path, exp_index, ppt)

    # load the cursor object tracker
    cursor_df <- load_cursorobjecttracker_csv(path, ppt)
    
    # we need to use data table to do non-equi joins
    setDT(trial_df); setDT(cursor_df) # converting to data.table in place
    
    cursor_df <- cursor_df[trial_df, on = .(time >= start_time, time < end_time), 
                           nomatch = 0, .(pos_x, pos_y, pos_z, x.time, 
                                          start_time, end_time, trial_num, 
                                          block_num, trial_num_in_block, targetAngle,
                                          type, cursor_rotation, hand, pick_up_time,
                                          obj_shape, ppid)]

  }
}

##### Dual 30 Rotation Experiments #####


##### Dual 60 Rotation Experiments #####


##### Main #####
make_single_rot_file(1)