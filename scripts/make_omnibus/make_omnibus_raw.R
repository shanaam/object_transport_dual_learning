## --------------------------------
## Purpose of script: Make omnibus (per_trial) CSVs (no baseline correction)
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: uses data from data/preprocessed
##
## --------------------------------

rm(list = ls()) # clean environment

source("src/helper_funcs.R")

library(data.table)
library(tidyverse)
library(future) # for parallel processing

##### Variables #####
to_load_dir_path <- "data/preprocessed/per_ppt"
to_save_dir_path <- "data/preprocessed/omnibus"

##### All Experiments #####

# note assigning to lists before merging should be done via futures
make_omnibus_raw_file <- function() {
  ppt_list <- list.dirs(to_load_dir_path, recursive = FALSE)
  # make a list of length length(ppt_list) 
  trial_df_list <- vector("list", length(ppt_list))

  # loop through all directories in the to_load_dir_path
  for (i in 1:length(ppt_list)) {
    trial_df_list[[i]] <- future(make_one_omnibus_file(i, ppt_list))
  }

  # evaluate
  future_output <- value(trial_df_list)

  # row bind all the trial_dfs
  omnibus_df <- do.call(rbind, future_output)

  # save the omnibus_df
  fwrite(omnibus_df, file = paste(to_save_dir_path, "omnibus_raw.csv", sep = "/"))
}

make_one_omnibus_file <- function(directory_index, ppt_list) {

  exp_dir <- ppt_list[directory_index]

  # load in all trial_results.csv files
  trial_df <- fread(paste(exp_dir, "trial_results.csv", sep = "/"))
  hand_df <- fread(paste(exp_dir, "hand_track_df.csv", sep = "/"))
  obj_df <- fread(paste(exp_dir, "object_track_df.csv", sep = "/"))

  # remove instruction type trials
  trial_df <- trial_df %>% filter(type != "instruction")

  # remove some columns that end with "filename
  trial_df <- trial_df %>% select(-c(ends_with("filename"), "directory", "experiment"))

  # make a new column with the first 5 characters of the ppid
  trial_df$exp <- substr(trial_df$ppid, 1, 5)

  # make a per trial summary and join it to the trial_df 
  hand_df_summary <- hand_df %>%
    group_by(trial_num) %>%
    summarise(
      hand_time_start_move = first(time_start_move),
      hand_time_reach = first(time_reach),
      hand_time_total_move = first(time_total_move),
      hand_angle_3cm_move = first(angle_3cm_move),
    )

  trial_df <- trial_df %>%
    left_join(hand_df_summary, by = "trial_num")
  
  obj_df_summary <- obj_df %>%
    group_by(trial_num) %>%
    summarise(
      obj_time_start_move = first(time_start_move),
      obj_time_reach = first(time_reach),
      obj_time_total_move = first(time_total_move),
      obj_angle_3cm_move = first(angle_3cm_move),
    )

  trial_df <- trial_df %>%
    left_join(obj_df_summary, by = "trial_num")

  # return the trial_df
  return(trial_df)
}

##### Do #####
plan(multisession)
make_omnibus_raw_file()