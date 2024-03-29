## --------------------------------
## Purpose of script: Make per_trial CSVs for trackerholder (the real hand),
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
library(collections)


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

# load and fix a csv
load_and_fix_csv <- function(path, exp_index, ppt) {
  df <- fread(path, stringsAsFactors = FALSE)

  # fix the ppid
  new_ppid <- paste(exp_short[exp_index], ppt, sep = "_")

  df$ppid <- NULL # get rid of ppid to avoid type coercion errors
  df$ppid <- new_ppid
  return(df)
}

# save modified csv
save_modified_csv <- function(df, processed_dir_path, exp_index, ppt) {
  # make a new directory path
  new_dir_path <- paste(processed_dir_path, "/", exp_short[exp_index], "_", ppt, sep = "")

  # create the directory for the participant
  create_dir(new_dir_path)

  fwrite(df, file = paste(new_dir_path, "trial_results.csv", sep = "/"))
}

# add miniblocks
add_miniblocks <- function(df) {
  # group by exp, ppid, block_num
  df <- df %>%
    group_by(type) %>%
    mutate(diff_dual_rotation = dual_rotation - lag(dual_rotation)) %>%
    mutate(rotation_switch = ifelse(diff_dual_rotation != 0 |
      is.na(diff_dual_rotation), 1, 0)) %>%
    mutate(rotation_switch = cumsum(rotation_switch)) %>%
    select(-diff_dual_rotation) %>%
    ungroup()

  # add trial_in_miniblock column
  df <- df %>%
    group_by(type, rotation_switch) %>%
    mutate(trial_in_miniblock = row_number()) %>%
    ungroup()

  return(df)
}


##### Single Rotation Experiments #####
make_single_rot_file <- function(exp_index) {
  path <- paste(raw_dir_path, exp_versions[exp_index], sep = "/")

  # loop through the files in path
  for (ppt in list.files(path = path)) {
    # update the path to the ppt
    csv_path <- paste(path, ppt, "S001", "trial_results.csv", sep = "/")

    # read in the csv
    df <- load_and_fix_csv(csv_path, exp_index, ppt)

    ##### any single_rot specific stuff here #####
    df$targetAngle <- df$targetAngle - df$distractor_loc

    # add a column called dual_rot
    df$dual_rotation <- df$cursor_rotation

    # add a column called positive_obj_shape
    opposite_obj_shape <- dict(
      list(
        "sphere" = "cube",
        "cube" = "sphere"
      )
    )

    df_rotated <- df %>% filter(type == "rotated")

    # label positive and negative object shape
    if (df_rotated[1, ]$dual_rotation == 30) {
      df$positive_obj_shape <- df_rotated[1, ]$obj_shape
    } else {
      df$positive_obj_shape <- opposite_obj_shape$get(df_rotated[1, ]$obj_shape)
    }

    # label trainedfirst object shape
    df$trainedfirst_obj_shape <- df_rotated[1, ]$obj_shape

    # change the column name pick_up_time to step_time
    df <- df %>% rename(step_time = pick_up_time)

    # add miniblocks
    df <- add_miniblocks(df)

    # calculate object/target angle (mislabeled in data)
    df <- df %>%
      mutate(targetAngle = targetAngle + distractor_loc)


    # save the modified csv
    save_modified_csv(df, processed_dir_path, exp_index, ppt)
  }
}

##### Dual 30 Rotation Experiments #####
make_dual_30_file <- function(exp_index) {
  path <- paste(raw_dir_path, exp_versions[exp_index], sep = "/")

  # loop through the files in path
  for (ppt in list.files(path = path)) {
    # update the path to the ppt
    csv_path <- paste(path, ppt, "S001", "trial_results.csv", sep = "/")

    # read in the csv
    df <- load_and_fix_csv(csv_path, exp_index, ppt)

    # any dual_rot specific stuff here
    # remove the blank columns
    df <- df[, -c(28:33)]

    # add a positive obj_shape column
    df_30 <- df %>% filter(dual_rotation == 30)
    first_obj_shape <- df_30[1, ]$obj_shape
    df$positive_obj_shape <- first_obj_shape

    # label trainedfirst object shape
    df$trainedfirst_obj_shape <- df_30[1, ]$obj_shape

    # add miniblocks
    df <- add_miniblocks(df)

    # save the modified csv
    save_modified_csv(df, processed_dir_path, exp_index, ppt)
  }
}

##### Dual 60 Rotation Experiments #####
make_dual_60_file <- function(exp_index) {
  path <- paste(raw_dir_path, exp_versions[exp_index], sep = "/")

  # loop through the files in path
  for (ppt in list.files(path = path)) {
    # update the path to the ppt
    csv_path <- paste(path, ppt, "S001", "trial_results.csv", sep = "/")

    # read in the csv
    df <- load_and_fix_csv(csv_path, exp_index, ppt)

    # any dual_rot specific stuff here
    # remove the blank columns
    df <- df[, -c(28:33)]

    # add a positive obj_shape column
    df_60 <- df %>% filter(dual_rotation == 60)
    first_obj_shape <- df_60[1, ]$obj_shape
    df$positive_obj_shape <- first_obj_shape

    # label trainedfirst object shape
    df$trainedfirst_obj_shape <- df_60[1, ]$obj_shape

    # add miniblocks
    df <- add_miniblocks(df)

    # save the modified csv
    save_modified_csv(df, processed_dir_path, exp_index, ppt)
  }
}

##### Main #####
# make_single_rot_file(1)
# make_dual_30_file(2)
# make_dual_60_file(3)

# run the functions in parallel
make_files_parallel <- function() {
  require(future)

  res1 %<-% make_single_rot_file(1) %seed% TRUE
  res2 %<-% make_dual_30_file(2) %seed% TRUE
  res3 %<-% make_dual_60_file(3) %seed% TRUE
}

plan(multisession)
make_files_parallel()
