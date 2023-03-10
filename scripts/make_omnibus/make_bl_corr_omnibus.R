## --------------------------------
## Purpose of script: Make baseline correcct omnibus (per_trial) CSVs
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: uses the omnubus_raw csv from data/preprocessed
##
## --------------------------------

rm(list = ls()) # clean environment

source("src/helper_funcs.R")

library(data.table)
library(tidyverse)
library(future) # for parallel processing
library(collections) # for dicts

##### Variables #####
to_load_file_path <- "data/preprocessed/omnibus/omnibus_raw.csv"
to_save_dir_path <- "data/preprocessed/omnibus"

##### Helper functions #####
slope <- function(x, y) {
  return(cov(x, y) / var(x))
}

intercept <- function(x, y, slope) {
  b <- mean(y) - (slope * mean(x))
  return(b)
}

# note where might futures be useful?
make_bl_corrected_omnibus <- function() {
  # load file
  omnibus_df <- fread(to_load_file_path)

  practice_blocks <- dict(
    list(
      sr_30 = c(2, 3, 4),
      dr_30 = c(2),
      dr_60 = c(2)
    )
  )

  # label practice blocks
  omnibus_df$practice_block <- case_when(
    omnibus_df$exp == "sr_30" &
      omnibus_df$block_num %in% practice_blocks$get("sr_30") ~ TRUE,
    omnibus_df$exp == "dr_30" &
      omnibus_df$block_num %in% practice_blocks$get("dr_30") ~ TRUE,
    omnibus_df$exp == "dr_60" &
      omnibus_df$block_num %in% practice_blocks$get("dr_60") ~ TRUE,
    TRUE ~ FALSE
  )

  # remove practice blocks
  omnibus_df <- omnibus_df %>%
    filter(practice_block == FALSE) %>%
    select(-practice_block)

  # recode "aligned" and "rotated" in type to "reach"
  omnibus_df$type <- recode(omnibus_df$type,
    "aligned" = "reach",
    "rotated" = "reach"
  )

  baseline_blocks <- dict(
    list(
      sr_30 = c(6, 7, 8, 9, 10, 11, 12, 13),
      dr_30 = c(4, 5),
      dr_60 = c(4, 5)
    )
  )

  # label baseline blocks
  omnibus_df$baseline_block <- case_when(
    omnibus_df$exp == "sr_30" &
      omnibus_df$block_num %in% baseline_blocks$get("sr_30") ~ TRUE,
    omnibus_df$exp == "dr_30" &
      omnibus_df$block_num %in% baseline_blocks$get("dr_30") ~ TRUE,
    omnibus_df$exp == "dr_60" &
      omnibus_df$block_num %in% baseline_blocks$get("dr_60") ~ TRUE,
    TRUE ~ FALSE
  )

  # make a baseline df
  baseline_df <- omnibus_df %>%
    filter(baseline_block == TRUE) %>%
    select(-baseline_block)

  # remove rows where hand_angle_3cm_move is 3 standard deviations away from the mean
  baseline_df <- baseline_df %>%
    group_by(exp, ppid, type, hand) %>%
    filter(abs(mean(hand_angle_3cm_move) - hand_angle_3cm_move) < 3 * abs(sd(hand_angle_3cm_move))) %>%
    filter(abs(mean(obj_angle_3cm_move) - obj_angle_3cm_move) < 3 * abs(sd(obj_angle_3cm_move)))

  # samarise baseline df
  bl_df_summary <- baseline_df %>%
    group_by(exp, ppid, type, hand) %>%
    summarise(
      bl_hand_angle_3cm_move = median(hand_angle_3cm_move),
      bl_obj_angle_3cm_move = median(obj_angle_3cm_move),
    )

  # make bl_df_summary and omnibus_df data tables
  bl_df_summary <- as.data.table(bl_df_summary)
  omnibus_df <- as.data.table(omnibus_df)

  # non equi join omnibus_df and bl_df_summary
  omnibus_df <- omnibus_df[bl_df_summary,
    on = .(exp, ppid, type, hand),
    nomatch = 0
  ]

  # copy 3cm move columns
  omnibus_df$raw_hand_angle_3cm_move <- omnibus_df$hand_angle_3cm_move
  omnibus_df$raw_obj_angle_3cm_move <- omnibus_df$obj_angle_3cm_move

  # subtract baselines
  omnibus_df$hand_angle_3cm_move <- omnibus_df$raw_hand_angle_3cm_move - omnibus_df$bl_hand_angle_3cm_move
  omnibus_df$obj_angle_3cm_move <- omnibus_df$raw_obj_angle_3cm_move - omnibus_df$bl_obj_angle_3cm_move

  # rename the rotation_switch column to miniblock_num
  omnibus_df <- omnibus_df %>%
    rename(miniblock_num = rotation_switch)
  
  #### Outlier Removal ####
  omnibus_df <- omnibus_df %>%
    group_by(exp, ppid, type, hand) %>%
    filter(abs(mean(obj_angle_3cm_move) - obj_angle_3cm_move) < 3 * abs(sd(obj_angle_3cm_move)))

  return(omnibus_df)
}

add_columns_to_omnibus <- function(omnibus_df) {
  # group by exp, ppid, block_num, miniblock_num
  omnibus_df <- omnibus_df %>%
    group_by(exp, ppid, block_num, miniblock_num) %>%
    mutate(
      # add a column for the mean of the first 3 values of obj_angle_3cm_move
      obj_angles_when_rot_change = mean(obj_angle_3cm_move[1:3]),
      # add a column for the slope of the first 3 values of obj_angle_3cm_move
      obj_angles_when_rot_change_slope = slope(1:3, obj_angle_3cm_move[1:3]),
      obj_angles_when_rot_change_intercept = intercept(1:3, obj_angle_3cm_move[1:3], obj_angles_when_rot_change_slope)
    ) %>%
    ungroup()

  # add column for the delta in obj_angle_3cm_move
  omnibus_df <- omnibus_df %>%
    group_by(exp, ppid, block_num) %>%
    mutate(
      obj_angle_3cm_move_delta = obj_angle_3cm_move - lag(obj_angle_3cm_move, default = 0)
    ) %>%
    ungroup()

  return <- omnibus_df
}

process_and_save_baseline_df <- function() {
  omnibus_df <- make_bl_corrected_omnibus()
  omnibus_df <- add_columns_to_omnibus(omnibus_df)

  # save as a csv
  fwrite(omnibus_df, file = paste(to_save_dir_path, "omnibus_bl_corrected.csv", sep = "/"))
}

##### Do #####
process_and_save_baseline_df()




# # plot the distributuion of bl_hand_angle_3cm_move
# p <- omnibus_df %>%
#   ggplot(aes(x = bl_hand_angle_3cm_move)) +
#   geom_histogram(bins = 50) +
#   facet_wrap(~exp * hand) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank()
#   ) +
#   labs(
#     title = "Distribution of baseline hand angle 3cm move",
#     x = "Baseline hand angle 3cm move",
#     y = "Count"
#   )
#
# p
#
# # plot the distributuion of bl_obj_angle_3cm_move
# p <- omnibus_df %>%
#   ggplot(aes(x = bl_obj_angle_3cm_move)) +
#   geom_histogram(bins = 50) +
#   facet_wrap(~exp * hand) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank()
#   ) +
#   labs(
#     title = "Distribution of baseline object angle 3cm move",
#     x = "Baseline object angle 3cm move",
#     y = "Count"
#   )
# p
