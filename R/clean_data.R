# code to prepare the datasets

library(dplyr)
library(tibble)
library(here)
library(stringr)
library(tidyr)

motion_df <- readRDS(here("inst", "extdata", "motion_benchmark.rds")) %>%
  tibble()

# --------------------------------#

# Rename the methods
motion_df <- motion_df %>%
  mutate(
    method = case_when(
      method == "eddy" ~ "Eddy",
      TRUE             ~ "SHORELine"
      )
    )

# convert any rotation columns from rad to degree
motion_df <- motion_df %>%
  mutate(
    across(
      matches("rot_"), ~.x * 180/pi
    )
  )

trans_rot_names <- names(motion_df) %>%
  str_subset("^trans|^rot")

calculate_error <- function(df, movement){
  
  # a quick tidyeval function
  # that allows the colname to be
  # passed in flexibly
  #
  # simply returns the `error_*movement*` column
  
  error <- sym(paste("error", movement, sep="_"))
  actual <- sym(paste("true", movement, sep="_"))
  movement_ <- sym(movement)
  
  mutate(df, !!error := !!actual - !!movement_) %>%
    select(!!error) %>%
    return()

}

for(name in trans_rot_names) {

  motion_df <- bind_cols(motion_df, calculate_error(motion_df, name))
  
}

# write out the motion df, this will be used for table 1
usethis::use_data(motion_df, overwrite = TRUE)

# Get only the columns for the motion analysis
moved <- motion_df %>%
  select(
    bval, contains("grad_"), iternum, scheme, method, setting, percent_motion,
    contains("error_"), contains("true"), contains("trans"), contains("rot"),
    volnum, denoising, matches("DWI")
  )

# pivot the df to make the rotations and translations one per row
moved_long <- pivot_longer(moved, names_to = "variable", values_to = "value",
                           cols = matches("trans|rot"))

moved_long <- moved_long %>%
  mutate(motion.type = case_when(str_detect(variable, "rot") ~ "Rotation",
                                 TRUE                        ~ "Translation"),
         axis = str_extract(variable, "[xyz]$"),
         source = case_when(str_detect(variable, "true")  ~ "Truth",
                            str_detect(variable, "error") ~ "Error",
                            TRUE                          ~ NA_character_),
         percent_motion = as.factor(percent_motion),
         scheme = as.factor(scheme)
         ) %>%
  select(!c(matches("grad_|DWIBias|DWIDenoise")))


error_df_wsetting <- moved_long %>%
  filter(source=="Error") %>%
  select(!source)

# get the sd of the error
error_rmse_wsetting <- error_df_wsetting %>%
  # Select only the b>0 images, since these use the model
  filter(bval > 100) %>%
  group_by(denoising, scheme, motion.type, method, percent_motion, iternum, setting) %>% 
  summarise(rmse=sqrt(sum(value^2)/sum(is.finite(value))),
            mean_error=mean(value),
            sd_error=sd(value))

# write out the error rmse df, this will be used for analyses
usethis::use_data(error_rmse_wsetting, overwrite = TRUE)

#---------------------------------#
# Load QC data and FWHM Correct it

qc_df <- droplevels(readRDS(here("inst", "extdata", "qc_benchmark.rds"))) %>%
  tibble::as_tibble()

qc_df <- qc_df %>%
  mutate(
    scheme = factor(scheme, levels=c("ABCD", "HCP", "DSIQ5", "HASC55")),
    method = recode(method, "3dSHORE"="SHORELine", "eddy"="Eddy"),
    denoising = recode(denoising, dwidenoise="MP-PCA", none="None")
    )

# A function to remove fwhm effect from NDC
fwhm_correct <- function(df, grp){
  df$fwhm.cen <- with(df, fwhm - mean(fwhm))

  # Partial out the fwhm effect in NDC
  ndc.model <- lm(t1_neighbor_corr ~ fwhm.cen, data=df)
  df$ndc.corrected <- ndc.model$coefficients[1] + residuals(ndc.model)

  return(tibble(df))
}

# Add a column for corrected ndc
resid_df <- qc_df %>%
  group_by(method, setting, scheme, denoising, percent_motion) %>%
  group_modify(~fwhm_correct(.x))

# Calculate the improvement in NDC relative to the original NDC
qc_df <- resid_df %>%
  mutate(
    improved.ndc = t1_neighbor_corr - raw_neighbor_corr,
    improved.ndc.corrected = ndc.corrected - raw_neighbor_corr
    )

usethis::use_data(qc_df, overwrite = TRUE)
