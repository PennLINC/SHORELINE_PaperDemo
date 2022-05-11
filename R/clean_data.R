# code to prepare the datasets

library(tidyverse)
library(here)

motion_df <- readRDS(here("inst", "extdata", "motion_benchmark.rds")) %>%
  tibble()

# TODO demonstrate the table below
# single_subset <- subset(
#   motion_df, (iternum==1) & (method=="3dSHORE") & 
#     (denoising=="none") & (bval > 100) & (setting=="Affine"))
# 
# # Show the number of b>0's for each scheme (Table 1 in paper)
# with(single_subset, table(percent_motion, scheme))
# rm(single_subset)

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
usethis::use_data(error_df_wsetting, overwrite = TRUE)

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

# rm(motion_df, m.moved, moved, motion_columns, drop_cols, est.mov,
#    true.mov)

#---------------------------------#
# Load QC data and FWHM Correct it

# qc_df <- droplevels(readRDS("data/qc_benchmark.rds"))
# qc_df$scheme <- factor(
#   qc_df$scheme, levels=c("ABCD", "HCP", "DSIQ5", "HASC55"))
# qc_df$method <- recode(
#   qc_df$method, "3dSHORE"="SHORELine", "eddy"="Eddy")
# qc_df$denoising <- recode(
#   qc_df$denoising, dwidenoise="MP-PCA", none="None")
# id_cols <- c("scheme", "iternum", "method", "denoising", "percent_motion")
# 
# # Use to remove fwhm effect from NDC
# fwhm_correct <- function(df, grp){
#   df$fwhm.cen <- with(df, fwhm - mean(fwhm))
#   
#   # Partial out the fwhm effect in NDC
#   ndc.model <- lm(t1_neighbor_corr ~ fwhm.cen, data=df)
#   df$ndc.corrected <- ndc.model$coefficients[1] + residuals(ndc.model)
#   
#   return(tibble(df))
# }
# 
# # Add a column for corrected ndc
# resid_df <- qc_df %>% 
#   group_by(method, setting, scheme, denoising, percent_motion) %>% 
#   group_modify(~fwhm_correct(.x))
# 
# # Calculate the improvement in NDC relative to the original NDC
# resid_df$improved.ndc <- with(
#   resid_df, 
#   t1_neighbor_corr - raw_neighbor_corr)
# 
# resid_df$improved.ndc.corrected <- with(
#   resid_df, 
#   ndc.corrected - raw_neighbor_corr)
# 
# qc_df <- resid_df
# rm(resid_df)
