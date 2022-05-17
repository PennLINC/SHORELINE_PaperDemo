# code to prepare the datasets

library(dplyr)
library(tibble)
library(here)
library(stringr)
library(tidyr)
library(reshape2)

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

# Calculate the errors
for (motion_axis in c("x", "y", "z")){
  for (motion_type in c("trans", "rot")){
    est.mov.name <- paste0(motion_type, "_", motion_axis)
    est.mov <- motion_df[est.mov.name]
    if (motion_type == "rot"){
      est.mov <- est.mov * 180 / pi
      motion_df[est.mov.name] <- est.mov
    }
    true.mov <- motion_df[paste0("true_", motion_type, "_", motion_axis)]
    error.col <- paste("error", motion_type, motion_axis, sep="_")
    motion_df[error.col] <- true.mov - est.mov
  }
}

# Get only the columns for the motion analysis
motion_columns <- c(
  "bval", "grad_x", "grad_y", "grad_z", "iternum", "scheme", "method", 
  "setting", "percent_motion", "error_trans_x", "error_trans_y",
  "error_trans_z", "error_rot_x", "error_rot_y", "error_rot_z",
  "true_trans_x", "true_trans_y", "true_trans_z", "true_rot_x", 
  "true_rot_y", "true_rot_z", "trans_x", "trans_y", "trans_z", "rot_x",
  "rot_y", "rot_z", "volnum", "denoising")


# write out the motion df, this will be used for table 1
motion_df <- as_tibble(motion_df)
usethis::use_data(motion_df, overwrite = TRUE)

moved <- motion_df[, motion_columns] %>%
  bind_cols(select(motion_df, matches("DWI")))


m.moved <- melt(moved,
                measure.vars = c(
                  "error_trans_x", "error_trans_y", "error_trans_z", "error_rot_x", 
                  "error_rot_y", "error_rot_z", "true_trans_x", "true_trans_y", 
                  "true_trans_z", "true_rot_x", "true_rot_y", "true_rot_z", "trans_x",
                  "trans_y", "trans_z", "rot_x", "rot_y", "rot_z"))

m.moved$motion.type <- 'Translation'
m.moved$motion.type[grepl('*rot_', m.moved$variable)] <- 'Rotation'
table(m.moved$motion.type)
m.moved$axis <- gsub(".*_([xyz])$", '\\1', m.moved$variable)
m.moved$source <- "Estimated"
m.moved$source[grep('true', m.moved$variable)] <- "Truth"
m.moved$source[grep('error', m.moved$variable)] <- "Error"
m.moved$percent_motion <- as.factor(m.moved$percent_motion)
m.moved$scheme <- factor(m.moved$scheme,
                         levels=c("ABCD", "HCP", "DSIQ5", "HASC55"))
drop_cols <- c("DWIBiasCorrect_pre", "grad_x", "grad_y", "grad_z",
               "DWIBiasCorrect_post", "DWIBiasCorrect_change", "DWIDenoise_pre",
               "DWIDenoise_post", "DWIDenoise_change", "variable")
error_df_wsetting <- subset(m.moved, (source=="Error"))  %>% 
  select(-one_of(drop_cols))
error_df_wsetting$source <- NULL

# get the sd of the error
error_rmse_wsetting <- error_df_wsetting %>%
  # Select only the b>0 images, since these use the model
  filter(bval > 100) %>%
  group_by(denoising, scheme, motion.type, method, percent_motion, iternum, setting) %>% 
  summarise(rmse=sqrt(sum(value^2)/sum(is.finite(value))),
            mean_error=mean(value),
            sd_error=sd(value))

rm(motion_df, m.moved, moved, motion_columns, drop_cols, est.mov,
   true.mov)

# write out the error rmse df, this will be used for analyses
error_rmse_wsetting <- as_tibble(error_rmse_wsetting)
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
