# Load packages and functions ---------------------------------------------

library(tidyverse)
library(nlme)
require(piecewiseSEM)
library(pROC)
source("R/get_MET.R")
source("R/get_HR_res.R")
source("R/get_kcal.R")
source("R/get_PAI_categories.R")

# Prepare files -----------------------------------------------------------

cardio       <- read_csv("data/cardio_data.csv")
hip_pri_acc  <- read_csv("data/hip_pri_acc.csv")
hip_sec_acc  <- read_csv("data/hip_sec_acc.csv")
back_pri_acc <- read_csv("data/back_pri_acc.csv")
back_sec_acc <- read_csv("data/back_sec_acc.csv")

# Compute MET values
cardio <- do.call(rbind, (lapply(unique(cardio$ID), get_MET, df = cardio)))
# Get PAI categories by MET
cardio <- get_PAI_categories(cardio)
# Compute percent HR reserve values
cardio <- do.call(rbind, (lapply(unique(cardio$ID), get_HR_res, df = cardio)))
# Compute Kcal values
cardio <- do.call(rbind, (lapply(unique(cardio$ID), get_kcal, df = cardio)))

# Merge related data frames
hip <- hip_pri_acc %>% 
  select(-c(MAD, ENMO)) %>% 
  full_join(hip_sec_acc, by = c("ID", "speed")) %>% 
  left_join(cardio, by = c("ID", "speed")) %>% 
  select(-c(BF, V.E, V.O2, Evaluation, Date))

back <- back_pri_acc %>% 
  select(-c(MAD, ENMO)) %>% 
  full_join(back_sec_acc, by = c("ID", "speed")) %>% 
  left_join(cardio, by = c("ID", "speed")) %>% 
  select(-c(BF, V.E, V.O2, Evaluation, Date))

# Sample descriptives -----------------------------------------------------

samp_desc <- read_csv("data/sample_descriptives_data.csv")
descriptives <- summarise(
  .data = samp_desc,
  age_mean    = round(mean(age), digits = 1),
  age_sd      = round(sd(age), digits = 1),
  weight_mean = round(mean(weight_kg), digits = 1),
  weight_sd   = round(sd(weight_kg), digits = 1),
  height_mean = round(mean(height_cm), digits = 1),
  height_sd   = round(sd(height_cm), digits = 1),
  BMI_mean    = round(mean(BMI_kgm2), digits = 1),
  BMI_sd      = round(sd(BMI_kgm2), digits = 1),
  fat_mean    = round(mean(body_fat), digits = 1),
  fat_sd      = round(sd(body_fat), digits = 1)
)
sex <- table(samp_desc$sex)

# Data analysis -----------------------------------------------------------

# ** Linear mixed models --------------------------------------------------

# Hip accelerometer
hip_AC_model <- lme(
  fixed = VO2.kg ~ AC + I(AC^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)
r2_hip_AC_model <- rsquared(hip_AC_model)

hip_MAD_model <- lme(
  fixed = VO2.kg ~ MAD + I(MAD^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)
r2_hip_MAD_model <- rsquared(hip_MAD_model)

hip_ENMO_model <- lme(
  fixed = VO2.kg ~ ENMO + I(ENMO^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)
r2_hip_ENMO_model <- rsquared(hip_ENMO_model)

# Back accelerometer
back_AC_model <- lme(
  fixed = VO2.kg ~ AC + I(AC^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back,
  na.action = na.omit
)
r2_back_AC_model <- rsquared(back_AC_model)

back_MAD_model <- lme(
  fixed = VO2.kg ~ MAD + I(MAD^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back,
  na.action = na.omit
)
r2_back_MAD_model <- rsquared(back_MAD_model)

back_ENMO_model <- lme(
  fixed = VO2.kg ~ ENMO + I(ENMO^2) + Age,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back,
  na.action = na.omit
)
r2_back_ENMO_model <- rsquared(back_ENMO_model)

# ROC curves --------------------------------------------------------------

# Hip accelerometer
