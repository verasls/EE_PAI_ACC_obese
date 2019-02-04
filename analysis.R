# Load packages and functions ---------------------------------------------

library(here)
library(broman)
library(tidyverse)
library(nlme)
library(piecewiseSEM)
library(pROC)
library(irr)
library(pgirmess)
source(here("R", "get_MET.R"))
source(here("R", "get_PAI_categories.R"))
source(here("R", "get_HR_res.R"))
source(here("R", "get_kcal.R"))
source(here("R", "cross_validate_mixed_model.R"))
source(here("R", "cross_validate_ROC_curves.R"))
source(here("R", "accuracy_indices.R"))
source(here("R", "percent_agreement.R"))

# 1. Prepare files --------------------------------------------------------

cardio       <- read_csv(here("data", "cardio_data.csv")) %>% 
  select(ID, speed, Sex, Age, Body_mass, BMI, V.O2, V.CO2, VO2.kg)
hip_pri_acc  <- read_csv(here("data", "hip_pri_acc.csv"))
hip_sec_acc  <- read_csv(here("data", "hip_sec_acc.csv"))
back_pri_acc <- read_csv(here("data", "back_pri_acc.csv"))
back_sec_acc <- read_csv(here("data", "back_sec_acc.csv"))

# Compute kcal values
cardio <- do.call(rbind, (lapply(unique(cardio$ID), get_kcal, df = cardio)))
# Compute MET values
cardio <- do.call(rbind, (lapply(unique(cardio$ID), get_MET, df = cardio)))
# Get PAI categories by MET
cardio <- get_PAI_categories(cardio)

# Merge related data frames
hip <- hip_pri_acc %>% 
  select(-c(MAD, ENMO)) %>% 
  full_join(hip_sec_acc, by = c("ID", "speed")) %>% 
  left_join(cardio, by = c("ID", "speed"))

back <- back_pri_acc %>% 
  select(-c(MAD, ENMO)) %>% 
  full_join(back_sec_acc, by = c("ID", "speed")) %>% 
  left_join(cardio, by = c("ID", "speed"))

# 2. Sample descriptives --------------------------------------------------

# Demographics
samp_desc <- read_csv(here("data", "sample_descriptives_data.csv"))
sample_descriptives <- summarise(
  .data = samp_desc,
  age_mean       = round(mean(age), digits = 1),
  age_sd         = round(sd(age), digits = 1),
  body_mass_mean = round(mean(body_mass_kg), digits = 1),
  body_mass_sd   = round(sd(body_mass_kg), digits = 1),
  height_mean    = round(mean(height_cm), digits = 1),
  height_sd      = round(sd(height_cm), digits = 1),
  BMI_mean       = round(mean(BMI_kgm2), digits = 1),
  BMI_sd         = round(sd(BMI_kgm2), digits = 1),
  fat_mean       = round(mean(body_fat), digits = 1),
  fat_sd         = round(sd(body_fat), digits = 1)
)
sex <- table(samp_desc$sex)

# Cardiorespiratory and accelerometry
hip_descriptives <- hip %>% 
  group_by(speed) %>% 
  summarise(
    n             = n(),
    kcal_mean     = myround(mean(kcal), 2),
    kcal_sd       = myround(sd(kcal), 2),
    MET_mean      = myround(mean(MET), 2),
    MET_sd        = myround(sd(MET), 2),
    hip_AC_mean   = round(mean(AC, na.rm = TRUE), 0),
    hip_AC_sd     = round(sd(AC, na.rm = TRUE), 0),
    hip_MAD_mean  = round(mean(MAD, na.rm = TRUE), 0),
    hip_MAD_sd    = round(sd(MAD, na.rm = TRUE), 0),
    hip_ENMO_mean = round(mean(ENMO, na.rm = TRUE), 0), 
    hip_ENMO_sd   = round(sd(ENMO, na.rm = TRUE), 0)
  )

back_descriptives <- back %>% 
  group_by(speed) %>% 
  summarise(
    n              = n(),
    kcal_mean      = myround(mean(kcal), 2),
    kcal_sd        = myround(sd(kcal), 2),
    MET_mean       = myround(mean(MET), 2),
    MET_sd         = myround(sd(MET), 2),
    back_AC_mean   = round(mean(AC, na.rm = TRUE), 0),
    back_AC_sd     = round(sd(AC, na.rm = TRUE), 0),
    back_MAD_mean  = round(mean(MAD, na.rm = TRUE), 0),
    back_MAD_sd    = round(sd(MAD, na.rm = TRUE), 0),
    back_ENMO_mean = round(mean(ENMO, na.rm = TRUE), 0), 
    back_ENMO_sd   = round(sd(ENMO, na.rm = TRUE), 0)
  )

## Merging both accelerometer placements
## For cardio variables, keeping back due to larger n
descriptives <- hip_descriptives %>% 
  select(-c(n, kcal_mean, kcal_sd, MET_mean, MET_sd)) %>% 
  full_join(back_descriptives, by = "speed") %>% 
  select(
    speed, n, kcal_mean, kcal_sd, MET_mean, MET_sd,
    hip_AC_mean, hip_AC_sd, back_AC_mean, back_AC_sd,
    hip_MAD_mean, hip_MAD_sd, back_MAD_mean, back_MAD_sd,
    hip_ENMO_mean, hip_ENMO_sd, back_ENMO_mean, back_ENMO_sd,
  )

# 3. Data analysis --------------------------------------------------------

# * 3.1 Linear mixed models -----------------------------------------------

# Hip accelerometer
## AC
hip_AC_model <- lme(
  fixed = kcal ~ AC + I(AC^2) + Body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)
r2_hip_AC_model <- rsquared(hip_AC_model)

## MAD
hip_MAD_model <- lme(
  fixed = kcal ~ MAD + I(MAD^2) + Body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)
r2_hip_MAD_model <- rsquared(hip_MAD_model)

## ENMO
hip_ENMO_model <- lme(
  fixed = kcal ~ ENMO + I(ENMO^2) + Body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = hip,
  na.action = na.omit
)
r2_hip_ENMO_model <- rsquared(hip_ENMO_model)

# Back accelerometer
## AC
back_AC_model <- lme(
  fixed = kcal ~ AC + I(AC^2) + Body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back,
  na.action = na.omit
)
r2_back_AC_model <- rsquared(back_AC_model)

## MAD
back_MAD_model <- lme(
  fixed = kcal ~ MAD + I(MAD^2) + Body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back,
  na.action = na.omit
)
r2_back_MAD_model <- rsquared(back_MAD_model)

## ENMO
back_ENMO_model <- lme(
  fixed = kcal ~ ENMO + I(ENMO^2) + Body_mass,
  random = ~ 1 | ID,
  method = "ML",
  correlation = corAR1(),
  data = back,
  na.action = na.omit
)
r2_back_ENMO_model <- rsquared(back_ENMO_model)

# * 3.2 ROC curves --------------------------------------------------------

# Hip accelerometer
## AC
hip_AC_ROC_SED <- roc(SED_CAT_by_MET ~ AC, ci = TRUE, data = hip, na.rm = TRUE)
hip_AC_ROC_MOD <- roc(MOD_CAT_by_MET ~ AC, ci = TRUE, data = hip, na.rm = TRUE)
hip_AC_ROC_VIG <- roc(VIG_CAT_by_MET ~ AC, ci = TRUE, data = hip, na.rm = TRUE)
cp_hip_AC_ROC_SED <- coords(hip_AC_ROC_SED, x = "best", best.method = "closest.topleft")
cp_hip_AC_ROC_MOD <- coords(hip_AC_ROC_MOD, x = "best", best.method = "closest.topleft")
cp_hip_AC_ROC_VIG <- coords(hip_AC_ROC_VIG, x = "best", best.method = "closest.topleft")

## MAD
hip_MAD_ROC_SED <- roc(SED_CAT_by_MET ~ MAD, ci = TRUE, data = hip, na.rm = TRUE)
hip_MAD_ROC_MOD <- roc(MOD_CAT_by_MET ~ MAD, ci = TRUE, data = hip, na.rm = TRUE)
hip_MAD_ROC_VIG <- roc(VIG_CAT_by_MET ~ MAD, ci = TRUE, data = hip, na.rm = TRUE)
cp_hip_MAD_ROC_SED <- coords(hip_MAD_ROC_SED, x = "best", best.method = "closest.topleft")
cp_hip_MAD_ROC_MOD <- coords(hip_MAD_ROC_MOD, x = "best", best.method = "closest.topleft")
cp_hip_MAD_ROC_VIG <- coords(hip_MAD_ROC_VIG, x = "best", best.method = "closest.topleft")

## ENMO
hip_ENMO_ROC_SED <- roc(SED_CAT_by_MET ~ ENMO, ci = TRUE, data = hip, na.rm = TRUE)
hip_ENMO_ROC_MOD <- roc(MOD_CAT_by_MET ~ ENMO, ci = TRUE, data = hip, na.rm = TRUE)
hip_ENMO_ROC_VIG <- roc(VIG_CAT_by_MET ~ ENMO, ci = TRUE, data = hip, na.rm = TRUE)
cp_hip_ENMO_ROC_SED <- coords(hip_ENMO_ROC_SED, x = "best", best.method = "closest.topleft")
cp_hip_ENMO_ROC_MOD <- coords(hip_ENMO_ROC_MOD, x = "best", best.method = "closest.topleft")
cp_hip_ENMO_ROC_VIG <- coords(hip_ENMO_ROC_VIG, x = "best", best.method = "closest.topleft")

# Back accelerometer
## AC
back_AC_ROC_SED <- roc(SED_CAT_by_MET ~ AC, ci = TRUE, data = back, na.rm = TRUE)
back_AC_ROC_MOD <- roc(MOD_CAT_by_MET ~ AC, ci = TRUE, data = back, na.rm = TRUE)
back_AC_ROC_VIG <- roc(VIG_CAT_by_MET ~ AC, ci = TRUE, data = back, na.rm = TRUE)
cp_back_AC_ROC_SED <- coords(back_AC_ROC_SED, x = "best", best.method = "closest.topleft")
cp_back_AC_ROC_MOD <- coords(back_AC_ROC_MOD, x = "best", best.method = "closest.topleft")
cp_back_AC_ROC_VIG <- coords(back_AC_ROC_VIG, x = "best", best.method = "closest.topleft")

## MAD
back_MAD_ROC_SED <- roc(SED_CAT_by_MET ~ MAD, ci = TRUE, data = back, na.rm = TRUE)
back_MAD_ROC_MOD <- roc(MOD_CAT_by_MET ~ MAD, ci = TRUE, data = back, na.rm = TRUE)
back_MAD_ROC_VIG <- roc(VIG_CAT_by_MET ~ MAD, ci = TRUE, data = back, na.rm = TRUE)
cp_back_MAD_ROC_SED <- coords(back_MAD_ROC_SED, x = "best", best.method = "closest.topleft")
cp_back_MAD_ROC_MOD <- coords(back_MAD_ROC_MOD, x = "best", best.method = "closest.topleft")
cp_back_MAD_ROC_VIG <- coords(back_MAD_ROC_VIG, x = "best", best.method = "closest.topleft")

## ENMO
back_ENMO_ROC_SED <- roc(SED_CAT_by_MET ~ ENMO, ci = TRUE, data = back, na.rm = TRUE)
back_ENMO_ROC_MOD <- roc(MOD_CAT_by_MET ~ ENMO, ci = TRUE, data = back, na.rm = TRUE)
back_ENMO_ROC_VIG <- roc(VIG_CAT_by_MET ~ ENMO, ci = TRUE, data = back, na.rm = TRUE)
cp_back_ENMO_ROC_SED <- coords(back_ENMO_ROC_SED, x = "best", best.method = "closest.topleft")
cp_back_ENMO_ROC_MOD <- coords(back_ENMO_ROC_MOD, x = "best", best.method = "closest.topleft")
cp_back_ENMO_ROC_VIG <- coords(back_ENMO_ROC_VIG, x = "best", best.method = "closest.topleft")

# * 3.3 Validity analysis -------------------------------------------------

# ** 3.3.1 Leave-one-out cross validation ---------------------------------
# Mixed models
## Hip accelerometer
### AC
fix_eff    <- kcal ~ AC + I(AC^2) + Body_mass
rand_eff   <- ~ 1 | ID
acc_metric <- "AC"
LOOCV_hip_AC_model <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

### MAD
fix_eff    <- kcal ~ MAD + I(MAD^2) + Body_mass
rand_eff   <- ~ 1 | ID
acc_metric <- "MAD"
LOOCV_hip_MAD_model <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

### ENMO
fix_eff    <- kcal ~ ENMO + I(ENMO^2) + Body_mass
rand_eff   <- ~ 1 | ID
acc_metric <- "ENMO"
LOOCV_hip_ENMO_model <- do.call(rbind, (lapply(unique(hip$ID), cross_validate_mixed_model, df = hip)))

## Back accelerometer
### AC
fix_eff    <- kcal ~ AC + I(AC^2) + Body_mass
rand_eff   <- ~ 1 | ID
acc_metric <- "AC"
LOOCV_back_AC_model <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))

### MAD
fix_eff    <- kcal ~ MAD + I(MAD^2) + Body_mass
rand_eff   <- ~ 1 | ID
acc_metric <- "MAD"
LOOCV_back_MAD_model <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))

### ENMO
fix_eff    <- kcal ~ ENMO + I(ENMO^2) + Body_mass
rand_eff   <- ~ 1 | ID
acc_metric <- "ENMO"
LOOCV_back_ENMO_model <- do.call(rbind, (lapply(unique(back$ID), cross_validate_mixed_model, df = back)))

# ROC curves
## Hip accelerometer
### AC
LOOCV_hip_AC_ROC <- do.call(rbind, (lapply(unique(hip$ID)[-1], # drop 1st ID (36); does not have valid AC
                                    cross_validate_ROC_curves, 
                                    df = hip, acc_metric = "AC")))

### MAD
LOOCV_hip_MAD_ROC <- do.call(rbind, (lapply(unique(hip$ID),
                                     cross_validate_ROC_curves,
                                     df = hip, acc_metric = "MAD")))

### ENMO
LOOCV_hip_ENMO_ROC <- do.call(rbind, (lapply(unique(hip$ID),
                                      cross_validate_ROC_curves,
                                      df = hip, acc_metric = "ENMO")))

## Back accelerometer
### AC
LOOCV_back_AC_ROC <- do.call(rbind, (lapply(unique(back$ID), 
                                     cross_validate_ROC_curves, 
                                     df = back, acc_metric = "AC")))

### MAD
LOOCV_back_MAD_ROC <- do.call(rbind, (lapply(unique(back$ID),
                                      cross_validate_ROC_curves,
                                      df = back, acc_metric = "MAD")))

### ENMO
LOOCV_back_ENMO_ROC <- do.call(rbind, (lapply(unique(back$ID),
                                       cross_validate_ROC_curves,
                                       df = back, acc_metric = "ENMO")))

# ** 3.3.2 Bland-Altman Plots ---------------------------------------------

# Hip accelerometer
## AC
LOOCV_hip_AC_model$diff <- LOOCV_hip_AC_model$kcal - LOOCV_hip_AC_model$kcal_predicted
LOOCV_hip_AC_model$mean <- (LOOCV_hip_AC_model$kcal + LOOCV_hip_AC_model$kcal_predicted) / 2
hip_AC_BA_plot <- ggplot(data = LOOCV_hip_AC_model) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_AC_model$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_AC_model$diff) + 1.96 * sd(LOOCV_hip_AC_model$diff),
    linetype = "dotted"
    ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_AC_model$diff) - 1.96 * sd(LOOCV_hip_AC_model$diff),
    linetype = "dotted"
  )

## MAD
LOOCV_hip_MAD_model$diff <- LOOCV_hip_MAD_model$kcal - LOOCV_hip_MAD_model$kcal_predicted
LOOCV_hip_MAD_model$mean <- (LOOCV_hip_MAD_model$kcal + LOOCV_hip_MAD_model$kcal_predicted) / 2
hip_MAD_BA_plot <- ggplot(data = LOOCV_hip_MAD_model) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_MAD_model$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_MAD_model$diff) + 1.96 * sd(LOOCV_hip_MAD_model$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_MAD_model$diff) - 1.96 * sd(LOOCV_hip_MAD_model$diff),
    linetype = "dotted"
  )

## ENMO
LOOCV_hip_ENMO_model$diff <- LOOCV_hip_ENMO_model$kcal - LOOCV_hip_ENMO_model$kcal_predicted
LOOCV_hip_ENMO_model$mean <- (LOOCV_hip_ENMO_model$kcal + LOOCV_hip_ENMO_model$kcal_predicted) / 2
hip_ENMO_BA_plot <- ggplot(data = LOOCV_hip_ENMO_model) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_hip_ENMO_model$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_ENMO_model$diff) + 1.96 * sd(LOOCV_hip_ENMO_model$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_ENMO_model$diff) - 1.96 * sd(LOOCV_hip_ENMO_model$diff),
    linetype = "dotted"
  )

# Back accelerometer
## AC
LOOCV_back_AC_model$diff <- LOOCV_back_AC_model$kcal - LOOCV_back_AC_model$kcal_predicted
LOOCV_back_AC_model$mean <- (LOOCV_back_AC_model$kcal + LOOCV_back_AC_model$kcal_predicted) / 2
back_AC_BA_plot <- ggplot(data = LOOCV_back_AC_model) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_AC_model$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_AC_model$diff) + 1.96 * sd(LOOCV_back_AC_model$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_AC_model$diff) - 1.96 * sd(LOOCV_back_AC_model$diff),
    linetype = "dotted"
  )

## MAD
LOOCV_back_MAD_model$diff <- LOOCV_back_MAD_model$kcal - LOOCV_back_MAD_model$kcal_predicted
LOOCV_back_MAD_model$mean <- (LOOCV_back_MAD_model$kcal + LOOCV_back_MAD_model$kcal_predicted) / 2
back_MAD_BA_plot <- ggplot(data = LOOCV_back_MAD_model) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_MAD_model$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_MAD_model$diff) + 1.96 * sd(LOOCV_back_MAD_model$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_MAD_model$diff) - 1.96 * sd(LOOCV_back_MAD_model$diff),
    linetype = "dotted"
  )

## ENMO
LOOCV_back_ENMO_model$diff <- LOOCV_back_ENMO_model$kcal - LOOCV_back_ENMO_model$kcal_predicted
LOOCV_back_ENMO_model$mean <- (LOOCV_back_ENMO_model$kcal + LOOCV_back_ENMO_model$kcal_predicted) / 2
back_ENMO_BA_plot <- ggplot(data = LOOCV_back_ENMO_model) +
  geom_point(mapping = aes(x = mean, y = diff)) +
  geom_hline(yintercept = mean(LOOCV_back_ENMO_model$diff)) +
  geom_hline(
    yintercept = mean(LOOCV_back_ENMO_model$diff) + 1.96 * sd(LOOCV_back_ENMO_model$diff),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_ENMO_model$diff) - 1.96 * sd(LOOCV_back_ENMO_model$diff),
    linetype = "dotted"
  )

# Linear regressions
# Hip accelerometer
## AC
hip_AC_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_AC_model)
summary(hip_AC_BA_plot_LR)

## MAD
hip_MAD_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_MAD_model)
summary(hip_MAD_BA_plot_LR)

## ENMO
hip_ENMO_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_hip_ENMO_model)
summary(hip_ENMO_BA_plot_LR)

# Back accelerometer
## AC
back_AC_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_AC_model)
summary(back_AC_BA_plot_LR)

## MAD
back_MAD_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_MAD_model)
summary(back_MAD_BA_plot_LR)

## ENMO
back_ENMO_BA_plot_LR <- lm(diff ~ mean, data = LOOCV_back_ENMO_model)
summary(back_ENMO_BA_plot_LR)

# ** 3.3.3 Indices of accuracy --------------------------------------------

# Hip accelerometer
## AC
hip_AC_model_accuracy <- accuracy_indices(LOOCV_hip_AC_model, "kcal", "kcal_predicted")

## MAD
hip_MAD_model_acccuracy <- accuracy_indices(LOOCV_hip_MAD_model, "kcal", "kcal_predicted")

## ENMO
hip_ENMO_model_acccuracy <- accuracy_indices(LOOCV_hip_ENMO_model, "kcal", "kcal_predicted")

# Back accelerometer
## AC
back_AC_model_accuracy <- accuracy_indices(LOOCV_back_AC_model, "kcal", "kcal_predicted")

## MAD
back_MAD_model_acccuracy <- accuracy_indices(LOOCV_back_MAD_model, "kcal", "kcal_predicted")

## ENMO
back_ENMO_model_acccuracy <- accuracy_indices(LOOCV_back_ENMO_model, "kcal", "kcal_predicted")

# ** 3.3.4 Kappa statistic ------------------------------------------------

# Hip accelerometer
## AC
kappa_hip_AC_SED  <- kappa2(select(LOOCV_hip_AC_ROC, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_hip_AC_MOD  <- kappa2(select(LOOCV_hip_AC_ROC, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_hip_AC_VIG  <- kappa2(select(LOOCV_hip_AC_ROC, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_hip_AC_glob <- kappa2(select(LOOCV_hip_AC_ROC, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                            weight = "squared")

## MAD
kappa_hip_MAD_SED  <- kappa2(select(LOOCV_hip_MAD_ROC, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_hip_MAD_MOD  <- kappa2(select(LOOCV_hip_MAD_ROC, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_hip_MAD_VIG  <- kappa2(select(LOOCV_hip_MAD_ROC, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_hip_MAD_glob <- kappa2(select(LOOCV_hip_MAD_ROC, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                             weight = "squared")

## ENMO
kappa_hip_ENMO_SED  <- kappa2(select(LOOCV_hip_ENMO_ROC, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_hip_ENMO_MOD  <- kappa2(select(LOOCV_hip_ENMO_ROC, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_hip_ENMO_VIG  <- kappa2(select(LOOCV_hip_ENMO_ROC, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_hip_ENMO_glob <- kappa2(select(LOOCV_hip_ENMO_ROC, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                              weight = "squared")

# Back accelerometer
## AC
kappa_back_AC_SED  <- kappa2(select(LOOCV_back_AC_ROC, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_back_AC_MOD  <- kappa2(select(LOOCV_back_AC_ROC, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_back_AC_VIG  <- kappa2(select(LOOCV_back_AC_ROC, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_back_AC_glob <- kappa2(select(LOOCV_back_AC_ROC, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                             weight = "squared")

## MAD
kappa_back_MAD_SED  <- kappa2(select(LOOCV_back_MAD_ROC, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_back_MAD_MOD  <- kappa2(select(LOOCV_back_MAD_ROC, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_back_MAD_VIG  <- kappa2(select(LOOCV_back_MAD_ROC, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_back_MAD_glob <- kappa2(select(LOOCV_back_MAD_ROC, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                              weight = "squared")

## ENMO
kappa_back_ENMO_SED  <- kappa2(select(LOOCV_back_ENMO_ROC, SED_CAT_by_MET, SED_CAT_by_ROC))
kappa_back_ENMO_MOD  <- kappa2(select(LOOCV_back_ENMO_ROC, MOD_CAT_by_MET, MOD_CAT_by_ROC))
kappa_back_ENMO_VIG  <- kappa2(select(LOOCV_back_ENMO_ROC, VIG_CAT_by_MET, VIG_CAT_by_ROC))
kappa_back_ENMO_glob <- kappa2(select(LOOCV_back_ENMO_ROC, INTENS_CAT_by_MET, INTENS_CAT_by_ROC), 
                               weight = "squared")

# ** 3.3.5 Percent agreement ----------------------------------------------

# Hip accelerometer
## AC
perc_agree_hip_AC_ROC <- percent_agreement(LOOCV_hip_AC_ROC, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## MAD
perc_agree_hip_MAD_ROC <- percent_agreement(LOOCV_hip_MAD_ROC, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## ENMO
perc_agree_hip_ENMO_ROC <- percent_agreement(LOOCV_hip_ENMO_ROC, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

# Back accelerometer
## AC
perc_agree_back_AC_ROC <- percent_agreement(LOOCV_back_AC_ROC, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## MAD
perc_agree_back_MAD_ROC <- percent_agreement(LOOCV_back_MAD_ROC, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

## ENMO
perc_agree_back_ENMO_ROC <- percent_agreement(LOOCV_back_ENMO_ROC, "INTENS_CAT_by_MET", "INTENS_CAT_by_ROC")

# ** 3.3.6 Prediction accuracy comparison ---------------------------------

# Hip accelerometer (among accelerometer metrics)
## Building data frame
pred_error_hip_AC <- LOOCV_hip_AC_model %>% 
  select(ID, speed, kcal, kcal_predicted) %>% 
  mutate(abs_error_hip_AC = abs(kcal - kcal_predicted)) %>% 
  select(ID, speed, abs_error_hip_AC)

pred_error_hip_MAD <- LOOCV_hip_MAD_model %>% 
  select(ID, speed, kcal, kcal_predicted) %>% 
  mutate(abs_error_hip_MAD = abs(kcal - kcal_predicted)) %>% 
  select(ID, speed, abs_error_hip_MAD)

pred_error_hip_ENMO <- LOOCV_hip_ENMO_model %>% 
  select(ID, speed, kcal, kcal_predicted) %>% 
  mutate(abs_error_hip_ENMO = abs(kcal - kcal_predicted)) %>% 
  select(ID, speed, abs_error_hip_ENMO)

hip_ANOVA_df <- pred_error_hip_AC %>%
  left_join(pred_error_hip_MAD, by = c("ID", "speed")) %>% 
  left_join(pred_error_hip_ENMO, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_AC, abs_error_hip_MAD, abs_error_hip_ENMO,
    key = metric,
    value = absolute_error
  )

## Running test
hip_ANOVA <- aov(absolute_error ~ metric, data = hip_ANOVA_df)
summary(hip_ANOVA)

# Back accelerometer (among accelerometer metrics)
## Building data frame
pred_error_back_AC <- LOOCV_back_AC_model %>% 
  select(ID, speed, kcal, kcal_predicted) %>% 
  mutate(abs_error_back_AC = abs(kcal - kcal_predicted)) %>% 
  select(ID, speed, abs_error_back_AC)

pred_error_back_MAD <- LOOCV_back_MAD_model %>% 
  select(ID, speed, kcal, kcal_predicted) %>% 
  mutate(abs_error_back_MAD = abs(kcal - kcal_predicted)) %>% 
  select(ID, speed, abs_error_back_MAD)

pred_error_back_ENMO <- LOOCV_back_ENMO_model %>% 
  select(ID, speed, kcal, kcal_predicted) %>% 
  mutate(abs_error_back_ENMO = abs(kcal - kcal_predicted)) %>% 
  select(ID, speed, abs_error_back_ENMO)

back_ANOVA_df <- pred_error_back_AC %>%
  left_join(pred_error_back_MAD, by = c("ID", "speed")) %>% 
  left_join(pred_error_back_ENMO, by = c("ID", "speed")) %>% 
  gather(
    abs_error_back_AC, abs_error_back_MAD, abs_error_back_ENMO,
    key = metric,
    value = absolute_error
  )

## Running test
back_ANOVA <- aov(absolute_error ~ metric, data = back_ANOVA_df)
summary(back_ANOVA)

# AC metric (between accelerometers)
## Building data frame
AC_ttest_df <- pred_error_hip_AC %>% 
  left_join(pred_error_back_AC, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_AC, abs_error_back_AC,
    key = placement,
    value = absolute_error
  )

## Running test
AC_ttest <- t.test(absolute_error ~ placement, data = AC_ttest_df)
AC_ttest

# MAD metric (between accelerometers)
## Building data frame
MAD_ttest_df <- pred_error_hip_MAD %>% 
  left_join(pred_error_back_MAD, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_MAD, abs_error_back_MAD,
    key = placement,
    value = absolute_error
  )

## Running test
MAD_ttest <- t.test(absolute_error ~ placement, data = MAD_ttest_df)
MAD_ttest

# ENMO metric (between accelerometers)
## Building data frame
ENMO_ttest_df <- pred_error_hip_ENMO %>% 
  left_join(pred_error_back_ENMO, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_ENMO, abs_error_back_ENMO,
    key = placement,
    value = absolute_error
  )

## Running test
ENMO_ttest <- t.test(absolute_error ~ placement, data = ENMO_ttest_df)
ENMO_ttest

# ** 3.3.7 Classification agreement comparison ----------------------------

# Hip accelerometer (among accelerometer metrics)
## Building data frame
class_error_hip_AC <- LOOCV_hip_AC_ROC %>% 
  select(ID, speed, INTENS_CAT_by_MET, INTENS_CAT_by_ROC) %>% 
  mutate(abs_error_hip_AC = abs(INTENS_CAT_by_MET - INTENS_CAT_by_ROC)) %>% 
  select(ID, speed, abs_error_hip_AC)

class_error_hip_MAD <- LOOCV_hip_MAD_ROC %>% 
  select(ID, speed, INTENS_CAT_by_MET, INTENS_CAT_by_ROC) %>% 
  mutate(abs_error_hip_MAD = abs(INTENS_CAT_by_MET - INTENS_CAT_by_ROC)) %>% 
  select(ID, speed, abs_error_hip_MAD)

class_error_hip_ENMO <- LOOCV_hip_ENMO_ROC %>% 
  select(ID, speed, INTENS_CAT_by_MET, INTENS_CAT_by_ROC) %>% 
  mutate(abs_error_hip_ENMO = abs(INTENS_CAT_by_MET - INTENS_CAT_by_ROC)) %>% 
  select(ID, speed, abs_error_hip_ENMO)

hip_KW_df <- class_error_hip_AC %>% 
  left_join(class_error_hip_MAD, by = c("ID", "speed")) %>% 
  left_join(class_error_hip_ENMO, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_AC, abs_error_hip_MAD, abs_error_hip_ENMO,
    key = metric,
    value = absolute_error
  )
hip_KW_df$metric <- as_factor(hip_KW_df$metric)

## Running test
hip_KW <- kruskal.test(absolute_error ~ metric, data = hip_KW_df)
hip_KW

# Back accelerometer (among accelerometer metrics)
## Building data frame
class_error_back_AC <- LOOCV_back_AC_ROC %>% 
  select(ID, speed, INTENS_CAT_by_MET, INTENS_CAT_by_ROC) %>% 
  mutate(abs_error_back_AC = abs(INTENS_CAT_by_MET - INTENS_CAT_by_ROC)) %>% 
  select(ID, speed, abs_error_back_AC)

class_error_back_MAD <- LOOCV_back_MAD_ROC %>% 
  select(ID, speed, INTENS_CAT_by_MET, INTENS_CAT_by_ROC) %>% 
  mutate(abs_error_back_MAD = abs(INTENS_CAT_by_MET - INTENS_CAT_by_ROC)) %>% 
  select(ID, speed, abs_error_back_MAD)

class_error_back_ENMO <- LOOCV_back_ENMO_ROC %>% 
  select(ID, speed, INTENS_CAT_by_MET, INTENS_CAT_by_ROC) %>% 
  mutate(abs_error_back_ENMO = abs(INTENS_CAT_by_MET - INTENS_CAT_by_ROC)) %>% 
  select(ID, speed, abs_error_back_ENMO)

back_KW_df <- class_error_back_AC %>% 
  left_join(class_error_back_MAD, by = c("ID", "speed")) %>% 
  left_join(class_error_back_ENMO, by = c("ID", "speed")) %>% 
  gather(
    abs_error_back_AC, abs_error_back_MAD, abs_error_back_ENMO,
    key = metric,
    value = absolute_error
  )
back_KW_df$metric <- as_factor(back_KW_df$metric)

## Running test
back_KW <- kruskal.test(absolute_error ~ metric, data = back_KW_df)
back_KW

## Post hoc
back_posthoc <- kruskalmc(absolute_error ~ metric, data = back_KW_df)
back_posthoc

# AC metric (between accelerometers)
## Building data frame
AC_wilcox_df <- class_error_hip_AC %>% 
  left_join(class_error_back_AC, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_AC, abs_error_back_AC,
    key = placement,
    value = absolute_error
  )
AC_wilcox_df$placement <- as_factor(AC_wilcox_df$placement)

## Running test
AC_wilcox <- wilcox.test(absolute_error ~ placement, data = AC_wilcox_df)
AC_wilcox

# MAD metric (between MADcelerometers)
## Building data frame
MAD_wilcox_df <- class_error_hip_MAD %>% 
  left_join(class_error_back_MAD, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_MAD, abs_error_back_MAD,
    key = placement,
    value = absolute_error
  )
MAD_wilcox_df$placement <- as_factor(MAD_wilcox_df$placement)

## Running test
MAD_wilcox <- wilcox.test(absolute_error ~ placement, data = MAD_wilcox_df)
MAD_wilcox

# ENMO metric (between ENMOcelerometers)
## Building data frame
ENMO_wilcox_df <- class_error_hip_ENMO %>% 
  left_join(class_error_back_ENMO, by = c("ID", "speed")) %>% 
  gather(
    abs_error_hip_ENMO, abs_error_back_ENMO,
    key = placement,
    value = absolute_error
  )
ENMO_wilcox_df$placement <- as_factor(ENMO_wilcox_df$placement)

## Running test
ENMO_wilcox <- wilcox.test(absolute_error ~ placement, data = ENMO_wilcox_df)
ENMO_wilcox