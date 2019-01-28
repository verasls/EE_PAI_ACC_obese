# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Read files --------------------------------------------------------------

LOOCV_hip_AC_model    <- read_csv("~/Dropbox/Projects/EE_PAI_ACC_obese/LOOCV_hip_AC.csv")
LOOCV_hip_MAD_model   <- read_csv("~/Dropbox/Projects/EE_PAI_ACC_obese/LOOCV_hip_MAD.csv")
LOOCV_hip_ENMO_model  <- read_csv("~/Dropbox/Projects/EE_PAI_ACC_obese/LOOCV_hip_ENMO.csv")
LOOCV_back_AC_model   <- read_csv("~/Dropbox/Projects/EE_PAI_ACC_obese/LOOCV_back_AC.csv")
LOOCV_back_MAD_model  <- read_csv("~/Dropbox/Projects/EE_PAI_ACC_obese/LOOCV_back_MAD.csv")
LOOCV_back_ENMO_model <- read_csv("~/Dropbox/Projects/EE_PAI_ACC_obese/LOOCV_back_ENMO.csv")

# Hip AC ------------------------------------------------------------------
hip_AC_BA_plot <- ggplot(data = LOOCV_hip_AC_model) +
  geom_point(mapping = aes(x = ((kcal + kcal_predicted) / 2), y = kcal - kcal_predicted)) +
  geom_hline(yintercept = mean(LOOCV_hip_AC_model$kcal - LOOCV_hip_AC_model$kcal_predicted)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_AC_model$kcal - LOOCV_hip_AC_model$kcal_predicted) +
      1.96 * sd(LOOCV_hip_AC_model$kcal - LOOCV_hip_AC_model$kcal_predicted),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_AC_model$kcal - LOOCV_hip_AC_model$kcal_predicted) -
      1.96 * sd(LOOCV_hip_AC_model$kcal - LOOCV_hip_AC_model$kcal_predicted),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "AC",
    x = "Mean of Measured and Predicted Kilocalories",
    y = "Difference of Measured and Predicted Kilocalories"
  )

# Hip MAD -----------------------------------------------------------------
hip_MAD_BA_plot <- ggplot(data = LOOCV_hip_MAD_model) +
  geom_point(mapping = aes(x = ((kcal + kcal_predicted) / 2), y = kcal - kcal_predicted)) +
  geom_hline(yintercept = mean(LOOCV_hip_MAD_model$kcal - LOOCV_hip_MAD_model$kcal_predicted)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_MAD_model$kcal - LOOCV_hip_MAD_model$kcal_predicted) +
      1.96 * sd(LOOCV_hip_MAD_model$kcal - LOOCV_hip_MAD_model$kcal_predicted),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_MAD_model$kcal - LOOCV_hip_MAD_model$kcal_predicted) -
      1.96 * sd(LOOCV_hip_MAD_model$kcal - LOOCV_hip_MAD_model$kcal_predicted),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "MAD",
    x = "Mean of Measured and Predicted Kilocalories",
    y = "Difference of Measured and Predicted Kilocalories"
  )

# Hip ENMO ----------------------------------------------------------------
hip_ENMO_BA_plot <- ggplot(data = LOOCV_hip_ENMO_model) +
  geom_point(mapping = aes(x = ((kcal + kcal_predicted) / 2), y = kcal - kcal_predicted)) +
  geom_hline(yintercept = mean(LOOCV_hip_ENMO_model$kcal - LOOCV_hip_ENMO_model$kcal_predicted)) +
  geom_hline(
    yintercept = mean(LOOCV_hip_ENMO_model$kcal - LOOCV_hip_ENMO_model$kcal_predicted) +
      1.96 * sd(LOOCV_hip_ENMO_model$kcal - LOOCV_hip_ENMO_model$kcal_predicted),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_hip_ENMO_model$kcal - LOOCV_hip_ENMO_model$kcal_predicted) -
      1.96 * sd(LOOCV_hip_ENMO_model$kcal - LOOCV_hip_ENMO_model$kcal_predicted),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "ENMO",
    x = "Mean of Measured and Predicted Kilocalories",
    y = "Difference of Measured and Predicted Kilocalories"
  )

# Back AC -----------------------------------------------------------------
back_AC_BA_plot <- ggplot(data = LOOCV_back_AC_model) +
  geom_point(mapping = aes(x = ((kcal + kcal_predicted) / 2), y = kcal - kcal_predicted)) +
  geom_hline(yintercept = mean(LOOCV_back_AC_model$kcal - LOOCV_back_AC_model$kcal_predicted)) +
  geom_hline(
    yintercept = mean(LOOCV_back_AC_model$kcal - LOOCV_back_AC_model$kcal_predicted) +
      1.96 * sd(LOOCV_back_AC_model$kcal - LOOCV_back_AC_model$kcal_predicted),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_AC_model$kcal - LOOCV_back_AC_model$kcal_predicted) -
      1.96 * sd(LOOCV_back_AC_model$kcal - LOOCV_back_AC_model$kcal_predicted),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "AC",
    x = "Mean of Measured and Predicted Kilocalories",
    y = "Difference of Measured and Predicted Kilocalories"
  )

# Back MAD ----------------------------------------------------------------
back_MAD_BA_plot <- ggplot(data = LOOCV_back_MAD_model) +
  geom_point(mapping = aes(x = ((kcal + kcal_predicted) / 2), y = kcal - kcal_predicted)) +
  geom_hline(yintercept = mean(LOOCV_back_MAD_model$kcal - LOOCV_back_MAD_model$kcal_predicted)) +
  geom_hline(
    yintercept = mean(LOOCV_back_MAD_model$kcal - LOOCV_back_MAD_model$kcal_predicted) +
      1.96 * sd(LOOCV_back_MAD_model$kcal - LOOCV_back_MAD_model$kcal_predicted),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_MAD_model$kcal - LOOCV_back_MAD_model$kcal_predicted) -
      1.96 * sd(LOOCV_back_MAD_model$kcal - LOOCV_back_MAD_model$kcal_predicted),
    linetype = "dotted"
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "MAD",
    x = "Mean of Measured and Predicted Kilocalories",
    y = "Difference of Measured and Predicted Kilocalories"
  )

# Back ENMO ---------------------------------------------------------------
back_ENMO_BA_plot <- ggplot(data = LOOCV_back_ENMO_model) +
  geom_point(mapping = aes(x = ((kcal + kcal_predicted) / 2), y = kcal - kcal_predicted)) +
  geom_hline(yintercept = mean(LOOCV_back_ENMO_model$kcal - LOOCV_back_ENMO_model$kcal_predicted)) +
  geom_hline(
    yintercept = mean(LOOCV_back_ENMO_model$kcal - LOOCV_back_ENMO_model$kcal_predicted) +
      1.96 * sd(LOOCV_back_ENMO_model$kcal - LOOCV_back_ENMO_model$kcal_predicted),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = mean(LOOCV_back_ENMO_model$kcal - LOOCV_back_ENMO_model$kcal_predicted) -
      1.96 * sd(LOOCV_back_ENMO_model$kcal - LOOCV_back_ENMO_model$kcal_predicted),
    linetype = "dotted") +
  scale_y_continuous(limits = c(-4, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(
    title = "ENMO",
    x = "Mean of Measured and Predicted Kilocalories",
    y = "Difference of Measured and Predicted Kilocalories"
  )

# Plot grid ---------------------------------------------------------------

BA_plot_grid <- plot_grid(
  hip_AC_BA_plot, back_AC_BA_plot, 
  hip_MAD_BA_plot, back_MAD_BA_plot, 
  hip_ENMO_BA_plot, back_ENMO_BA_plot,
  labels = c("A", "B", "", "", "", ""),
  align = "h", vjust = 1, label_size = 16,
  ncol = 2, nrow = 3
)