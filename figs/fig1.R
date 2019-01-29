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

# Plot grid ---------------------------------------------------------------

BA_plot_grid <- plot_grid(
  hip_AC_BA_plot, back_AC_BA_plot, 
  hip_MAD_BA_plot, back_MAD_BA_plot, 
  hip_ENMO_BA_plot, back_ENMO_BA_plot,
  labels = c("A", "B", "", "", "", ""),
  align = "h", vjust = 1, label_size = 16,
  ncol = 2, nrow = 3
)