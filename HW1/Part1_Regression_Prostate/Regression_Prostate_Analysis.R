# ---- Setup ----
rm(list = ls())

# Install once if needed:
install.packages(c("tidyverse", "GGally", "broom", "lmtest", "car"))

library(tidyverse)  # ggplot2, dplyr, readr, etc.
library(GGally)     # ggpairs
library(broom)      # tidy model outputs
library(lmtest)     # bptest (Breusch–Pagan)
library(car)        # influence plots (optional)

set.seed(210)       # reproducibility if you sample anything