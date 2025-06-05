
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(taxize)

# Directories
plotdir <- "figures"

# Read data
data <- readRDS("data/stock_smart/processed/stock_smart_data_use.Rds")

