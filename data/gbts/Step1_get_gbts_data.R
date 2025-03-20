
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(nwfscSurvey)

# Directories


# Details
#https://github.com/pfmc-assessments/nwfscSurvey
# https://github.com/pfmc-assessments/nwfscSurvey/tree/main

?get_species_info()
?pull_spp

spp <- nwfscSurvey::pull_spp()
spp1 <- PullSpp
spp2 <- PullSpp.fn()

?PullSpp.fn
?PullSpp
