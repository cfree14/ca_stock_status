
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(taxize)

# Directories
datadir <- "data/fish_species"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "ca_species_not_in_miller_lea_1972.xlsx"))


# Format data
################################################################################

# Get taxa
taxa <- freeR::taxa(data_orig$species)

# Add taxa
data <- data_orig %>% 
  # Add taxa
  left_join(taxa %>% select(-c(type, species)), by=c("species"="sciname")) %>% 
  # Arrange
  arrange(class, order, family, genus, species, source)

# Look up common names
comm_names <- freeR::fb_comm_name(data$species)

# Add common names
data1 <- data %>% 
  # Add common names
  left_join(comm_names %>% select(-source)) %>% 
  # Fill missing commmon names
  mutate(comm_name=case_when(species=="Bathyraja kincaidii" ~ "Sandpaper skate",
                             species=="Phanerodon vacca" ~ "Pile perch",
                             T ~ comm_name))


