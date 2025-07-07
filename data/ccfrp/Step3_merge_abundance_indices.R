
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)
library(foreach)
library(doParallel)

# Directories
indir <- "data/ccfrp/raw"
datadir <- "data/ccfrp/processed"
plotdir <- "data/ccfrp/figures"
outdir <- "data/ccfrp/output"

# Read species key
spp_key <- readRDS(file.path(datadir, "CCFRP_species_to_evaluate.Rds"))


# Merge data
################################################################################

# Merge
files2merge <- list.files(outdir)
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readRDS(file.path(outdir, x))
})

# Format
data <- data_orig %>% 
  rename(comm_name=species) %>% 
  left_join(spp_key %>% select(comm_name, sci_name)) %>% 
  select(comm_name, sci_name, everything())


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(datadir, "CCFRP_abundance_indices.Rds"))


ggplot(data, aes(x=year, y=index)) +
  facet_wrap(~comm_name, scales="free_y", ncol=7) +
  geom_line() +
  # Labels
  labs(x="Year", y="Index of relative abundance") +
  # Theme
  theme_bw()



