
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

# Merge data
################################################################################

files2merge <- list.files(outdir)
data <- purrr::map_df(files2merge, function(x){
  df <- readRDS(file.path(outdir, x))
})


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(datadir, "CCFRP_abundance_indices.Rds"))


ggplot(data, aes(x=year, y=index)) +
  facet_wrap(~species, scales="free_y", ncol=7) +
  geom_line() +
  # Labels
  labs(x="Year", y="Index of relative abundance") +
  # Theme
  theme_bw()



