
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(mblm)
library(zyp)

# Directories
datadir <- "data/calcofi/processed"
outdir <- "data/calcofi/output"
plotdir <- "data/calcofi/figures"

# Export
data_orig <- readRDS(file=file.path(datadir, "calcofi_indices_of_abundance.Rds"))


# Estimate trends
################################################################################

# Species
spp <- sort(unique(data_orig$species))

# Years to look over
nyrs <- 5
max_year <- max(data_orig$year)
min_year <- max_year - nyrs + 1

# For loop
x <- spp[1]
output <- purrr::map_df(spp, function(x){
  
  # Species do
  spp_do <- x
  
  # Subset data
  sdata <- data_orig %>% 
    filter(species==spp_do)
  sdata_use <- sdata %>% 
    filter(year >= min_year)
  
  # Fit robust regression
  model <- MASS ::rlm(index ~ year, data = sdata_use)
  summary(model)
  
  # Extract p-value
  ptest <- parameters::model_parameters(model)
  pvalue <- ptest$p[2]

  # Extract slope, intercept, and p-value
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Calculate recent and longterm averages
  avg_recent <- mean(sdata_use$index)
  avg_longterm <- mean(sdata$index)
  ttest_avg <- t.test(sdata_use$index, sdata$index)
  pvalue_avg <- ttest_avg$p.value

  # Generate line
  df <- tibble(species=spp_do,
               slope=slope,
               intercept=intercept, 
               pvalue=pvalue,
               avg_recent=avg_recent,
               avg_longterm=avg_longterm,
               pvalue_avg=pvalue_avg,
               year=min_year:max_year)
  index_est <- predict(model, df)
  df$index <- index_est
  
  # Plot data
  ggplot(sdata, aes(x=year, y=index)) +
    # Data
    geom_ribbon(mapping=aes(ymin=index_lo, ymax=index_hi), fill="grey90") +
    geom_line() +
    geom_point() +
    geom_hline(yintercept=avg_longterm, linetype="dashed") +
    # Plot fit
    # geom_abline(slope=slope, intercept=intercept) +
    geom_line(data=df, color="red", linewidth=1.3) +
    # Labels
    labs(y="Index of relative abundance", x="Year") +
    scale_x_continuous(breaks=seq(1985, 2025, 5)) +
    # Theme
    theme_bw()
  
  # Return
  df
  
})

# Export
saveRDS(output, file=file.path(datadir, "calcofi_index_trends.Rds"))


