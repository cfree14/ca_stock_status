
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(mblm)
library(zyp)

# Directories
datadir <- "data/merged"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "abundance_indices_merged.Rds"))

# Stock key
stock_key <- data_orig %>% 
  select(stock, comm_name, sci_name, dataset) %>% 
  unique()
freeR::which_duplicated(stock_key$stock)


# Estimate trends
################################################################################

# Stocks
stocks <- sort(unique(data_orig$stock))

# Years to look over
nyrs <- 5


# For loop
x <- stocks[1]
output <- purrr::map_df(stocks, function(x){
  
  # Stock do
  stock_do <- x
  
  # Subset stock's data
  sdata <- data_orig %>% 
    filter(stock==stock_do)
  
  # Years
  max_year <- max(sdata$year)
  min_year <- max_year - nyrs + 1
  
  # Subset years to look over
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
  df <- tibble(stock=stock_do,
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
    labs(y="Index of relative abundance", x="Year", title=stock_do) +
    # Theme
    theme_bw()
  
  # Return
  df
  
})


# Add 
################################################################################

# Recent trend data
recent_trends <- output %>% 
  left_join(stock_key, by=c("stock")) %>% 
  select(dataset, stock, comm_name, sci_name, everything())

# Stats
stats <- recent_trends %>% 
  select(dataset:pvalue_avg) %>% 
  unique()
freeR::which_duplicated(stats$stock)


# Export
################################################################################

# Export
saveRDS(recent_trends, file=file.path(datadir, "abundance_index_recent_trends.Rds"))
saveRDS(stats, file=file.path(datadir, "abundance_index_stats.Rds"))




