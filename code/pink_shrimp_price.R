

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(priceR)
library(tidyverse)

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2023/processed/1980_2022_landings_receipts.Rds")


# Build data
################################################################################

# Subset data
data <- data_orig %>% 
  # Pink shrimp
  filter(comm_name=="Pacific pink shrimp") %>% 
  filter(price_usd>0 & !is.na(price_usd)) %>% 
  # Adust for inflation
  mutate(price_usd2020=priceR::adjust_for_inflation(price=price_usd, 
                                                    from_date=date, 
                                                    to_date = ymd("2020-01-01"), 
                                                    country="United States")) %>% 
  # Summarize
  group_by(year) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            price_usd2020=mean(price_usd2020)) %>% 
  ungroup() %>% 
  # Rule-of-three
  filter(nvessels>=3)

# Plot data
ggplot(data, aes(x=year, y=price_usd2020)) +
  geom_line() + 
  geom_point() +
  # Labels
  labs(x="Year", y="Price (2020USD/lb)") +
  scale_y_continuous(lim=c(0,NA)) +
  # Theme
  theme_bw()


