
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)

# Directories
datadir <- "data/merged"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "abundance_index_stats.Rds"))

# Read StockSMART stats
stocks_orig <- readRDS("data/stock_smart/processed/stock_smart_stocks_use.Rds")

# Get species traits
spp_key <- readRDS(file=file.path(datadir, "species_key.Rds"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Compute stats
  mutate(avg_ratio=avg_recent/avg_longterm,
         # avg_ratio=(avg_recent-avg_longterm) / avg_longterm,
         slope_rel=slope/avg_recent) %>% 
  # Add B/BMSY for assessed stocks
  mutate(stock_match=gsub(" 1| 2", "", stock)) %>% 
  left_join(stocks_orig %>% select(stock, b_bmsy), by=c("stock_match"='stock')) %>% 
  # Classify trend
  mutate(trend=case_when(slope>0 & pvalue < 0.05 ~ "increasing",
                         slope<0 & pvalue < 0.05 ~ "decreasing",
                         T ~ "stable"),
         trend_use=recode(trend,
                          "stable"="increasing/stable",
                          "increasing"="increasing/stable")) %>% 
  # Classify status
  mutate(status=case_when(avg_ratio>1 & pvalue_avg < 0.05 ~ "high",
                          avg_ratio<1 & pvalue_avg < 0.05 ~ "low",
                          T ~ "average"),
         status=case_when(!is.na(b_bmsy) & b_bmsy > 1 ~ "high",
                          !is.na(b_bmsy) & b_bmsy < 1 ~ "low",
                          T ~ status),
         status_use=recode(status,
                           "average"="high/average",
                           "high"="high/average")) %>% 
  # Combo status
  mutate(catg=paste(status_use, trend_use, sep="-"),
         catg=recode_factor(catg, 
                            "low-decreasing" = "Low and decreasing",
                            "low-increasing/stable" = "Low but increasing",
                            "high/average-decreasing" = "High but decreasing",
                            "high/average-increasing/stable" = "High and increasing")) %>% 
  # Add dataset rank
  mutate(dataset_rank=recode(dataset,
                             "StockSMART"="1",
                             "GBTS"="2",
                             "CalCOFI"="3",
                             "RREAS"="4",
                             "CCFRP"="5",
                             "SCUBA"="6") %>% as.numeric(),
         dataset=factor(dataset, levels=c("StockSMART", "GBTS", "CalCOFI", "RREAS", "CCFRP", "SCUBA"))) %>%
  # Add species key
  left_join(spp_key %>% select(sci_name, class, order, family, tl_long, habitat, assessed_yn, managed_yn, tmax_yr, linf_cm), by="sci_name")

# Inspect
freeR::complete(data)
table(data$catg)


# Export
saveRDS(data, file.path(datadir, "abundance_index_stats_expanded.Rds"))



# # Select the average ratio to use (B/BMSY for assessed stocks)
# mutate(avg_ratio_use=ifelse(!is.na(b_bmsy), b_bmsy, avg_ratio)) %>% 
# # Categorize
# mutate(catg=case_when(avg_ratio_use>=1 & slope_rel>=0 ~ "top-right",
#                       avg_ratio_use>=1 & slope_rel<0~ "bottom-right",
#                       avg_ratio_use<1 & slope_rel<0 ~ "bottom-left",
#                       avg_ratio_use<1 & slope_rel>=0 ~ "top-left",
#                        T ~ NA),
#        catg=recode_factor(catg, 
#                          "bottom-left"="Low and decreasing",
#                          "top-left"="Low but increasing",
#                          "bottom-right"="High but decreasing",
#                          "top-right"="High and increasing" )) %>% 
# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

ggplot(data, aes(x=comm_name, y=dataset, fill=catg)) +
  # Data
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  # Theme
  theme_bw() + my_theme



  