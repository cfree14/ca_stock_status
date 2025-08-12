
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
stocks_orig <- readRDS("data/stock_smart/processed/stock_smart_stocks_use.Rds")

# Read RAM status time series
ram_orig <- readRDS("data/ramldb/WC_RAM_status_time_series.Rds")


# Plot data
################################################################################

# StockSMART
##############################

# Build data
stocks <- stocks_orig %>% 
  # Simplify
  select(stock_id, stock, fmp, comm_name, sci_name, area, last_data_year, 
         f_year:msy_unit) %>% 
  # Reduce
  filter(!is.na(b_bmsy) & !is.na(f_fmsy)) %>% 
  # Remove old assessments
  filter(!stock %in% c("Cabezon - California", "Black rockfish - Southern Pacific Coast")) # Cabezon gets split

# Stocks 2 label
stocks_label <- stocks %>% 
  filter(b_bmsy<=0.5 | f_fmsy > 1)

freeR::which_duplicated(stocks$comm_name)

# Manuscript stats
nrow(stocks)
sum(stocks$b_bmsy>=1 & stocks$f_fmsy<=1)
sum(stocks$b_bmsy>=0.5 & stocks$f_fmsy<=1)


# RAM
##############################

# Format RAM data
ram <- ram_orig %>% 
  # Reduce to species with catch
  filter(species %in% data$sci_name) %>% 
  # Eliminate stocks outside of california
  filter(!area %in% c("Oregon Coast", "Washington", "Central Western Pacific Ocean", "Western Pacific",
                      "Northern Pacific Coast"))

# RAM stock key
stock_key <- ram %>% 
  select(stockid:species) %>% 
  unique()

# Summarize RAM data
ram_sum <- ram %>% 
  group_by(species, comm_name, year) %>% 
  summarize(nstocks=n_distinct(stockid),
            stockids=paste(unique(stockid), collapse=","),
            bbmsy=mean(bbmsy, na.rm=T), 
            ffmsy=mean(ffmsy, na.rm=T)) %>% 
  ungroup()

# Build data
ram_sum_extended <- expand.grid(comm_name=unique(ram_sum$comm_name),
                                year=1980:2022) %>% 
  arrange(comm_name, year) %>% 
  left_join(ram_sum %>% select(comm_name, year, bbmsy, ffmsy)) %>% 
  # Fill
  mutate(bbmsy_type=ifelse(is.na(bbmsy), "Extended", "Provided"),
         ffmsy_type=ifelse(is.na(ffmsy), "Extended", "Provided")) %>% 
  group_by(comm_name) %>% 
  fill(bbmsy:ffmsy, .direction="updown") %>% 
  ungroup() %>% 
  # Categorize
  mutate(bbmsy_status=cut(bbmsy, breaks=c(0, 0.5, 1, 1.5, 999), labels=c("< 0.5", "0.5-1.0", "1.0-1.5", ">1.5")),
         ffmsy_status=cut(ffmsy, breaks=c(0, 0.5, 1, 1.5, 999), labels=c("< 0.5", "0.5-1.0", "1.0-1.5", ">1.5"))) %>% 
  # Add median values
  group_by(year) %>% 
  mutate(bbmsy_avg=mean(bbmsy, na.rm=T),
         ffmsy_avg=median(ffmsy, na.rm=T)) %>% # NOT AVERGE!!!!! CHANGE WHEN FIXING CRAZY THRESHER RESULTS
  ungroup()

ram_sum_extended_simp <- ram_sum_extended %>% 
  group_by(year) %>% 
  summarize(bbmsy_avg=mean(bbmsy, na.rm=T),
            bbmsy_lo=quantile(bbmsy, prob=0.025, na.rm=T),
            bbmsy_hi=quantile(bbmsy, prob=0.975, na.rm=T)) %>% 
  ungroup()

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=9),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=9),
                   legend.title=element_text(size=10),
                   strip.text=element_text(size=9),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot BBMSY boxplot 
g2 <- ggplot(ram_sum_extended, aes(x=year, y=bbmsy, group=year, fill=bbmsy_avg)) +
  geom_boxplot(lwd=0.2, outlier.size = 0.6, outlier.color = NA) +
  # Reference line
  geom_hline(yintercept=1, linewidth=0.3) +
  geom_hline(yintercept=0.5, linewidth=0.3, color="red", linetype="dashed") +
  # Limits
  lims(y=c(0,4.5)) +
  scale_x_continuous(breaks=seq(1980,2020, 5), lim=c(1979.5, 2035)) +
  # Labels
  annotate(geom="text", x=2024, y=1.18, hjust=0, size=2.1,
           label="Above target abundance") +
  annotate(geom="text", x=2024, y=0.82, hjust=0, size=2.1,
           label="Below target abundance") +
  annotate(geom="text", x=2024, y=0.32, hjust=0, size=2.1,
           label="Overfished", color="red") +
  # Legend
  scale_fill_gradientn(name="Average",
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) + # title.position="top", title.hjust = 0.5)
  # Labels
  labs(x="Year", y="Abundance relative\nto target abundance", tag="",
       title="Status of 45 important fishery species over time") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g2

# Export
ggsave(g2, filename=file.path(plotdir, "leaflet_trend.png"), 
       width=5, height=2.75, units="in", dpi=600)


# Plot BBMSY boxplot 
g2 <- ggplot(ram_sum_extended_simp, aes(x=year, y=bbmsy_avg)) +
  # geom_ribbon(mapping=aes(ymin=bbmsy_lo, ymax=bbmsy_hi), fill="grey80") +
  geom_line(linewidth=1, color="blue") +
  # Reference line
  geom_hline(yintercept=1, linewidth=0.3) +
  geom_hline(yintercept=0.5, linewidth=0.3, color="red", linetype="dashed") +
  # Limits
  lims(y=c(0,2)) +
  scale_x_continuous(breaks=seq(1980,2020, 5), lim=c(1979.5, NA)) +
  # Labels
  annotate(geom="text", x=2010, y=1.08, hjust=0, size=2.1,
           label="Above target abundance") +
  annotate(geom="text", x=2010, y=0.92, hjust=0, size=2.1,
           label="Below target abundance") +
  annotate(geom="text", x=2010, y=0.42, hjust=0, size=2.1,
           label="Overfished", color="red") +
  # Legend
  scale_fill_gradientn(name="Average",
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) + # title.position="top", title.hjust = 0.5)
  # Labels
  labs(x="Year", y="Abundance relative\nto target abundance", tag="",
       title="Status of 45 important fishery species over time") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g2

# Export
ggsave(g2, filename=file.path(plotdir, "leaflet_trend_line.png"), 
       width=5, height=2.75, units="in", dpi=600)
 

