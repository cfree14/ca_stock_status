
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


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(stocks, aes(x=b_bmsy, y=f_fmsy, fill=last_data_year)) +
  # Reference lines
  geom_vline(xintercept=0.5, linetype="dotted", color="grey60") +
  geom_vline(xintercept=1, linetype="dashed", color="grey30") +
  geom_hline(yintercept=1, linetype="dashed", color="grey30") +
  # Point
  geom_point(pch=21, size=2) +
  # Label overfished
  # ggrepel::geom_text_repel(data=stocks_label, 
  #                          mapping=aes(x=b_bmsy, y=f_fmsy, label=stock)) +
  geom_text(data=stocks_label, 
           mapping=aes(x=b_bmsy, y=f_fmsy, label=stock),
           hjust=-0.1, size=2.0) +
  # Labels
  labs(x=expression("B / B"["MSY"]), 
       y=expression("F / F"["MSY"]),
       tag="A") +
  # Legend
  scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Axes
  scale_x_continuous(lim=c(0, NA), breaks=c(0:5, 0.5)) +
  scale_y_continuous(lim=c(0, NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.9,  0.8))
g1

# Plot BBMSY boxplot 
g2 <- ggplot(ram_sum_extended, aes(x=year, y=bbmsy, group=year, fill=bbmsy_avg)) +
  geom_boxplot(lwd=0.2, outlier.size = 0.6) +
  # Reference line
  geom_hline(yintercept=1) +
  # Legend
  scale_fill_gradientn(name="Average",
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  # scale_fill_gradient2(name="Average", 
  #                      midpoint = 1,
  #                      low="darkred", high="navy", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) + # title.position="top", title.hjust = 0.5)
  # Labels
  labs(x="Year", y=expression("B / B"["MSY"]), tag="B") +
  # Theme
  theme_bw() + my_theme 
  # theme(legend.position = c(0.1, 0.8),
  #       legend.direction = "horizontal")
g2

# Plot FFMSY boxplot 
g3 <- ggplot(ram_sum_extended, aes(x=year, y=ffmsy, group=year, fill=ffmsy_avg)) +
  geom_boxplot(lwd=0.2, outlier.size = 0.6) +
  # Limits
  lims(y=c(0,3)) +
  # Reference line
  geom_hline(yintercept=1) +
  # Legend
  # scale_fill_gradientn(name="Average", 
  #                      colors=RColorBrewer::brewer.pal(9, "Blues") %>% rev()) +
  scale_fill_gradient2(name="Average", 
                       midpoint = 1,
                       low=RColorBrewer::brewer.pal(9, "Blues")[8], high="darkred", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) + #  title.position="top", title.hjust = 0.5)) +
  # Labels
  labs(x="Year", y=expression("F / F"["MSY"]), tag="C") +
  # Theme
  theme_bw() + my_theme 
  # theme(legend.position = c(0.8, 0.7),
  #       legend.direction = "horizontal")
g3

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               1,3), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, 
                        layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_ca_stock_status.png"), 
       width=6.5, height=3.5, units="in", dpi=600)
 

