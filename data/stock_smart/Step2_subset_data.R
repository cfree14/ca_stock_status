
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(mblm)
library(zyp)

# Directories
indir <- "data/stock_smart/raw"
outdir <- "data/stock_smart/processed"
plotdir <- "data/stock_smart/figures"

# Read data
stocks <- readRDS(file=file.path(outdir, "stock_smart_stocks.Rds"))
data <- readRDS(file=file.path(outdir, "stock_smart_time_series.Rds"))

# MAJOR TO DO
# Areas, common names, and scientific names missing from STOCK look up


# Subset data
################################################################################

# Areas
areas <- freeR::uniq(stocks$area)
areas_ca <- areas[grepl("California", areas)]
areas_ca_plus <- c(areas_ca, "Central Pacific Coast", "Pacific Coast", 
                   "Point Conception / Oregon Border", "South of Point Conception",
                   "Southern Pacific Coast")

# CA stocks
stocks_ca <- stocks %>% 
  # Reduce to CA stocks
  filter(area %in% areas_ca_plus) %>% 
  # Fill missing common names
  mutate(comm_name=case_when(stock=="Vermilion rockfish and Sunset rockfish Complex - Northern California" ~ "Vermilion rockfish, Sunset rockfish",
                             stock=="Vermilion rockfish and Sunset rockfish Complex - Southern California" ~ "Vermilion rockfish, Sunset rockfish",
                             stock=="Northern California Gopher / Black-and-Yellow Rockfish Complex" ~ "Gopher rockfish, Black-and-yellow rockfish",
                             T ~ comm_name)) %>% 
  # Fill missing scientific names
  mutate(sci_name=case_when(comm_name=="Vermilion rockfish, Sunset rockfish" ~ "Sebastes miniatus, Sebastes crocotulus", 
                            comm_name=="Gopher rockfish, Black-and-yellow rockfish" ~ "Sebastes carnatus, Sebastes chrysomelas",
                            T ~ sci_name))


# Subset data
data_ca <- data %>% 
  # Stocks of interest
  filter(stock %in% stocks_ca$stock) %>% 
  # Abundance data
  filter(param_catg=="Abundance" & !is.na(value)) %>% 
  # Scale abundance data
  group_by(stock) %>% 
  mutate(value_scaled=value/max(value)) %>% 
  ungroup() %>% 
  # Add taxa info
  left_join(stocks_ca %>% select(stock, comm_name, sci_name), by="stock") %>% 
  # Arrange
  select(stock, comm_name, sci_name, everything())

# Stats
stats <- data_ca %>% 
  group_by(stock) %>% 
  summarize(year=min(year)) %>% 
  ungroup() %>% 
  arrange(year)

# Species evaluated
species <- stocks_ca %>% 
  count(comm_name, sci_name) %>% 
  rename(nstocks=n) %>% 
  # TEMPORARY - CAN REMOVE AFTER FIXING STOCK TABLE
  filter(!is.na(comm_name))

# Export
saveRDS(species, file=file.path(outdir, "stock_smart_species_to_evaluate.Rds"))
saveRDS(stocks_ca, file=file.path(outdir, "stock_smart_stocks_use.Rds"))
saveRDS(data_ca, file=file.path(outdir, "stock_smart_data_use.Rds"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Visualize
g <- ggplot(data_ca, aes(x=year, y=factor(stock, stats$stock), fill=value_scaled)) + 
  geom_tile() +
  # Vertical line
  geom_vline(xintercept=2025, linetype="dashed") +
  # Legend
  scale_fill_gradientn(name="% of max abundance", 
                       labels=scales::percent_format(),
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=seq(1870, 2020, 10)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.25, 0.85))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_assessment_time_series.png"), 
       width=6.5, height=5.5, units="in", dpi=600)







