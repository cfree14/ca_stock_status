
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2023/processed/"
data_orig <- readRDS(file.path(datadir, "1980_2022_landings_receipts.Rds"))

# Species key
spp_key <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/public/cdfw_keys/processed/CDFW_species_key.Rds")

# Read RAM status time series
ram_orig <- readRDS("data/ramldb/WC_RAM_status_time_series.Rds")


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Summarize
  group_by(year, species_id, comm_name) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Add taxa info
  left_join(spp_key %>% select(spp_code_num, sci_name, level), by=c("species_id"="spp_code_num")) %>% 
  # Fill is missing type
  mutate(level=ifelse(species_id==-1, "group", level)) %>% 
  # Reduce to species
  filter(level=="species")

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


# Stock assessment coverage
################################################################################

# stats
stats_ram <- ram_sum %>% 
  group_by(species, comm_name) %>% 
  summarise(year1=min(year),
            nyears=n()) %>% 
  ungroup() %>% 
  arrange(year1)

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(ram_sum, aes(y=factor(comm_name, levels=stats_ram$comm_name), x=year, fill=pmin(bbmsy,2))) +
  geom_tile() +
  # Reference line
  geom_vline(xintercept=c(1980, 2022), linetype="dashed") +
  # Labels
  labs(x="Year", y="", tag="A") +
  # Legend
  scale_fill_gradient2(name="B/BMSY", low="darkred", high="navy", mid="white", midpoint = 1,
                       breaks=seq(0, 2, 0.5), labels=c("0.0", "0.5", "1.0", "1.5", "≥2.0")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size=unit(0.4, "cm"))
g1

# Plot data
g2 <- ggplot(ram_sum, aes(y=factor(comm_name, levels=stats_ram$comm_name), x=year, fill=pmin(2, ffmsy))) +
  geom_tile() +
  # Reference line
  geom_vline(xintercept=c(1980, 2022), linetype="dashed") +
  # Labels
  labs(x="Year", y="", tag="B") +
  # Legend
  scale_fill_gradient2(name="F/FMSY", low="navy", high="darkred", mid="white", midpoint = 1,
                       breaks=seq(0, 2, 0.5), labels=c("0.0", "0.5", "1.0", "1.5", "≥2.0")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        legend.key.size=unit(0.4, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_ca_ram_coverage.png"), 
       width=6.5, height=4, units="in", dpi=600)


# Build study period data
################################################################################

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


# Boxplots
################################################################################

# Plot BBMSY boxplot 
g1 <- ggplot(ram_sum_extended, aes(x=year, y=bbmsy, group=year, fill=bbmsy_avg)) +
  geom_boxplot(lwd=0.2, outlier.size = 0.6) +
  # Reference line
  geom_hline(yintercept=1) +
  # Legend
  scale_fill_gradientn(name="B/BMSY average", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="Year", y="B/BMSY", tag="A") +
  # Theme
  theme_bw() + base_theme
g1

# Plot FFMSY boxplot 
g2 <- ggplot(ram_sum_extended, aes(x=year, y=ffmsy, group=year, fill=ffmsy_avg)) +
  geom_boxplot(lwd=0.2, outlier.size = 0.6) +
  # Limits
  lims(y=c(0,3)) +
  # Reference line
  geom_hline(yintercept=1) +
  # Legend
  scale_fill_gradientn(name="F/FMSY average", colors=RColorBrewer::brewer.pal(9, "Blues") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="Year", y="F/FMSY", tag="B") +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2)


# Export
ggsave(g, filename=file.path(plotdir, "Fig5_ca_stock_status_boxplots.png"), 
       width=4.5, height=4.5, units="in", dpi=600)


# Categories
################################################################################

# Add categories to darta
data_use <- data %>% 
  # Add categories
  left_join(ram_sum_extended) %>% 
  # Summarize
  group_by(year, bbmsy_status) %>% 
  summarize(nspecies=n_distinct(species_id),
            landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Convert
  mutate(landings_kg=measurements::conv_unit(landings_lbs, "lbs", "kg"),
         landings_mt=landings_kg/1000)


# Landings
g1 <- ggplot(data_use, aes(x=year, y=landings_mt/1000, fill=bbmsy_status)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Landings (1000s of mt)", tag="A") +
  # Legend
  scale_fill_manual(name="B/BMSY status", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + base_theme  +
  theme(legend.position=c(0.7, 0.8),
        legend.key.size = unit(0.3, "cm"))
g1

# Value
g2 <- ggplot(data_use, aes(x=year, y=value_usd/1e6, fill=bbmsy_status)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", tag="B") +
  # Legend
  scale_fill_manual(name="B/BMSY status", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + base_theme  +
  theme(legend.position="none")
g2

# Number of species
g3 <- ggplot(data_use, aes(x=year, y=nspecies, fill=bbmsy_status)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of species", tag="C") +
  # Legend
  scale_fill_manual(name="B/BMSY status", values=RColorBrewer::brewer.pal(4, "RdBu")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g3

# Merge 
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)


# Export
ggsave(g, filename=file.path(plotdir, "Fig6_ca_biomass_status_catgs.png"), 
       width=6.5, height=2.5, units="in", dpi=600)
