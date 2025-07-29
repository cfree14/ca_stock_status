
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
# data_orig <- readRDS(file.path(datadir, "1980_2022_landings_receipts.Rds"))

# Species key
spp_key <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/public/cdfw_keys/processed/CDFW_species_key.Rds")

# Read mgmt key
mgmt_key <- readRDS(file="data/mgmt_plans/spp_mgmt_key.Rds")



# RECFIN
################################################################################

# Get data
recfin_orig <- wcfish::recfin_cte2
freeR::check_names(recfin_orig$sci_name)


# Format data
recfin <- recfin_orig %>% 
  # Filter
  filter(state=="California" & status=="Retained" & type=="fish") %>% 
  # Correct some scientific names
  # All the commented ones are accepted names
  mutate(sci_name=recode(sci_name, 
                         "Semicossyphus pulcher"="Bodianus pulcher",
                         "Beringraja stellulata" = "Caliraja stellulata")) %>% 
  # Add mgmt data
  left_join(mgmt_key %>% select(species, authority, fmp), by=c("sci_name"="species")) %>% 
  # Fill in missing
  mutate(fmp=ifelse(is.na(fmp), "Unmanaged", fmp)) %>% 
  # Summarize
  group_by(year, fmp) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup()
  # 

ggplot(recfin, aes(x=year, y=catch_mt, fill=fmp)) + 
  geom_bar(stat="identity")


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
  # Add mgmt data
  left_join(mgmt_key %>% select(species, authority, fmp), by=c("sci_name"="species")) %>% 
  # Fill in missing
  mutate(fmp=ifelse(is.na(fmp), "Unmanaged", fmp)) %>% 
  # Reduce to species
  filter(level=="species") %>% 
  # Summarize
  group_by(year, fmp) %>% 
  summarize(nspecies=n_distinct(species_id),
            landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Convert
  mutate(landings_kg=measurements::conv_unit(landings_lbs, "lbs", "kg"),
         landings_mt=landings_kg/1000)




# Plot data
################################################################################

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

# FMP colors
fmp_colors <- c("grey40", 
                RColorBrewer::brewer.pal(4, "Blues"),
                RColorBrewer::brewer.pal(6, "Reds")) %>% rev()

# Plot landings
g1 <- ggplot(data, aes(x=year, y=landings_mt/1000, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Landings (1000s mt)", tag="A") +
  # Legend
  scale_fill_manual(name="Taxa type", values=fmp_colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot value
g2 <- ggplot(data, aes(x=year, y=value_usd/1e6, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Ex-vessel revenues (USD millions)", tag="B") +
  # Legend
  scale_fill_manual(name="Taxa type", values=fmp_colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot number of species
g3 <- ggplot(data, aes(x=year, y=nspecies, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Number of species in the catch", tag="C") +
  # Legend
  scale_fill_manual(name="Taxa type", values=fmp_colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="right",
        legend.key.size=unit(0.2, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge data
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.25, 0.25, 0.5))


# Export
ggsave(g, filename=file.path(plotdir, "Fig3_ca_landings_by_mgmt_catg.png"), 
       width=6.5, height=2, units="in", dpi=600)

