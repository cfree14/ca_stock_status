
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
  mutate(level=ifelse(species_id==-1, "group", level))

freeR::complete(data)

# Build stats
stats <- data %>% 
  group_by(year, level) %>% 
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
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=8),
                    plot.tag=element_text(size=9),
                    plot.title=element_text(size=9),
                    plot.subtitle = element_text(size=8, face="italic"),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot landings
g1 <- ggplot(stats, aes(x=year, y=landings_mt/1000, fill=level)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Landings\n(1000s mt)", tag="A") +
  # Legend
  scale_fill_discrete(name="Taxa type") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position= c(0.7, 0.8),
        legend.key.size=unit(0.3, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot value
g2 <- ggplot(stats, aes(x=year, y=value_usd/1e6, fill=level)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Ex-vessel revenues\n(USD millions)", tag="B") +
  # Legend
  scale_fill_discrete(name="Taxa type") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot number of species
g3 <- ggplot(stats, aes(x=year, y=nspecies, fill=level)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Number of species\nin the catch", tag="C") +
  # Legend
  scale_fill_discrete(name="Taxa type") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge data
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)


# Export
ggsave(g, filename=file.path(plotdir, "Fig2_ca_landings_by_type.png"), 
       width=6.5, height=2.25, units="in", dpi=600)

