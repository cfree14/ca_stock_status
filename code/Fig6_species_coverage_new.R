
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "output"

# Read list of all species in California
spp_all <- readRDS("data/fish_species/processed/ca_finfish_species.Rds") 

# Read 
scores <- readRDS("data/merged/abundance_index_stats_expanded.Rds") %>% 
  arrange(sci_name, dataset_rank) %>% 
  group_by(sci_name) %>% 
  slice(1) %>% 
  ungroup()


# Build list of species that have been OBSERVED in monitoring programs
################################################################################

# Read lists of species
ccfrp_spp <- read.csv("data/ccfrp/processed/ccfrp_species_all.csv")
calcofi_spp <- read.csv("data/calcofi/processed/calcofi_species_key.csv")
scuba_spp <- read.csv("data/kelp_scuba/processed/scuba_species_with_data.csv")
rreas_spp <- readRDS("data/rreas/processed/RREAS_species_key.Rds")
gbts_spp <- read.csv("data/gbts/processed/gbts_species_key_but_needs_much_work.csv")

# Observed species
spp_obs <- c(ccfrp_spp$sci_name,
             calcofi_spp$sci_name,
             scuba_spp$sci_name,
             rreas_spp$sci_name,
             gbts_spp$sci_name) %>% unique() %>% sort()


# Build data
################################################################################

# Number of species by order overall
stats_all <- spp_all %>% 
  group_by(order) %>% 
  summarize(nspp_tot=n_distinct(species)) %>% 
  ungroup() %>% 
  arrange(desc(nspp_tot))

# Build data
data <- spp_all %>% 
  # Mark which have survey data
  mutate(status = ifelse(species %in% spp_obs, "Insufficient data", "No data")) %>% 
  # Add dataset
  left_join(scores %>% select(sci_name, dataset), by=c("species"="sci_name")) %>%
  mutate(dataset=as.character(dataset)) %>% 
  # Record status
  mutate(status=ifelse(!is.na(dataset), dataset, status))

# Count number observed in surveys
sum(data$status!="No data")

# Compute stats
stats <- data %>% 
  # Count
  count(order, status) %>% 
  # Calculate prop
  group_by(order) %>% 
  mutate(prop=n/sum(n)) %>% 
  # Order source
  mutate(status=factor(status, levels=c("StockSMART",
                                         "GBTS",
                                         "CalCOFI",
                                         "RREAS",
                                         "CCFRP", 
                                         "SCUBA",
                                         "Insufficient data", 
                                         "No data")))
   
surveys <- c("StockSMART", "GBTS", "CalCOFI", "RREAS", "CCFRP", "SCUBA", "Some data", "No data")
survey_colors <- c( "#A65628", "#FF7F00", "#377EB8", "#E41A1C", "#4DAF4A", "#984EA3", "grey60", "grey90")                 

# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(stats, aes(y=factor(order, stats_all$order), x=n, fill=status)) +
  geom_bar(stat="identity",  position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Number of species", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Index source", values=survey_colors ) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.7, 0.8))
g1

g2 <- ggplot(stats, aes(x=prop, y=factor(order, stats_all$order), fill=status)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  geom_vline(xintercept=0.5, linetype="dashed", linewidth=0.2) +
  geom_text(data=stats_all,
            mapping=aes(label=nspp_tot, y=order),
            hjust=0, x=1.01, inherit.aes = F,
            color="grey30", size=2.4) +
  # Labels
  labs(x="Percent of species", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format(), lim=c(0,1.1),
                     breaks=seq(0,1,0.25)) +
  # Legend
  scale_fill_manual(name="Source", values=survey_colors ) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.65, 0.35))

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_species_coverage_new.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

