
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
data_orig <- readRDS(file=file.path(datadir, "abundance_index_stats_expanded.Rds"))


# Build data
################################################################################

# Reduce to best dataset
data <- data_orig %>% 
  arrange(sci_name, dataset_rank) %>% 
  group_by(sci_name) %>% 
  slice(1) %>% 
  ungroup()
  

# Calculate proportion in each category
stats <- data %>% 
  count(catg) %>% 
  mutate(prop=n/sum(n))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   plot.tag = element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Define quadrant boundaries
xmid <- 1
ymid <- 0


g <- ggplot(stats, aes(x="", y=prop, fill=catg)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x=" ", y="% of species") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_score_card.png"), 
       width=3.0, height=5.5, units="in", dpi=600)


