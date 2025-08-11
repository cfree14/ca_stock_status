
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


# Plot
g1 <- ggplot(data, aes(x=avg_ratio_use, y=slope_rel, shape=assessed_yn)) +
  # Add quadrant background shading using geom_rect
  geom_rect(aes(xmin = -Inf, xmax = xmid, ymin = -Inf, ymax = ymid), fill = "red", alpha = 0.4, inherit.aes = FALSE) +  # Bottom-left
  geom_rect(aes(xmin = xmid, xmax = Inf, ymin = -Inf, ymax = ymid), fill = "yellow", alpha = 0.4, inherit.aes = FALSE) +  # Bottom-right
  geom_rect(aes(xmin = -Inf, xmax = xmid, ymin = ymid, ymax = Inf), fill = "orange", alpha = 0.4, inherit.aes = FALSE) +        # Top-left
  geom_rect(aes(xmin = xmid, xmax = Inf, ymin = ymid, ymax = Inf), fill = "lightgreen", alpha = 0.4, inherit.aes = FALSE) +       # Top-right
  # Reference lines
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=1, linetype="dashed") +
  # Data
  geom_point(size=2) +
  # Labels
  labs(x="Species status", 
       y="Species trend",
       tag="A") +
  # Legend
  scale_shape_manual(name="", values=c(16, 1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.15),
        legend.key.size = unit(0.4, "cm"))
g1

g2 <- ggplot(stats, aes(x="", y=prop, fill=catg)) +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x=" ", y="% of species", tag="B") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.6, 0.4))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_score_card.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


