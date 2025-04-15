
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)

# Directories
datadir <- "data/calcofi/processed"
outdir <- "data/calcofi/output"
plotdir <- "data/calcofi/figures"

# Read data
data <- readRDS(file=file.path(datadir, "calcofi_indices_of_abundance.Rds"))
trends_orig <- readRDS( file=file.path(datadir, "calcofi_index_trends.Rds"))

# Format data
trends <- trends_orig %>% 
  select(species:pvalue_avg) %>% 
  unique() %>% 
  mutate(avg_ratio=(avg_recent-avg_longterm) / avg_longterm)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
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


# 
g <- ggplot(trends, aes(x=avg_ratio, y=slope/avg_recent)) +
  # Reference lines
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  # Data
  geom_point() +
  # Labels
  labs(x="Species status\n(% difference between recent and longterm average abundance)", 
       y="Species trend\n(Relative change in recent abundance)") +
  # Theme
  theme_bw() + my_theme


# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_score_card.png"), 
       width=5.5, height=5.5, units="in", dpi=600)



