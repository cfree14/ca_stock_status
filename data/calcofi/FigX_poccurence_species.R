
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(rerddap)

# Directories
rawdir <- "data/calcofi/raw"
outdir <- "data/calcofi/processed"
plotdir <- "data/calcofi/figures"

# Read data
stats <- read.csv(file=file.path(outdir, "calcofi_species_to_evaluate.csv"), as.is=T)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.8, 0.8),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(y=reorder(comm_name, desc(ptows)), x=ptows, fill=taxa_type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tows", y="",
       title="Species observed in >300 tows over >30 years") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Taxanomic level") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_poccurence_by_species.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

