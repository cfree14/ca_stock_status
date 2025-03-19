
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



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.background = element_blank(),
                   panel.border = element_blank(),
                   strip.text=element_text(size=5.5, margin = margin(t = 1, r = 1, b = 1, l = 1)),
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

# Plot data
g <- ggplot(data, aes(x=year, y=index/1e6)) +
  facet_wrap(~species, scales="free_y", ncol=8, labeller = label_wrap_gen(width=10)) +
  # CI
  geom_ribbon(mapping=aes(ymin=index_lo/1e6, ymax=index_hi/1e6), fill="grey80") +
  # Median
  geom_line(linewidth=0.3) +
  # Labels
  labs(x="Year", y="Index of abundance") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y = element_blank())
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_abundance_indices.png"), 
       width=6.5, height=5.5, units="in", dpi=600)




# Plot data with robust regression results
################################################################################

# Format trends
trends <- trends_orig %>% 
  # Mark significance and direction
  mutate(dir=ifelse(slope<0, "decreasing", "increasing"), 
         sig_yn=ifelse(pvalue<0.05, "yes", "no")) %>% 
  # Create category
  mutate(catg=paste(sig_yn, dir, sep="-"),
         catg=recode_factor(catg, 
                            "yes-increasing"="Increase",
                            "no-increasing"="Increase (non-sig)",
                            "no-decreasing"="Decrease (non-sig)",
                            "yes-decreasing"="Decrease")) 

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.background = element_blank(),
                   panel.border = element_blank(),
                   strip.text=element_text(size=5.5, margin = margin(t = 1, r = 1, b = 1, l = 1)),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   # Legend
                   legend.position = "top",
                   legend.margin = margin(t=-1, b=-2, r=0, l=0),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=index)) +
  facet_wrap(~species, scales="free_y", ncol=8, labeller = label_wrap_gen(width=10)) +
  # CI
  geom_ribbon(mapping=aes(ymin=index_lo, ymax=index_hi), fill="grey80") +
  # Median
  geom_line(linewidth=0.3) +
  # Trends
  geom_line(data=trends, mapping=aes(color=catg), linewidth=1.2) +
  # Labels
  labs(x="Year", y="Index of abundance") +
  # Legend
  scale_color_manual(name="Trend", values=RColorBrewer::brewer.pal(4, "RdBu") %>% rev(),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y = element_blank())
#g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_abundance_indices_w_trends.png"), 
       width=6.5, height=6, units="in", dpi=600)

