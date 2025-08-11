
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/merged"
plotdir <- "figures"

data <- readRDS(file=file.path(datadir, "species_key.Rds")) %>% 
  mutate(age_bin=cut(tmax_yr, breaks=c(0,5,10, 20, 50, 100)),
         length_bin=cut(linf_cm, breaks=c(0, 25, 50, 100, 200)))


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
                   legend.position = c(0.8, 0.7),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g1 <- ggplot(data, aes(x=tmax_yr, fill=age_bin)) +
  geom_histogram(breaks=seq(0,100,5), closed="left") +
  # geom_vline(xintercept = c(5,10,20,50)) +
  # Labels
  labs(x="Maximum age (yr)", y="Number of species") +
  # Legend
  scale_fill_discrete(name="Age bin") +
  # Theme
  theme_bw() + my_theme
g1

g2 <- ggplot(data, aes(x=linf_cm, fill=length_bin)) +
  geom_histogram(breaks=seq(0, 200, 5), closed="left") +
  # geom_vline(xintercept = c(5,10,20,50)) +
  # Labels
  labs(x="Asymptotic length (yr)", y="Number of species") +
  # Legend
  scale_fill_discrete(name="Length bin") +
  # Theme
  theme_bw() + my_theme
g2

g <- gridExtra::grid.arrange(g1, g2, ncol=2)



