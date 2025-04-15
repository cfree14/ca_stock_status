
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/fish_species/miller_lea_1972"

# Read data
data <- read.csv(file.path(datadir, "ca_fish_species_miller_lea_1972.csv"), as.is=T)


# Build data
################################################################################

# Stats
stats <- data %>% 
  count(class, order)

n_distinct(data$species)
n_distinct(data$genus)
n_distinct(data$family)
n_distinct(data$order)
n_distinct(data$class)



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
                   legend.position=c(0.75, 0.9),
                   legend.key.size=unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(x=n, y=reorder(order, desc(n)), fill=class)) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="Number of species", y="") +
  # Legend
  scale_fill_ordinal(name="Class") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(datadir, "FigX_ca_fish_species.png"), 
       width=4.5, height=5.5, units="in", dpi=600)



