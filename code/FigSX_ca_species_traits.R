
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
data_orig <- readRDS("data/fish_species/processed/ca_finfish_species.Rds") 

# Format data
data <- data_orig %>% 
  # Format TL
  mutate(tl_long=ifelse(is.na(tl_long), "Unknown", tl_long),
         tl_long=factor(tl_long,
                        levels=c("Herbivores/omnivores",
                                 "Primary carnivores",
                                 "Secondary carnivores",
                                 "Tertiary carnivores",
                                 "Quaternary carnivores",
                                 "Unknown"))) %>% 
  # Format habitat
  mutate(habitat=stringr::str_to_sentence(habitat),
         habitat=recode(habitat, "Pelagic"="Anadramous"),
         habitat=factor(habitat,
                        levels=c("Anadramous",
                                 "Pelagic-oceanic",
                                 "Pelagic-neritic",
                                 "Benthopelagic",
                                 "Bathypelagic",
                                 "Reef-associated",
                                 "Demersal",
                                 "Bathydemersal")))

# Build data
################################################################################

# Confirm unique
freeR::which_duplicated(data$species)
freeR::which_duplicated(data$comm_name)

# Stats for manuscript
n_distinct(data$species)
n_distinct(data$genus)
n_distinct(data$family)
n_distinct(data$order)
n_distinct(data$class)

# Orders
orders <- data %>% 
  count(order)

# Habitats
habitats <- data %>% 
  count(habitat)

# Trophic levels
tls <- data %>% 
  count(tl_long)

# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))



g1 <- ggplot(orders, aes(x=n, y=reorder(order, desc(n)))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of species", y="", tag="A") +
  # Theme
  theme_bw() + base_theme
g1

g2 <- ggplot(habitats, aes(x=n, y=habitat)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of species", y="", tag="B") +
  # Theme
  theme_bw() + base_theme
g2

g3 <- ggplot(tls, aes(x=n, y=tl_long)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of species", y="", tag="C") +
  # Theme
  theme_bw() + base_theme
g3


g4 <- ggplot(data, aes(x=tmax_yr)) +
  geom_histogram(binwidth = 5) +
  # Labels
  labs(x="Maximum age (yr)", y="Number of species", tag="D") +
  # Theme
  theme_bw() + base_theme
g4

g5 <- ggplot(data, aes(x=linf_cm)) +
  geom_histogram(binwidth = 20) +
  # Labels
  labs(x="Asymptotic length (cm)", y="Number of species", tag="E") +
  # Theme
  theme_bw() + base_theme
g5

# Merge
layout_matrix <- matrix(data=c(1,2, 
                               1,3,
                               1,4, 
                               1,5), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, 
                             layout_matrix=layout_matrix,
                             widths=c(0.55, 0.45))

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_ca_species_traits.png"), 
       width=6.5, height=6.5, units="in", dpi=600)
