
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
<<<<<<< HEAD
<<<<<<< HEAD
stats <- read.csv(file=file.path(outdir, "calcofi_species_to_evaluate.csv"), as.is=T)


=======
=======
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
data_orig <- readRDS(file=file.path(outdir, "calcofi_fish_larvae_counts.Rds"))
tows_orig <- readRDS(file=file.path(outdir, "calcofi_fish_larvae_tows.Rds"))


# Read data
################################################################################

# Number of tows
ntows_tot <- tows_orig %>% 
  # CALCOFI surveys
  filter(!is.na(survey)) %>% 
  # Remove gear with #/100m3
  filter(tow_type!="Twin winged continuous-flow surface tow") %>% 
  # Focus on last 10 years
  filter(year>=2014) %>%
  # Number
  nrow()

# Stats
range(data_orig$year)
stats <- data_orig %>% 
  # CALCOFI surveys
  filter(!is.na(survey)) %>% 
  # Remove gear with #/100m3
  filter(tow_type!="Twin winged continuous-flow surface tow") %>% 
  # Focus on last 10 years
  filter(year>=2014) %>%
  # Count number of tows with species
  count(taxa_type, sci_name, comm_name) %>% 
  rename(ntows=n) %>% 
  # Add proportion
  mutate(ptows=ntows/ntows_tot) %>% 
  # Species-specific
  filter(taxa_type=="species") %>% 
  # Commonish
  filter(ptows>0.02)

hist(stats$ptows, breaks=seq(0,0.5,0.01))
  

<<<<<<< HEAD
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
=======
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
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
<<<<<<< HEAD
<<<<<<< HEAD
                   legend.position = c(0.8, 0.8),
=======
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
=======
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
<<<<<<< HEAD
<<<<<<< HEAD
g <- ggplot(stats, aes(y=reorder(comm_name, desc(ptows)), x=ptows, fill=taxa_type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tows", y="",
       title="Species observed in >300 tows over >30 years") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Taxanomic level") +
=======
=======
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
g <- ggplot(stats, aes(y=reorder(comm_name, desc(ptows)), x=ptows)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tows", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
<<<<<<< HEAD
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
=======
>>>>>>> 5db34a8ff2c7c061ab12dad1a5385a02f220d8dd
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_poccurence_by_species.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

