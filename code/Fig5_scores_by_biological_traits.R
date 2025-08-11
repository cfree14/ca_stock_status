
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
  ungroup() %>% 
  # Format habitat
  mutate(habitat=stringr::str_to_sentence(habitat)) %>% 
  # Add age/length groups
  mutate(tmax_yr_bin=cut(tmax_yr, 
                         breaks=c(0, 5, 10, 20, 50, 100), 
                         labels=c("< 5 yr", "5-10 yr", "10-20 yr", "20-50 yr", "> 50 yr")),
         linf_cm_bin=cut(linf_cm, 
                         breaks=c(0, 25, 50, 100, 200), 
                         labels=c("< 25 cm", "25-50 cm", "50-100 cm", "> 100 cm"))) %>% 
  # Recode anadramous salmon
  mutate(habitat=case_when(comm_name=="Coho salmon" ~ "Anadramous",
                           T ~ habitat))


# Build variable order
vars <- c( "All", 
           "Herbivores/omnivores","Primary carnivores","Secondary carnivores", "Tertiary carnivores", "Unknown",  
           "Bathydemersal", "Demersal", "Reef-associated", "Bathypelagic", "Benthopelagic", "Pelagic-neritic", "Pelagic-oceanic", "Anadramous",
           "< 5 yr", "5-10 yr", "10-20 yr", "20-50 yr", "> 50 yr",
           "< 25 cm", "25-50 cm", "50-100 cm", "> 100 cm")

# Build group order 
groups <- c("All", "Trophic level", "Habitat", "Tmax (yr)", "Linf (cm)")

# Build status stats
##################################

# Proportion
status_all <- data %>% 
  group_by(status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(variable="All", 
         group="All")

# Proportion by trophic level
status_tl <- data %>% 
  group_by(tl_long, status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(tl_long) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Trophic level") %>% 
  rename(variable=tl_long)

# Proportion by habitat
status_habitat <- data %>% 
  group_by(habitat, status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(habitat) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Habitat") %>% 
  rename(variable=habitat)

# Proportion by max age
status_tmax <- data %>%
  group_by(tmax_yr_bin, status) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(tmax_yr_bin) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  mutate(group="Tmax (yr)") %>%
  rename(variable=tmax_yr_bin)

# Proportion by length
status_linf <- data %>%
  group_by(linf_cm_bin, status) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(linf_cm_bin) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  mutate(group="Linf (cm)") %>%
  rename(variable=linf_cm_bin)

# Merge stats
status <- bind_rows(status_all, status_tl, status_habitat, status_tmax, status_linf) %>% 
  # Order groups
  mutate(group=factor(group, levels=groups)) %>% 
  # Order variables
  mutate(variable=factor(variable, levels=vars)) %>% 
  # Order status
  mutate(status=recode_factor(status,
                       "low"="Low",
                       "average"="Average",
                       "high"="High"))

# Build trend stats
##################################

# Proportion
trend_all <- data %>% 
  group_by(trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(variable="All", 
         group="All")

# Proportion by trophic level
trend_tl <- data %>% 
  group_by(tl_long, trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(tl_long) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Trophic level") %>% 
  rename(variable=tl_long)

# Proportion by habitat
trend_habitat <- data %>% 
  group_by(habitat, trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(habitat) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Habitat") %>% 
  rename(variable=habitat)

# Proportion by max age
trend_tmax <- data %>%
  group_by(tmax_yr_bin, trend) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(tmax_yr_bin) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  mutate(group="Tmax (yr)") %>%
  rename(variable=tmax_yr_bin)

# Proportion by length
trend_linf <- data %>%
  group_by(linf_cm_bin, trend) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(linf_cm_bin) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  mutate(group="Linf (cm)") %>%
  rename(variable=linf_cm_bin)

# Merge stats
trends <- bind_rows(trend_all, trend_tl, trend_habitat, trend_tmax, trend_linf) %>% 
  # Order groups
  mutate(group=factor(group, levels=groups)) %>% 
  # Order variables
  mutate(variable=factor(variable, levels=vars)) %>% 
  # Order trend
  mutate(trend=recode_factor(trend,
                              "decreasing"="Decreasing",
                              "stable"="Stable",
                              "increasing"="Increasing"))

# Build combo stats
##################################

# Proportion
stats_all <- data %>% 
  group_by(catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(prop=n/sum(n)) %>% 
  mutate(variable="All", 
         group="All")

# Proportion by trophic level
stats_tl <- data %>% 
  group_by(tl_long, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(tl_long) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Trophic level") %>% 
  rename(variable=tl_long)

# Proportion by habitat
stats_habitat <- data %>% 
  group_by(habitat, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(habitat) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Habitat") %>% 
  rename(variable=habitat)

# Proportion by max age
stats_tmax <- data %>%
  group_by(tmax_yr_bin, catg) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(tmax_yr_bin) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  mutate(group="Tmax (yr)") %>%
  rename(variable=tmax_yr_bin)

# Proportion by length
stats_linf <- data %>%
  group_by(linf_cm_bin, catg) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(linf_cm_bin) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  mutate(group="Linf (cm)") %>%
  rename(variable=linf_cm_bin)
          
# Merge stats
stats <- bind_rows(stats_all, stats_tl, stats_habitat, stats_tmax, stats_linf) %>% 
  # Order groups
  mutate(group=factor(group, levels=groups)) %>% 
  # Order variables
  mutate(variable=factor(variable, levels=vars))

# Calculate sample sizes
nstats <- stats %>% 
  group_by(group, variable) %>% 
  summarize(n=sum(n)) %>% 
  ungroup()


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   strip.text=element_text(size=6),
                   legend.text=element_text(size=6),
                   legend.title=element_blank(), #element_text(size=7),
                   plot.title=element_text(size=8),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   # Legend
                   legend.position = "bottom",
                   legend.key.size=unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Colors
colors1 <- RColorBrewer::brewer.pal(3, "RdBu")

# Status
g1 <- ggplot(status %>% filter(group!="Order"), aes(x=prop, y=variable, fill=status)) +
  # Facet
  facet_grid(group~., space="free_y", scales="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats %>% filter(group!="Order"), 
            mapping=aes(label=n, y=variable), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="Percent of species", y="", tag="A", title="Status") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Status", values=colors1) +
  guides(fill = guide_legend(ncol = 2), reverse=T) +
  # Theme
  theme_bw() + my_theme
g1

# Trend
g2 <- ggplot(trends %>% filter(group!="Order"), aes(x=prop, y=variable, fill=trend)) +
  # Facet
  facet_grid(group~., space="free_y", scales="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats %>% filter(group!="Order"), 
            mapping=aes(label=n, y=variable), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="Percent of species", y="", tag="B", title="Trend") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Status", values=colors1) +
  guides(fill = guide_legend(ncol = 2), reverse=T) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Status
g3 <- ggplot(stats %>% filter(group!="Order"), aes(x=prop, y=variable, fill=catg)) +
  # Facet
  facet_grid(group~., space="free_y", scales="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats %>% filter(group!="Order"), 
            mapping=aes(label=n, y=variable), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="Percent of species", y="", tag="C", title="Trajectory") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  guides(fill = guide_legend(ncol = 2, reverse=T)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_scores_by_biological_traits.png"), 
       width=6.5, height=5.0, units="in", dpi=600)



