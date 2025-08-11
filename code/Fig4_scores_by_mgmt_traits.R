
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
  # Add genus
  mutate(genus=stringr::word(sci_name, 1,1))

table(data$dataset)
n_distinct(data$sci_name)
n_distinct(data$order)
n_distinct(data$family)
n_distinct(data$genus)

# Build variable order
vars <- c( "All", 
           "Not harvested", "Harvested",
           "Not managed", "Managed",
           "Not assessed", "Assessed")

# Build group order 
groups <- c("All", "Harvested?", "Managed?", "Assessed?")

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

# Proportion by harvested
status_harvested <- data %>% 
  group_by(harvested_yn, status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(harvested_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Harvested?") %>% 
  rename(variable=harvested_yn)

# Proportion by managed
status_managed <- data %>% 
  group_by(managed_yn, status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(managed_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Managed?") %>% 
  rename(variable=managed_yn)

# Proportion by assessed
status_assessed <- data %>% 
  group_by(assessed_yn, status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(assessed_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Assessed?") %>% 
  rename(variable=assessed_yn)

# Merge stats
status <- bind_rows(status_all, status_harvested, status_managed, status_assessed) %>% 
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

# Proportion by harvested
trend_harvested <- data %>% 
  group_by(harvested_yn, trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(harvested_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Harvested?") %>% 
  rename(variable=harvested_yn)

# Proportion by managed
trend_managed <- data %>% 
  group_by(managed_yn, trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(managed_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Managed?") %>% 
  rename(variable=managed_yn)

# Proportion by assessed
trend_assessed <- data %>% 
  group_by(assessed_yn, trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(assessed_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Assessed?") %>% 
  rename(variable=assessed_yn)

# Merge stats
trends <- bind_rows(trend_all, trend_harvested, trend_managed, trend_assessed) %>% 
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

# Proportion by harvested
stats_harvested <- data %>% 
  group_by(harvested_yn, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(harvested_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Harvested?") %>% 
  rename(variable=harvested_yn)

# Proportion by managed
stats_managed <- data %>% 
  group_by(managed_yn, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(managed_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Managed?") %>% 
  rename(variable=managed_yn)

# Proportion by assessed
stats_assessed <- data %>% 
  group_by(assessed_yn, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(assessed_yn) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Assessed?") %>% 
  rename(variable=assessed_yn)
          
# Merge stats
stats <- bind_rows(stats_all, stats_harvested, stats_managed, stats_assessed) %>% 
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
ggsave(g, filename=file.path(plotdir, "Fig4_scores_by_mgmt_trait.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



