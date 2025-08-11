
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
                         labels=c("< 25 cm", "25-50 cm", "50-100 cm", "> 100 cm")))


# Build variable order
vars <- 

# Build group order 
groups <- c("All", "Trophic level", "Habitat", "Tmax (yr)", "Linf (cm)")

# Build status stats
##################################

# Proportion by order
status <- data %>% 
  group_by(order, status) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(order) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Order status
  mutate(status=recode_factor(status,
                       "low"="Low",
                       "average"="Average",
                       "high"="High"))


# Proportion by trophic level
trends <- data %>% 
  group_by(order, trend) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(order) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Order trend
  mutate(trend=recode_factor(trend,
                              "decreasing"="Decreasing",
                              "stable"="Stable",
                              "increasing"="Increasing"))


# Proportion by trophic level
stats <- data %>% 
  group_by(order, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(order) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup()

# Calculate sample sizes
nstats <- stats %>% 
  group_by(order) %>% 
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
g1 <- ggplot(status, aes(x=prop, y=order, fill=status)) +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats, 
            mapping=aes(label=n, y=order), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="% of species", y="", tag="A", title="Status") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Status", values=colors1) +
  guides(fill = guide_legend(ncol = 2), reverse=T) +
  # Theme
  theme_bw() + my_theme
g1

# Trend
g2 <- ggplot(trends, aes(x=prop, y=order, fill=trend)) +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats, 
            mapping=aes(label=n, y=order), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="% of species", y="", tag="B", title="Trend") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Status", values=colors1) +
  guides(fill = guide_legend(ncol = 2), reverse=T) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Status
g3 <- ggplot(stats, aes(x=prop, y=order, fill=catg)) +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats,
            mapping=aes(label=n, y=order), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="% of species", y="", tag="C", title="Trajectory") +
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
ggsave(g, filename=file.path(plotdir, "Fig6_scores_by_order.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



