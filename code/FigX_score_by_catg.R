
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
  ungroup()


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

# Proportion by TL
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
  rename(variable=habitat) %>% 
  mutate(variable=stringr::str_to_sentence(variable))

# Proportion by order
stats_order <- data %>% 
  group_by(order, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(order) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Order") %>% 
  rename(variable=order)

stats_order_order <- stats_order %>% 
  group_by(variable) %>% 
  summarize(prop=sum(prop[catg %in% c("High and increasing", "High but decreasing")])) %>% 
  ungroup() %>% 
  arrange(desc(prop))

# Build variable order
vars <- c( "Not managed", "Managed",
           "Not assessed", "Assessed",
          "Herbivores/omnivores","Primary carnivores","Secondary carnivores", "Tertiary carnivores", "Unknown",  
          "Bathydemersal", "Demersal", "Reef-associated", "Bathypelagic", "Benthopelagic", "Pelagic-neritic", "Pelagic-oceanic", "Pelagic",
          "Aulopiformes", 
          "Heterodontiformes",   
          "Myxiniformes",     
          "Carcharhiniformes",
          "Batrachoidiformes",   
          "Myctophiformes",     
          "Rajiformes",      
          "Stomiiformes",         
          "Scorpaeniformes",   
          "Pleuronectiformes",   
          "Squaliformes",      
          "Osmeriformes",       
          "Perciformes",       
          "Clupeiformes",         
          "Gadiformes",
          "Gasterosteiformes",   
          "Myliobatiformes",  
          "Stephanoberyciformes",
          "Salmoniformes",
          "Anguilliformes",    
          "Chimaeriformes")
          
# Merge stats
stats <- bind_rows(stats_managed, stats_assessed, stats_tl, stats_habitat, stats_order) %>% 
  # Order groups
  mutate(group=factor(group, levels=c("Managed?", "Assessed?", "Trophic level", "Habitat", "Order"))) %>% 
  # Order variables
  mutate(variable=factor(variable, levels=vars ))

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
                   legend.title=element_text(size=7),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   # Legend
                   legend.key.size=unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


g1 <- ggplot(stats %>% filter(group!="Order"), aes(x=prop, y=variable, fill=catg)) +
  # Facet
  facet_grid(group~., space="free_y", scales="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats %>% filter(group!="Order"), 
            mapping=aes(label=n, y=variable), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="% of species", y="", tag="A") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g1

g2 <- ggplot(stats %>% filter(group=="Order"), aes(x=prop, y=variable, fill=catg)) +
  # Facet
  facet_grid(group~., space="free_y", scales="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  geom_text(data=nstats %>% filter(group=="Order"), 
            mapping=aes(label=n, y=variable), inherit.aes = F,
            x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="% of species", y="", tag="B") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  # Theme
  theme_bw() + my_theme 
g2

g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_score_by_catg.png"), 
       width=6.5, height=4.2, units="in", dpi=600)




# Build data
################################################################################

data1 <- data %>% 
  mutate(tmax_yr_bin=cut(tmax_yr, breaks=c(0, 5, 10, 20, 50, 100), labels=c("<5 yr", "5-10 yr", "10-20 yr", "20-50 yr", ">50 yr")),
         linf_cm_bin=cut(linf_cm, breaks=c(0, 25, 50, 100, 200), labels=c("<25 cm", "25-50 cm", "50-100 cm", "100-200 cm")))

g1 <- ggplot(data1, aes(x=linf_cm, fill=linf_cm_bin)) +
  geom_histogram(binwidth=2.5, closed="left") +
  # Labels
  labs(x="Linf (cm)", y="Number of species") +
  # Theme
  theme_bw()

g2 <- ggplot(data1, aes(x=tmax_yr, fill=tmax_yr_bin)) +
  geom_histogram(binwidth=2.5, closed="left") +
  # Labels
  labs(x="Max age (yr)", y="Number of species") +
  # Theme
  theme_bw()

g <- gridExtra::grid.arrange(g1, g2)


# Proportion by age
stats_age <- data1 %>% 
  group_by(tmax_yr_bin, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(tmax_yr_bin) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Max age (yr)") %>% 
  rename(variable=tmax_yr_bin)

stats_linf <- data1 %>% 
  group_by(linf_cm_bin, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(linf_cm_bin) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(group="Linf (cm)") %>% 
  rename(variable=linf_cm_bin)

stats_size <- bind_rows(stats_linf, stats_age) %>% 
  mutate(variable=factor(variable, 
                         levels=c(levels(stats_linf$variable), levels(stats_age$variable))))


g <- ggplot(stats_size, aes(x=prop, y=variable, fill=catg)) +
  # Facet
  facet_grid(group~., space="free_y", scales="free_y") +
  # Data
  geom_bar(stat="identity", color="grey30", lwd=0.3) +
  # geom_text(data=nstats %>% filter(group!="Order"), 
  #           mapping=aes(label=n, y=variable), inherit.aes = F,
  #           x=1.07, hjust=0.5, size=1.8, color="grey40") +
  # Labels
  labs(x="% of species", y="") +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1,0.25), lim=c(0, 1.09)) +
  # Legend
  scale_fill_manual(name="Abundance", values=c("lightgreen", "yellow",  "orange", "red") %>% rev()) +
  # Theme
  theme_bw() + my_theme
g


# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_score_by_catg_size.png"), 
       width=5.5, height=3.5, units="in", dpi=600)
  