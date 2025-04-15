
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "output"

# Read data
spp_all <- read.csv("data/fish_species/miller_lea_1972/ca_fish_species_miller_lea_1972.csv")
spp_do <- readRDS(file=file.path(outdir, "species_in_report_card.Rds"))


# Build data
################################################################################

# Stats all
stats_all <- spp_all %>% 
  group_by(order) %>% 
  summarize(nspp_tot=n_distinct(species)) %>% 
  ungroup()

# Stats do
stats_do <- spp_do %>% 
  group_by(order) %>% 
  summarize(nspp=n_distinct(sci_name)) %>% 
  ungroup()

# Merge
data <- stats_all %>% 
  left_join(stats_do) %>% 
  mutate(nspp=ifelse(is.na(nspp), 0, nspp)) %>% 
  mutate(nspp_missing=nspp_tot-nspp) %>% 
  gather(key="type", value="nspp", 3:ncol(.)) %>% 
  group_by(order) %>% 
  mutate(prop=nspp/nspp_tot) %>% 
  # Recoe type
  mutate(type=recode_factor(type,
                            "nspp"="Evaluated",
                            "nspp_missing"="Not evaluated"))

# Stats
stats <- data %>% 
  filter(type=="Evaluated") %>% 
  arrange(desc(nspp))
  

# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   plot.tag = element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


g1 <- ggplot(data, aes(x=nspp, y=factor(order, stats$order), fill=type)) +
  geom_bar(stat="identity",  position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Number of species", y="", tag="A") +
  # Legend
  scale_fill_manual(name="", values=c("darkred", "grey90")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.7, 0.8))
g1

g2 <- ggplot(data, aes(x=prop, y=factor(order, stats$order), fill=type)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Labels
  labs(x="Percent of species", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="", values=c("darkred", "grey90")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)


# Export
ggsave(g, filename=file.path(plotdir, "FigX_species_coverage2.png"), 
       width=10.5, height=6.5, units="in", dpi=600)

