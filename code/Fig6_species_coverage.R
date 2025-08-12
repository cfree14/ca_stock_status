
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "output"

# Read list of all species in California
spp_all <- readRDS("data/fish_species/processed/ca_finfish_species.Rds") 
spp_do <- readRDS(file=file.path(outdir, "species_in_report_card.Rds"))


# Build data
################################################################################

# All of California
n_distinct(spp_all$species)
n_distinct(spp_all$class)
n_distinct(spp_all$order)
n_distinct(spp_all$family)
n_distinct(spp_all$genus)

# Number of species by order overall
stats_all <- spp_all %>% 
  group_by(order) %>% 
  summarize(nspp_tot=n_distinct(species)) %>% 
  ungroup() %>% 
  arrange(desc(nspp_tot))

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
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


g1 <- ggplot(data, aes(x=nspp, y=factor(order, stats_all$order), fill=type)) +
  geom_bar(stat="identity",  position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Number of species", y="", tag="A") +
  # Legend
  scale_fill_manual(name="", values=c("darkred", "grey90")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.7, 0.95))
g1

g2 <- ggplot(data, aes(x=prop, y=factor(order, stats_all$order), fill=type)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  geom_vline(xintercept=0.5, linetype="dashed", linewidth=0.2) +
  geom_text(data=stats_all, 
            mapping=aes(label=nspp_tot, y=order), 
            hjust=0, x=1.01, inherit.aes = F,
            color="grey30", size=2.4) +
  # Labels
  labs(x="Percent of species", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format(), lim=c(0,1.1),
                     breaks=seq(0,1,0.25)) +
  # Legend
  scale_fill_manual(name="", values=c("darkred", "grey90")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.65, 0.35))

# Export
ggsave(g, filename=file.path(plotdir, "Fig7_species_coverage.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

