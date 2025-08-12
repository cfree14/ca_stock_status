
# Read data
################################################################################

# Turn off scientific notation
options(scipen=999)

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Get data
comm_orig <- wcfish::pacfin_all1
rec_orig <- wcfish::recfin_cte2

# Read mgmt key
mgmt_key <- readRDS(file="data/mgmt_plans/spp_mgmt_key.Rds") %>% 
  mutate(fmp=recode(fmp,
                "PFMC Coastal Pelagic Species" = "PFMC CPS", 
                "PFMC Highly Migratory Species" = "PFMC HMS",
                "CDFW Enhanced Status Report"="CDFW ESR"))


# Prepare recreational data
################################################################################

fmp_list <- c("PFMC Groundfish", "PFMC CPS", "PFMC HMS", "PFMC Salmon",
              "CDFW Nearshore", "CDFW White Seabass", "CDFW Pacific Herring", "CDFW ESR",
              "Unmanaged") %>% rev()

# Format data
rec <- rec_orig %>% 
  # Filter
  filter(state=="California" & status=="Retained" & type=="fish") %>% 
  # Correct some scientific names
  mutate(sci_name=recode(sci_name, 
                         "Semicossyphus pulcher"="Bodianus pulcher",
                         "Beringraja stellulata" = "Caliraja stellulata")) %>% 
  # Add mgmt data
  left_join(mgmt_key %>% select(species, authority, fmp), by=c("sci_name"="species")) %>% 
  # Fill in missing
  mutate(fmp=ifelse(is.na(fmp), "Unmanaged", fmp)) %>% 
  # Summarize
  group_by(year, fmp) %>% 
  summarize(landings_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Categorize
  mutate(fmp=factor(fmp, fmp_list)) %>% 
  # Add prop
  group_by(year) %>% 
  mutate(prop=landings_mt/sum(landings_mt)) %>% 
  ungroup()

ggplot(rec, aes(x=year, y=landings_mt, fill=fmp)) + 
  geom_bar(stat="identity")

# Build data
comm <- comm_orig %>% 
  # Reduce to finfish
  filter(state=="California" & type=="fish") %>% 
  # Correct some scientific names
  mutate(sci_name=recode(sci_name, 
                         "Semicossyphus pulcher"="Bodianus pulcher",
                         "Beringraja stellulata" = "Caliraja stellulata")) %>% 
  # Add mgmt data
  left_join(mgmt_key %>% select(species, authority, fmp), by=c("sci_name_nom"="species")) %>% 
  # Summarize
  group_by(year, fmp) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Fill in missing
  mutate(fmp=ifelse(is.na(fmp), "Unmanaged", fmp)) %>% 
  # Categorize
  mutate(fmp=factor(fmp, fmp_list)) %>% 
  # Add prop
  group_by(year) %>% 
  mutate(prop_mt=landings_mt/sum(landings_mt),
         prop_usd=value_usd/sum(value_usd)) %>% 
  ungroup()


# Build species
################################################################################

# Species in rec catch by year
rec_spp <- rec_orig %>% 
  filter(type=="fish" & level=="species") %>% 
  select(year, comm_name, sci_name, level) %>% 
  unique()

# Species in comm catch by year
comm_spp <- comm_orig %>% 
  filter(type=="fish" & !is.na(sci_name_nom) & !grepl("spp.", sci_name_nom)) %>% 
  select(year, comm_name_nom, sci_name_nom) %>% 
  unique() %>% 
  rename(comm_name=comm_name_nom,
         sci_name=sci_name_nom)

# Species in catch by year
catch_spp <- bind_rows(rec_spp, comm_spp) %>% 
  select(year, sci_name) %>% 
  unique() %>% 
  # Add mgmt data
  left_join(mgmt_key %>% select(species, authority, fmp), by=c("sci_name"="species")) %>% 
  # Fill in missing
  mutate(fmp=ifelse(is.na(fmp), "Unmanaged", fmp)) %>% 
  # Categorize
  mutate(fmp=factor(fmp, fmp_list))

# Summarize
nspp <- catch_spp %>% 
  group_by(year, fmp) %>% 
  summarize(nspp=n_distinct(sci_name)) %>% 
  unique() %>% 
  # Add prop
  group_by(year) %>% 
  mutate(prop=nspp/sum(nspp)) %>% 
  ungroup()


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=5.5),
                    axis.title=element_text(size=6.5),
                    legend.text=element_text(size=5.5),
                    # legend.title=element_text(size=6),
                    legend.title=element_blank(),
                    plot.tag=element_text(size=7),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Margin
                    plot.margin = margin(1,1,1,1),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# FMP colors
fmp_colors <- c("grey70", 
                RColorBrewer::brewer.pal(4, "Reds"),
                RColorBrewer::brewer.pal(4, "Blues")) #%>% rev()

# Plot rec landings
g1 <- ggplot(rec, aes(x=year, y=landings_mt/1000, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Recreational\nlandings (1000s mt)", tag="A") +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot rec landings
g1b <- ggplot(rec, aes(x=year, y=prop, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y=" \nProportion", tag=" ") +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1b

# Plot commercial landings
g2 <- ggplot(comm, aes(x=year, y=landings_mt/1000, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Commercial\nlandings (1000s mt)", tag="B") +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot commercial landings
g2b <- ggplot(comm, aes(x=year, y=prop_mt, fill=fmp)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y=" \nProportion", tag=" ") +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2b

# Plot value
g3 <- ggplot(comm, aes(x=year, y=value_usd/1e6, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Commercial\nrevenues (USD millions)", tag="C") +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),,
        legend.position = c(0.62, 0.72),
        legend.key.size = unit(0.2, "cm"),
        # legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Plot commercial landings
g3b <- ggplot(comm, aes(x=year, y=prop_usd, fill=fmp)) +
  geom_bar(stat="identity") +
  # Axes
  # Labels
  labs(x="Year", y=" \nProportion", tag=" ") +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3b

# Plot number of species
g4 <- ggplot(nspp, aes(x=year, y=nspp, fill=fmp)) +
  geom_bar(stat="identity") +
  geom_vline(xintercept = 2004.5, linetype="dashed", color="black", linewidth=0.2) +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Number of harvested species", tag="D") +
  # Legend
  scale_fill_manual(name="Taxa type", values=fmp_colors) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g4

# Plot species richness
g4b <- ggplot(nspp, aes(x=year, y=prop, fill=fmp)) +
  geom_bar(stat="identity") +
  geom_vline(xintercept = 2004.5, linetype="dashed", color="black", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Proportion", tag=" ") +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g4b

# Merge data
x <- 0.15
g <- gridExtra::grid.arrange(g1, g2, g3, g4, 
                             g1b, g2b, g3b, g4b, nrow=2, 
                             widths=c(x, rep((1-x)/3, 3)),
                             heights=c(0.6, 0.4))

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_ca_landings_by_mgmt_catg.png"),
       width=6.5, height=3, units="in", dpi=600)



# # Merge data (no props)
# x <- 0.15
# g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=1, widths=c(x, rep((1-x)/3, 3)))
# 
# 
# # Export (no props)
# ggsave(g, filename=file.path(plotdir, "Fig3_ca_landings_by_mgmt_catg.png"), 
#        width=6.5, height=2, units="in", dpi=600)


