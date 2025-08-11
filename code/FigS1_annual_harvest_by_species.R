
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
outdir <- "data/fish_species/processed"

# Get data
comm_orig <- wcfish::pacfin_all1
rec_orig <- wcfish::recfin_cte2

# Read mgmt key
mgmt_key <- readRDS(file="data/mgmt_plans/spp_mgmt_key.Rds") %>% 
  mutate(fmp=recode(fmp,
                "PFMC Coastal Pelagic Species" = "PFMC CPS", 
                "PFMC Highly Migratory Species" = "PFMC HMS",
                "CDFW Enhanced Status Report"="CDFW ESR"))


# Build data
################################################################################

# FMP order
fmp_list <- c("PFMC Groundfish", "PFMC CPS", "PFMC HMS", "PFMC Salmon",
              "CDFW Nearshore", "CDFW White Seabass", "CDFW Pacific Herring", "CDFW ESR",
              "Unmanaged") %>% rev()


# Prepare comm
comm1 <- comm_orig %>% 
  # Reduce
  filter(state=="California" & type=="fish" & !is.na(sci_name_nom) & !grepl("spp.", sci_name_nom) & year>=2005) %>% 
  # Summarize
  group_by(sci_name_nom, year) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T)) %>% 
  ungroup() %>% 
  rename(sci_name=sci_name_nom) %>% 
  # Fix some scientific names
  mutate(sci_name=recode(sci_name,
                         "Semicossyphus pulcher"="Bodianus pulcher"))

freeR::check_names(comm1$sci_name)

# Prepare rec
rec1 <- rec_orig %>% 
  # Filter
  filter(state=="California" & status=="Retained" & type=="fish" & level=="species") %>% 
  # Summarize
  group_by(sci_name, year) %>% 
  summarize(landings_mt=sum(catch_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Fix some scientific names
  mutate(sci_name=recode(sci_name,
                         "Semicossyphus pulcher"="Bodianus pulcher"))

freeR::check_names(rec1$sci_name)


# Species
spp1 <- comm_orig %>% 
  filter(state=="California" & type=="fish" & !is.na(sci_name_nom) & !grepl("spp.", sci_name_nom) & year>=2005) %>% 
  mutate(comm_name_nom=recode(comm_name_nom, "Black skate"="Roughtail skate")) %>% 
  select(comm_name_nom, sci_name_nom) %>% 
  unique() %>% 
  rename(comm_name=comm_name_nom,
         sci_name=sci_name_nom) 
freeR::which_duplicated(spp1$sci_name)
spp2 <- rec_orig %>% 
  # Filter
  filter(state=="California" & status=="Retained" & type=="fish" & level=="species") %>% 
  select(comm_name, sci_name) %>% 
  unique()
freeR::which_duplicated(spp2$sci_name)


# Merge
data <- bind_rows(comm1, rec1) %>% 
  # Summarize
  group_by(sci_name) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T)/length(2005:2024)) %>% 
  ungroup() %>% 
  # Add mgmt data
  left_join(mgmt_key %>% select(species, authority, fmp), by=c("sci_name"="species")) %>% 
  # Fill in missing
  mutate(fmp=ifelse(is.na(fmp), "Unmanaged", fmp)) %>% 
  # Categorize
  mutate(fmp=factor(fmp, fmp_list)) %>% 
  # Convert to pounds
  mutate(landings_kg=landings_mt*1e3,
         landings_lbs=measurements::conv_unit(landings_kg, "kg", "lbs") %>% pmax(., 1)) %>% 
  # Arrange
  arrange(desc(landings_mt)) %>% 
  # Add common name
  left_join(spp1, by="sci_name") %>% 
  rename(comm_name_comm=comm_name) %>% 
  left_join(spp2, by="sci_name") %>% 
  rename(comm_name_rec=comm_name) %>% 
  mutate(comm_name=ifelse(!is.na(comm_name_comm), comm_name_comm, comm_name_rec)) %>% 
  select(-c(comm_name_rec, comm_name_comm)) %>% 
  # Arrange
  select(comm_name, sci_name, authority, fmp, landings_mt, landings_kg, landings_lbs)

data1 <- data %>% 
  slice(1:100)
data2 <- data %>% 
  slice(101:200)
data3 <- data %>% 
  slice(201:300)

# Export
saveRDS(data, file=file.path(outdir, "ca_finfish_annual_harvest.Rds"))


# Plot data
################################################################################

# FMP colors
fmp_colors <- c("grey70", 
                RColorBrewer::brewer.pal(4, "Reds"),
                RColorBrewer::brewer.pal(4, "Blues")) #%>% rev()

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
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))


g1 <- ggplot(data1, aes(y=landings_lbs, x=reorder(comm_name, desc(landings_lbs)), fill=fmp)) +
  geom_point(pch=21, stroke=0.1, size=2) +
  geom_segment(y=0.01, mapping=aes(yend=landings_lbs, color=fmp)) +
  geom_hline(yintercept=1000) +
  # Labels
  labs(y="Annual landings (lbs, 2005-2024)", x="") +
  # Scale
  scale_y_continuous(trans="log10",
                     lim=c(1, 10^8),
                     breaks=c(1, 10, 100, 1000, 10^4, 10^5, 10^6, 10^7),
                     labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1 million", "10 million")) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  scale_color_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size = unit(0.1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1

g2 <- ggplot(data2, aes(y=landings_lbs, x=reorder(comm_name, desc(landings_lbs)), fill=fmp)) +
  geom_point(pch=21, stroke=0.1, size=2) +
  geom_segment(y=0.01, mapping=aes(yend=landings_lbs, color=fmp)) +
  geom_hline(yintercept=1000) +
  # Labels
  labs(y="Annual landings (lbs, 2005-2024)", x="") +
  # Scale
  scale_y_continuous(trans="log10",
                     lim=c(1, 10^8),
                     breaks=c(1, 10, 100, 1000, 10^4, 10^5, 10^6, 10^7),
                     labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1 million", "10 million")) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  scale_color_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

g3 <- ggplot(data3, aes(y=landings_lbs, x=reorder(comm_name, desc(landings_lbs)), fill=fmp)) +
  geom_point(pch=21, stroke=0.1, size=2) +
  geom_segment(y=0.1, mapping=aes(yend=landings_lbs, color=fmp)) +
  geom_hline(yintercept=1000) +
  # Labels
  labs(y="Annual landings (lbs, 2005-2024)", x="") +
  # Scale
  scale_y_continuous(trans="log10",
                     lim=c(1,10^8),
                     breaks=c(1, 10, 100, 1000, 10^4, 10^5, 10^6, 10^7),
                     labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1 million", "10 million")) +
  # Legend
  scale_fill_manual(name="FMP", values=fmp_colors, drop=F) +
  scale_color_manual(name="FMP", values=fmp_colors, drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        legend.key.size = unit(0.1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, heights=c(0.4, 0.3, 0.3))

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_annual_landings_average.png"), 
       width=6.5, height=7.5, units="in", dpi=600)

