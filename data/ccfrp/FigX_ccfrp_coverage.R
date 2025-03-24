
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)
library(foreach)
library(doParallel)

# Directories
indir <- "data/ccfrp/raw"
outdir <- "data/ccfrp/processed"
plotdir <- "data/ccfrp/figures"

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read data
sites <- readRDS(file=file.path(outdir, "ccfrp_sites.Rds"))
catch <- readRDS(file=file.path(outdir, "ccfrp_catch_data.Rds"))
surveys <- readRDS(file=file.path(outdir, "ccfrp_surveys.Rds")) %>% 
  mutate(date_dummy=paste("2000", month(date), day(date), sep="-") %>% ymd())


# Plot map
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     axis.title.y=element_blank(),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=6),
                     plot.title=element_text(size=7),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.5, "cm"),
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot map
g1 <- ggplot(sites %>% filter(!is.na(lat_dd)), aes(x=long_dd, y=lat_dd, color=mpa_region)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot sites
  geom_point() +
  # Legend
  scale_color_discrete(name="Region", guide = guide_legend(reverse = TRUE)) +
  # Crop
  coord_sf(xlim = c(-124.5, -116.5), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(), 
        legend.position = c(0.8, 0.8))
g1

# Timeline
g2 <- ggplot(surveys, aes(x=date, y=reorder(area, desc(area)), color=mpa_region)) +
  geom_point() +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Region") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.47, 0.53))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_ccfrp_map_spatiotemporal.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Temporal coverage
################################################################################

g1 <- ggplot(surveys %>% filter(mpa_region=="Central"), 
             aes(x=date_dummy, y=year, 
                 color=mpa_region)) +
  geom_point() +
  # Labels
  labs(x="Day of year", y="") +
  scale_y_reverse(breaks=seq(2004,2026,2)) + 
  scale_x_date(breaks=seq(ymd("2000-06-01"), 
                          ymd("2000-12-01"), by="1 month"),
               lim=c(ymd("2000-05-20"), ymd("2000-12-01")),
               date_label="%b") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

g2 <- ggplot(surveys, aes(x=date_dummy, y=year, color=mpa_region)) +
  geom_point() +
  # Labels
  labs(x="Day of year", y="") +
  scale_y_reverse(breaks=seq(2004,2026,2)) +
  scale_x_date(breaks=seq(ymd("2000-06-01"), 
                          ymd("2000-12-01"), by="1 month"),
               lim=c(ymd("2000-05-20"), ymd("2000-12-01")),
               date_label="%b") +
  scale_color_discrete(name="Region") +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.42, 0.58))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_ccfrp_seasonality.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Species coverage
################################################################################

# Total surveys
nsurveys_tot <- catch %>% 
  filter(mpa_region=="Central") %>% 
  pull(survey_id) %>% n_distinct()

# Species stats
stats <- catch %>% 
  filter(mpa_region=="Central" & count>0) %>% 
  group_by(comm_name) %>% 
  summarize(nyr=n_distinct(year),
            nsurveys=n_distinct(survey_id)) %>% 
  ungroup() %>% 
  mutate(psurveys=nsurveys/nsurveys_tot) %>% 
  filter(nyr>=14 & nsurveys >= 10)

# Plot data
g <- ggplot(stats, aes(x=psurveys, y=reorder(comm_name, desc(psurveys)))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of surveys", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + base_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_ccfrp_species.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

