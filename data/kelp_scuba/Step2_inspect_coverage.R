
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
indir <- "data/kelp_scuba/raw"
outdir <- "data/kelp_scuba/processed"
plotdir <- "data/kelp_scuba/figures"

# Read data
spp_orig <- readRDS(file=file.path(outdir, "scuba_species.Rds"))
data_orig <- readRDS(file=file.path(outdir, "scuba_data.Rds"))
transect_orig <- readRDS(file=file.path(outdir, "scuba_transects.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Format data
################################################################################

# Build sites
sites <- transect_orig %>% 
  group_by(site) %>% 
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd),
            nyrs=n_distinct(survey_year),
            nyrs_post2008=n_distinct(survey_year[survey_year>=2008])) %>% 
  ungroup() %>% 
  mutate(include_yn=ifelse(nyrs_post2008>10, "yes", "no"))

sites_use <- sites$site[sites$include_yn=="yes"]

# Export sites to use
sites2use <- sites %>% 
  filter(include_yn=="yes")
saveRDS(sites2use, file.path(outdir, "scuba_sites_to_evaluate.Rds"))

# Check unique
freeR::which_duplicated(sites$site)

# Summarize surveys
surveys <- transect_orig %>% 
  # Calculate latitude of a survey (which contains multiple transets)
  group_by(campus, method, survey_year, year, date, yday, site, site_old, survey_id) %>% 
  summarize(lat_dd=mean(lat_dd, na.rm=T)) %>% 
  ungroup() %>% 
  # Order campus
  mutate(campus=factor(campus, levels=c("VRG", "UCSB", "UCSC", "HSU") %>% rev())) %>% 
  arrange(campus, desc(lat_dd)) %>% 
  mutate(date_dummy=paste("2000", month(date), day(date), sep="-") %>% ymd()) %>% 
  # Mark sites
  left_join(sites %>% select(site, include_yn), by="site")

# Check unique
freeR::which_duplicated(surveys$survey_id)


# Spatial coverage
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot map
g1 <- ggplot(sites, aes(x=long_dd, y=lat_dd, fill=nyrs)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot sites
  geom_point(pch=21) +
  # Legend
  scale_fill_gradientn(name="# of years", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-124.5, -116.5), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = c(0.8, 0.8),
        axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot data
g2 <- ggplot(surveys, aes(x=date, y=reorder(site, desc(lat_dd)), color=include_yn)) +
  facet_grid(campus~., space="free_y", scales="free_y") +
  geom_point(size=0.3) +
  # Add reference line
  geom_vline(xintercept=ymd("2007-04-01"), color="red") +
  # Labels
  labs(x="", y="") +
  scale_x_date(breaks=seq(ymd("2000-01-01"), 
                          ymd("2020-01-01"), by="5 years"),
               date_label="%Y") +
  # Legend
  scale_color_manual(name="Include?", values=c("grey70", "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.38, 0.62))

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_kelp_scuba_map_spatiotemporal.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Temporal coverage
################################################################################

# Plot data
g <- ggplot(surveys, aes(x=date_dummy, y=year, color=campus)) +
  geom_point(size=0.5) +
  # Reference line
  # geom_hline(yintercept=2007.5, linetype="dashed", color="grey30") +
  # geom_vline(xintercept = 180, linetype="dashed", color="grey30") +
  geom_vline(xintercept = ymd("2000-07-01"), linetype="dashed", color="grey30") +
  # Labels
  labs(x="Day of year", y="") +
  scale_y_reverse() +
  scale_x_date(breaks=seq(ymd("2000-01-01"), 
                          ymd("2001-01-01"), by="1 month"),
               date_label="%b") +
  # Legend
  scale_color_discrete(name="Campus") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size = unit(0.5, "cm"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_kelp_scuba_seasonality.png"), 
       width=5.5, height=3, units="in", dpi=600)


# Species coverage
################################################################################

# Criteria
range(data_orig$year)
length(2008:2023)
14/16

# Species stats
stats <- data_orig %>% 
  # Filter
  filter(year>=2008 & yday>=182 & site %in% sites_use) %>% 
  # Summarize
  group_by(classcode, comm_name, sci_name) %>% 
  summarize(nyr=n_distinct(year), 
            nsurveys=n_distinct(survey_id),
            ntransects=n_distinct(transect_id)) %>% 
  ungroup() %>% 
  # Proportion
  mutate(psurveys=nsurveys/n_distinct(transect_orig$survey_id),
         ptransects=ntransects/n_distinct(transect_orig$transect_id)) %>% 
  # Remove species with insuffient info
  filter(nyr>=14) %>% 
  # Remove bad species
  filter(!comm_name %in% c("No organisms present in this sample") & !grepl("young of| or |,", comm_name))

freeR::check_names(stats$sci_name)

# Export 
saveRDS(stats, file=file.path(outdir, "scuba_species_to_evaluate.Rds"))


# Plot data
g <- ggplot(stats, aes(y=reorder(comm_name, desc(psurveys)), x=psurveys)) +
  geom_bar(stat="identity", fill="grey70") +
  # Reference line
  geom_vline(xintercept=0.025, color="red", linetype="dashed") +
  # Labels
  labs(y="", x="Percent of surveys") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text = element_text(size=7),
        axis.title = element_text(size=8))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_kelp_scuba_species.png"), 
       width=6.5, height=5, units="in", dpi=600)




