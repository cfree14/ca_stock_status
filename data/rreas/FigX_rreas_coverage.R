
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(rerddap)

# Directories
indir <- "data/rreas/raw"
outdir <- "data/rreas/processed"
plotdir <- "data/rreas/figures"

# Read data
catch <- readRDS(file=file.path(outdir, "RREAS_catch_data.Rds"))
stations <- readRDS(file=file.path(outdir, "RREAS_station_key.Rds"))
spp_key <- readRDS(file=file.path(outdir, "RREAS_species_key.Rds"))
hauls <- readRDS(file=file.path(outdir, "RREAS_haul_key.Rds"))




# Plot station maps
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.7, 0.8),
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot(stations, aes(x=long_dd, y=lat_dd, color=strata)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Legend
  scale_color_discrete(name="Strata", guide = guide_legend(reverse = TRUE)) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32, 42)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_rreas_station_map.png"), 
       width=3.2, height=4.5, units="in", dpi=600)


# Plot data
ggplot(stations, aes(x=long_dd, y=lat_dd, color=area)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Crop
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Theme
  theme_bw()


# Plot station maps
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Times of year
ptsize <- 0.1
g1 <- ggplot(hauls %>% filter(strata=="Central"), aes(x=yday, y=year, color=strata)) +
  geom_point(size=ptsize) +
  # Labels
  labs(x="Day of year", y="Year", tag="A") +
  scale_y_reverse(breaks=seq(1980,2025,5)) +
  # Legend
  scale_color_discrete(name="Strata", guide = guide_legend(reverse = TRUE), drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Times of year
g2 <- ggplot(hauls, aes(x=yday, y=year, color=strata)) +
  geom_point(size=ptsize) +
  # Labels
  labs(x="Day of year", y="Year", tag="B") +
  scale_y_reverse(breaks=seq(1980,2025,5)) +
  # Legend
  scale_color_discrete(name="Strata", guide = guide_legend(reverse = TRUE), drop=F) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_rreas_temporal_coverage.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# Plot species occurences
################################################################################

# Nyr
n_distinct(hauls$year)
ntows_tot <- hauls %>% 
  # Focus on Central
  filter(strata=="Central") %>% 
  nrow()

# Species
stats <- catch %>% 
  # Focus on Central
  filter(strata=="Central" & maturity_code %in% c("A", "Y") & catch_n>0) %>% 
  # Summarize
  group_by(spp_code, comm_name, sci_name, maturity, maturity_code) %>% 
  summarize(nyrs=n_distinct(year),
            ntows=n()) %>% 
  # Filter
  filter(nyrs>25 & ntows>250) %>% 
  # Compute percent of tows
  mutate(ptows=ntows/ntows_tot) %>% 
  # Add common name long
  mutate(comm_name_long=ifelse(maturity_code=="A", paste(comm_name, "(adults)"), comm_name))
  # Format maturity

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position=c(0.8, 0.8),
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(y=reorder(comm_name_long, desc(ptows)), x=ptows, fill=maturity)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tows", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Maturity") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_rreas_species_to_evaluate.png"), 
       width=6.5, height=3.5, units="in", dpi=600)
