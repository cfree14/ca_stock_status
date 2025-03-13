
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)

# Directories
rawdir <- "data/calcofi/raw"
outdir <- "data/calcofi/processed"
plotdir <- "data/calcofi/figures"


# Read data
data_orig <- readRDS(file=file.path(outdir, "calcofi_fish_larvae_counts.Rds"))
tows_orig <- readRDS(file=file.path(outdir, "calcofi_fish_larvae_tows.Rds"))
stations <- readRDS(file=file.path(outdir, "calcofi_stations.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Export
pred_grid <- readRDS(file.path(outdir, "calcofi_prediction_grid.Rds"))

# Build data
################################################################################

# Subset tows
tows <- tows_orig %>% 
  # Filter to standardized data
  filter(!is.na(survey)) %>% 
  filter(tow_type!="Twin winged continuous-flow surface tow")

# Subset data
sdata <- data_orig %>% 
  # Filter to standardized data
  filter(!is.na(survey)) %>% 
  filter(tow_type!="Twin winged continuous-flow surface tow") %>% 
  # Filter to species of interest
  filter(comm_name=="Northern anchovy")

# Build full data
data <- tows %>% 
  # Add larval densities
  left_join(sdata %>% select(tow_id, larvae_10m2)) %>% 
  # Change NAs to zeros
  mutate(larvae_10m2=ifelse(is.na(larvae_10m2), 0, larvae_10m2))


# Build stations that were not visited
##############################################

# Build key
stations_yr <- expand.grid(order=stations$order,
                           year=sort(unique(data$year))) %>% 
  left_join(stations)

# Stations visited
stations_yes <- data %>% 
  count(year, line, station)

# Stations not visited
stations_no <- stations_yr %>% 
  left_join(stations_yes) %>% 
  filter(is.na(n))
  


# Plot data
################################################################################

# Map theme
map_theme <- theme(axis.ticks=element_blank(),
                   axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7, angle = 45, , vjust = 0.8, hjust=0.5),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=5),
                   plot.title = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data %>% filter(larvae_10m2>0), aes(x=long_dd, y=lat_dd, color=larvae_10m2)) +
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=foreign, fill="grey60", color="white", lwd=0.1, inherit.aes = F) +
  geom_sf(data=usa, fill="grey60", color="white", lwd=0.1, inherit.aes = F) +
  # Zero values
  geom_point(data %>% filter(larvae_10m2==0), mapping=aes(x=long_dd, y=lat_dd), color="grey90", size=0.3) +
  # Positive values
  geom_point(size=0.3) + 
  # Not visited
  geom_point(data=stations_no, mapping=aes(long_dd, y=lat_dd), inherit.aes = F, shape="x", size=0.8, color="black") +
  # Labels
  labs(title="Northern anchovy") +
  # Legend
  scale_color_gradientn(name="Larvae (#/m2)", 
                        colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                        trans="log10") +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126.5, -116.5), ylim = c(30, 38)) +
  # Theme
  theme_bw() + map_theme
#g

ggsave(g, filename=file.path(plotdir, "FigX_calcofi_counts_northern_anchovy.png"), 
       width=6.5, height=7, units="in", dpi=600)



# Fit index of relative abundance
################################################################################

# Step 1. Prepare data
#########################################

# Build data
data1 <- data %>% 
  # Rename
  rename(lat_utm11m=lat_utm11n,
         long_utm11m=long_utm11n) %>% 
  # Convert to UTM km
  mutate(lat_utm11km=lat_utm11m/1000,
         long_utm11km=long_utm11m/1000) %>% 
  # Add julian day
  mutate(yday=yday(date1)) %>% 
  # Temporary
  filter(year>=2000)

# Build mesh
mesh <- make_mesh(data1, c("long_utm11km", "lat_utm11km"), cutoff = 50) # 10 is a slow model fit, 5 is too big
plot(mesh)


# Step 2. Fit model
#########################################

# Fit model
m <- sdmTMB(
  data = data1, 
  formula = larvae_10m2 ~ 0 + as.factor(year),
  time = "year", 
  mesh = mesh, 
  family = tweedie(link = "log"))


# Step 3. Inspect fit
#########################################

# Sanity check
sanity(m)

# Record and inspect residuals
data1$resids <- residuals(m, type="mle-mvn")
hist(data1$resids)
qqnorm(data1$resids)
abline(a = 0, b = 1)


# Make predictions and extract index
################################################################################

# Build prediction grid
area_km2 <- 40*40
years <- sort(unique(data1$year))
pred_grid_yrs <- purrr::map_df(years, function(x){
  df <- pred_grid %>% 
    mutate(year=x) %>% 
    select(year, everything())
})

# Coastwide predictions
preds <- predict(m, newdata = pred_grid_yrs, return_tmb_object = TRUE)

# Extract index
index <- get_index(preds, area = area_km2, bias_correct = TRUE)

# Plot index
ggplot(index, aes(year, est)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab('Year') + ylab('Biomass estimate (kg)')


# Plot effects
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
usa_utm <- usa %>% sf::st_transform(crs=32611)
foreign_utm <- foreign %>% sf::st_transform(crs=32611)

# Extract fits
fits <- preds$data

# Plot spatial fixed+random effects
g <- ggplot(data=fits, aes(x= long_utm11m, y= lat_utm11m, fill=exp(est))) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  # geom_sf(data=foreign_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # geom_sf(data=usa_utm, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Data
  geom_tile() +
  # Labels
  labs(title="Fixed+random effects") +
  # Legend
  scale_fill_gradientn(name="Biomass density (kg/km2)", 
                       trans = "log10", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  # coord_sf(xlim = c(-126.5, -116.5), ylim = c(30, 38)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())
g





