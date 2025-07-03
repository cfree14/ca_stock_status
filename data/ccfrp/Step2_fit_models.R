
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
datadir <- "data/ccfrp/processed"
plotdir <- "data/ccfrp/figures"
outdir <- "data/ccfrp/output"

# Read data
sites <- readRDS(file=file.path(datadir, "ccfrp_sites.Rds"))
catch <- readRDS(file=file.path(datadir, "ccfrp_catch_data.Rds"))
surveys <- readRDS(file=file.path(datadir, "ccfrp_surveys.Rds")) 
spp2eval <- readRDS(file=file.path(datadir, "CCFRP_species_to_evaluate.Rds"))

# Read helper functions
source("data/ccfrp/helper_functions.R")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
  sf::st_transform(crs = 32610)
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")  %>% 
  sf::st_transform(crs = 32610)


# Build data
################################################################################

# Prepare data
data <- catch %>% 
  # Reduce to central region 
  filter(mpa_region=="Central") %>% 
  # Reduce to qualifying species
  filter(sci_name %in% spp2eval$sci_name) %>% 
  # Simplify
  select(year, date, yday, mpa_status, cell_id, lat_dd, long_dd,
         survey_id, angler_hrs,
         comm_name, sci_name, count, cpue_n_hr, cpue_kg_hr) %>% 
  # Add UTM coordinates
  sdmTMB::add_utm_columns(ll_names=c("long_dd", "lat_dd"),
                          utm_names=c("long_utm10km", "lat_utm10km"),
                          ll_crs=4326,
                          utm_crs = 32610,
                          units="km")

# Inspect
freeR::complete(data)

# Build locations
locations <- data %>% 
  select(cell_id, lat_dd, long_dd, lat_utm10km, long_utm10km) %>% 
  unique()
freeR::which_duplicated(locations$cell_id)


# Build grid
################################################################################

# Resolution
res_km <- 5
res_m <- res_km * 1000

# Build prediction grid
pred_grid <- locations %>% 
  # Simplify
  select(long_utm10km, lat_utm10km) %>% 
  # Round
  mutate(long_utm10km=floor(long_utm10km/res_km)*res_km,
         lat_utm10km=floor(lat_utm10km/res_km)*res_km,
         long_utm10m=long_utm10km*1000,
         lat_utm10m=lat_utm10km*1000) %>% 
  # Simplify
  unique()

# Export
saveRDS(pred_grid, file=file.path(datadir, "CCFRP_prediction_grid.Rds"))

# Plot
g <- ggplot(pred_grid, aes(x=long_utm10m, y=lat_utm10m)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot grid
  geom_tile(fill="red") +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = range(pred_grid$long_utm10m), ylim = range(pred_grid$lat_utm10m)) +
  # Theme
  theme_bw()
g

# Expand grid
################################################################################

# Expand grid
years <- sort(unique(data$year))
pred_grid_yr <- purrr::map_df(years, function(x){
  df <- pred_grid %>% 
    mutate(year=x,
           mpa_status="REF")
})

# Fit model
################################################################################

# Species
species <- unique(data$comm_name)

# Loop through species
i <- 1
for(i in 1:length(species)){
  
  # Build data and mesh
  ####################################################
  
  # Subset data
  spp_do <- species[i]
  sdata <- data %>% 
    filter(comm_name==spp_do)
  
  # Build mesh
  mesh <- make_mesh(sdata, c("long_utm10km", "lat_utm10km"), cutoff = 1) # 10 is a slow model fit, 5 is too big
  plot(mesh)
  
  # Fit model
  ####################################################
  
  # Fit model
  model <- sdmTMB(
    data = sdata,
    formula = cpue_n_hr ~ -1 + mpa_status + as.factor(year), # yday as smoother created problems
    spatiotemporal = "iid",
    time = "year",
    spatial="on",
    mesh = mesh,
    family = tweedie(link = "log"))
  
  # Sanity check
  sanity(model)
  
  # Inspect model
  ####################################################
  
  san <- sanity(model)
  san_text <- parse_sanity_check(san)
  
  # Record and inspect residuals
  resids <- residuals(model, type="mle-mvn")
  mdata <- model$data
  mdata$resid <- resids
  hist(mdata$resid)

  # Base theme
  base_theme <- theme(axis.text=element_text(size=7),
                      axis.title=element_text(size=8),
                      plot.title=element_text(size=9),
                      # Gridlines
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      # Legend
                      legend.key = element_rect(fill = NA, color=NA),
                      legend.background = element_rect(fill=alpha('blue', 0)))
  
  
  # Residual histogram
  g1 <- ggplot(mdata, aes(x=resid)) +
    geom_histogram(fill="grey80") +
    # Reference line
    geom_vline(xintercept=0, linetype="dashed", color="grey30") +
    # Labels
    labs(x="Residual", y="# of tows", title=spp_do) +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  g1
  
  # QQ-plot
  qq <- qqnorm(mdata$resid)
  qq_df <- tibble(x=qq$x,
                  y=qq$y)
  g2 <- ggplot(qq_df, aes(x=x, y=y)) +
    geom_point(pch=1, color="grey50") +
    annotate("text",
             x=min(qq_df$x, na.rm=T),
             y=max(qq_df$y, na.rm=T)*0.7,
             label=san_text, size=2, hjust=0) +
    # Reference line
    geom_abline(slope=1) +
    # Labels
    labs(x="Theoretical quantiles", y="Sample quantiles") +
    # Theme
    theme_bw() + base_theme
  g2
  
  # Spatial residuals
  g3 <- ggplot(mdata, aes(x=long_dd, y=lat_dd, color=resid)) +
    facet_wrap(~year) +
    geom_point(size=0.5) +
    # Legend
    scale_color_gradient2(name="Residual") +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2, 
                                  direction = "horizontal", title.position = "top", title.hjust=0.5)) +
    # Theme
    theme_bw() + base_theme +
    theme(axis.title=element_blank(),
          strip.text = element_text(size=5),
          strip.background = element_rect(colour=NA, fill=NA),
          axis.text=element_text(size=5),
          legend.text=element_text(size=5),
          legend.title=element_text(size=6),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          # legend.position = "top",
          # legend.margin = margin(0,0,0,0)
          legend.position=c(0.82, 0.03),
          legend.key.size = unit(0.3, "cm"))
  g3
  
  # Merge
  layout_matrix <- matrix(data=c(1,3,
                                 2,3), ncol=2, byrow = T)
  g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.54, 0.6))
  
  # Export
  figname <- spp_do %>% tolower() %>% gsub(" ", "_", .) %>% paste0("diagnostics_", .,  ".png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=6.5, units="in", dpi=600)
  
  
  # Make predictions
  ####################################################
  
  # Make predictions
  preds <- predict(model, newdata = pred_grid_yr, return_tmb_object = TRUE)
  
  
  # Build index
  ####################################################
  
  # Get index
  index <- get_index(preds, area=res_km*res_km, bias_correct = TRUE)
  
  # Format index
  index_df <- index %>% 
    rename(index=est, index_lo=lwr, index_hi=upr) %>% 
    mutate(species=spp_do) %>% 
    select(species, everything())
  
  # Plot index
  g <- ggplot(index_df, aes(year, index/1e6)) + 
    # Confidence intervals
    geom_ribbon(aes(ymin = index_lo/1e6, ymax = index_hi/1e6), fill="grey85") +
    # Estimate
    geom_line(linewidth=0.3) +
    # Labels
    labs(x="Year", y="Index of relative abundance", title = spp_do) +
    scale_x_continuous(breaks=seq(1980,2030,5)) +
    # Them
    theme_bw() +
    theme(axis.text=element_text(size=7),
          axis.title=element_text(size=8),
          axis.text.y = element_text(angle = 90, hjust = 0.5), 
          plot.title=element_text(size=9),
          # Gridlines
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  g
  
  # Export
  figname <- spp_do %>% tolower() %>% gsub(" ", "_", .) %>% paste0("index_", ., ".png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=4.5, height=2.5, units="in", dpi=600)
  
  # Export index
  filename <- spp_do %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., ".Rds")
  saveRDS(index_df, file=file.path(outdir, filename))
  
  
  
}



