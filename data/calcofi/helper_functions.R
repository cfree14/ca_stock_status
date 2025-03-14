

# Function to build data
################################################################################

# Function to build and visualize data
# tow_data <- tows_orig; count_data <- data_orig; species <- "Northern anchovy"
build_data <- function(tow_data, count_data, species){
  
  # Build data
  ###########################################################
  
  # Subset tows
  tows <- tow_data 
  
  # Subset data
  sdata <- count_data %>% 
    # Filter to species of interest
    filter(comm_name==species)
  
  # Build full data
  data <- tows %>% 
    # Add common name
    mutate(comm_name=species_do) %>% 
    # Add larval densities
    left_join(sdata %>% select(tow_id, larvae_10m2), by="tow_id") %>% 
    # Change NAs to zeros
    mutate(larvae_10m2=ifelse(is.na(larvae_10m2), 0, larvae_10m2))
  
  
  # Build stations that were not visited
  ##############################################
  
  # Build key
  stations_yr <- expand.grid(order=stations$order,
                             year=sort(unique(data$year))) %>% 
    left_join(stations, by="order")
  
  # Stations visited
  stations_yes <- data %>% 
    count(year, line, station)
  
  # Stations not visited
  stations_no <- stations_yr %>% 
    left_join(stations_yes, by=c("year", "line", "station")) %>% 
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
  
  # Get land
  usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
  foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
  
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
    labs(title=species) +
    # Legend
    scale_color_gradientn(name=expression("Larvae (#/10m"^2*")"), 
                          colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                          trans="log10") +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Crop
    coord_sf(xlim = c(-126.5, -116.5), ylim = c(30, 38)) +
    # Theme
    theme_bw() + map_theme
  #g
  
  # Export figure
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_data.png")
  ggsave(g, filename=file.path(plotdir, figname ), 
         width=6.5, height=4.3, units="in", dpi=600) # 7 in height when 1951
  
  # Return
  ################################################################################
  
  # Return
  return(data)
  
}


# Function to fit index of abundace
################################################################################

# Function to build and visualize data
fit_model <- function(data){

  # Fit model
  m <- sdmTMB(
    data = data,
    formula = larvae_10m2 ~ -1 + s(yday) + as.factor(year),
    spatiotemporal = "iid",
    time = "year",
    spatial="on",
    mesh = mesh,
    family = tweedie(link = "log"))
  
  # m <- sdmTMB(
  #   data = data, 
  #   formula = larvae_10m2 ~ 0 + as.factor(year),
  #   time = "year", 
  #   mesh = mesh, 
  #   family = tweedie(link = "log"))
  
  # Return model
  return(m)

}


# Function validate model
################################################################################

inspect_model <- function(model, data, species){
  
  # Sanity check
  sanity(model)
  
  # Record and inspect residuals
  resids <- residuals(model, type="mle-mvn")
  mdata <- model$data
  mdata$resid <- resids
  hist(mdata$resid)
  species <- unique(mdata$comm_name)

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
    labs(x="Residual", y="# of tows", title=species) +
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
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_diagnostics.png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=6.5, units="in", dpi=600)
  
  
}

# Function to make predictions
################################################################################

make_preds <- function(model, pred_grid){
  
  # Build prediction grid
  area_km2 <- 5 * 5
  years <- sort(unique(model$data$year))
  pred_grid_yrs <- purrr::map_df(years, function(x){
    df <- pred_grid %>% 
      mutate(year=x) %>% 
      select(year, everything())
  }) %>% 
    mutate(yday=105) # Apr 15
  
  
  # Make predictions
  preds <- predict(model, newdata = pred_grid_yrs, return_tmb_object = TRUE)
  
  # Add species tag
  preds[["species"]] <- model$data$comm_name %>% unique()
  
  # Return predictions
  return(preds)
  
}

# Function to extract and visualize index
################################################################################

# Extract index
extract_index <- function(preds){
  
  # Get species
  species <- preds$species
  
  # Extract index
  area_km2 <- 40*40
  index <- get_index(preds, area = area_km2, bias_correct = TRUE)
  
  # Format index
  index_df <- index %>% 
    rename(index=est, index_lo=lwr, index_hi=upr) %>% 
    mutate(species=species) %>% 
    select(species, everything())
  
  # Plot index
  g <- ggplot(index_df, aes(year, index/1e6)) + 
    # Confidence intervals
    geom_ribbon(aes(ymin = index_lo/1e6, ymax = index_hi/1e6), fill="grey85") +
    # Estimate
    geom_line(linewidth=0.3) +
    # Labels
    labs(x="Year", y="Index of relative abundance", title = species ) +
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
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_index.png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=4.5, height=2.5, units="in", dpi=600)
  
  # Return
  return(index_df)

}


# Function to plot spatial random effects
################################################################################

plot_spatial_effects <- function(preds){
  
  # Plot effects
  ################################################################################
  
  # Extract fits
  fits <- preds$data
  species <- preds$species
  
  # Get land
  usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
  foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
  usa_utm <- usa %>% sf::st_transform(crs=32611)
  foreign_utm <- foreign %>% sf::st_transform(crs=32611)
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=9),
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
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  
  # Predictions (spatial fixed+random effects)
  ################################################################################
  
  # Plot spatial fixed+random effects
  g <- ggplot(data=fits, aes(x= long_utm11m, y= lat_utm11m, fill=exp(est))) +
    # Facet
    facet_wrap(~year, ncol=10) +
    # Plot land
    # geom_sf(data=foreign_utm, fill="grey70", color="white", lwd=0.2, inherit.aes = F) +
    # geom_sf(data=usa_utm, fill="grey70", color="white", lwd=0.2, inherit.aes = F) +
    # Data
    geom_tile() +
    # Labels
    labs(title=paste0(species, ": fixed+random effects")) +
    # Legend
    scale_fill_gradientn(name="Abundance", 
                         trans = "log10", 
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Crop
    # coord_sf(xlim = range(fits$long_utm11m), ylim = range(fits$lat_utm11m)) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.key.size=unit(0.3, "cm"),
          axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank())
  g
  
  # Export
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_distribution.png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=6.5, units="in", dpi=600)
  
  # Spatial random effects
  ################################################################################
  
  # Plot spatial random effects
  g <- ggplot(data=fits, aes(x= long_utm11m, y= lat_utm11m, fill=omega_s)) +
    # Facet
    facet_wrap(~year, ncol=10) +
    # # Plot land
    # geom_sf(data=foreign_utm, fill="grey70", color="white", lwd=0.2, inherit.aes = F) +
    # geom_sf(data=usa_utm, fill="grey70", color="white", lwd=0.2, inherit.aes = F) +
    # Data
    geom_tile() +
    # Labels
    labs(title=paste0(species, ": spatial random effects")) +
    # Legend
    scale_fill_gradient2() +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Crop
    # coord_sf(xlim = range(fits$long_utm11m), ylim = range(fits$lat_utm11m)) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.key.size=unit(0.3, "cm"),
          axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank())
  g
  
  # Export
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_spatial_re.png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=6.5, units="in", dpi=600)
  
  
  # Spatial-temporal random effects
  ################################################################################
  
  # Plot spatiotemporal random effects
  g <- ggplot(data=fits, aes(x= long_utm11m, y= lat_utm11m, fill=epsilon_st)) +
    # Facet
    facet_wrap(~year, ncol=10) +
    # Plot land
    # geom_sf(data=foreign_utm, fill="grey70", color="white", lwd=0.2, inherit.aes = F) +
    # geom_sf(data=usa_utm, fill="grey70", color="white", lwd=0.2, inherit.aes = F) +
    # Data
    geom_tile() +
    # Labels
    labs(title=paste0(species, ": spatiotemporal random effects")) +
    # Legend
    scale_fill_gradient2() +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Crop
    # coord_sf(xlim = range(fits$long_utm11m), ylim = range(fits$lat_utm11m)) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.key.size=unit(0.3, "cm"),
          axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank())
  g
  
  # Export
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_spatial_temporal_re.png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=6.5, units="in", dpi=600)
  
}
  

  