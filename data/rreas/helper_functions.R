

# Function to build data
################################################################################

# Function to build and visualize data
# tow_data <- tows_orig; count_data <- data_orig; species <- "North pacific hake (adults)"; "Blue rockfish"
build_data <- function(tow_data, count_data, species){
  
  # Build data
  ###########################################################
  
  # Subset tows
  tows <- tow_data 
  
  # Subset data
  sdata <- count_data %>% 
    # Filter to species of interest
    filter(comm_name_long==species) %>% 
    mutate(yday=lubridate::yday(date),
           yday2=yday^2)
  
  
  # Build stations that were not visited
  ##############################################
  
  # Build key
  stations_yr <- expand.grid(station_id=stations$station_id,
                             year=sort(unique(sdata$year))) %>% 
    left_join(stations, by="station_id")
  
  # Stations visited
  stations_yes <- sdata %>% 
    count(year, station_id)
  
  # Stations not visited
  stations_no <- stations_yr %>% 
    left_join(stations_yes, by=c("year", "station_id")) %>% 
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
  g <- ggplot(sdata %>% filter(catch_n>0), aes(x=long_dd, y=lat_dd, color=catch_n)) +
    facet_wrap(~year, ncol=7) +
    # Plot land
    geom_sf(data=foreign, fill="grey60", color="white", lwd=0.1, inherit.aes = F) +
    geom_sf(data=usa, fill="grey60", color="white", lwd=0.1, inherit.aes = F) +
    # Zero values
    geom_point(sdata %>% filter(catch_n==0), mapping=aes(x=long_dd, y=lat_dd), color="grey90", size=0.6) +
    # Positive values
    geom_point(size=0.6) + 
    # Not visited
    geom_point(data=stations_no, mapping=aes(long_dd, y=lat_dd), inherit.aes = F, shape="x", size=1, color="black") +
    # Labels
    labs(title=species) +
    # Legend
    scale_color_gradientn(name="Count", 
                          colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                          trans="log10") +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Crop
    coord_sf(xlim = c(-124, -121.5), ylim = c(36.5, 38.3)) +
    # Theme
    theme_bw() + map_theme
  g
  #g
  
  # Export figure
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0(., "_data.png")
  ggsave(g, filename=file.path(plotdir, figname ), 
         width=6.5, height=6.25, units="in", dpi=600) # 7 in height when 1951
  
  # Return
  ################################################################################
  
  # Return
  return(sdata)
  
}


# Function to fit index of abundace
################################################################################

# Function to build and visualize data
fit_model <- function(data){

  # Fit model
  m <- sdmTMB(
    data = data,
    formula = catch_n ~ -1 + yday + as.factor(year), # yday as smoother created problems
    spatiotemporal = "iid",
    time = "year",
    spatial="on",
    mesh = mesh,
    family = tweedie(link = "log"))
  
  # Sanity check
  sanity(m)
  
  # m <- sdmTMB(
  #   data = data, 
  #   formula = larvae_10m2 ~ 0 + as.factor(year),
  #   time = "year", 
  #   mesh = mesh, 
  #   family = tweedie(link = "log"))
  
  # Return model
  return(m)

}

# Function to fit index of abundace
################################################################################

# Function to build and visualize data
fit_model_gams <- function(data){
  
  # Format data
  data_yn <- data %>% 
    mutate(present_yn = ifelse(catch_n==0, 0, 1))
  data_nonzero <- data_yn %>% 
    filter(catch_n>0)
  
  # Fit presence only model
  model_pres <- mgcv::gam(present_yn ~ yday + s(lat_utm10km, long_utm10km) + as.factor(year) +I(yday^2),
                          data = data_yn,
                          family = "binomial")

  # Fit non-zero model
  model_pois <- mgcv::gam(catch_n ~ yday + s(lat_utm10km, long_utm10km) + as.factor(year) +I(yday^2),
                          data = data_nonzero,
                          family = "poisson")
  
  # Merge models
  models <- list(model_pres, model_pois)
  
  # Return model
  return(models)
  
}

make_preds_gams <- function(models, data, pred_grid){
  
  # Seperate models
  model_pres <- models[[1]]
  model_pois <- models[[2]]
  
  # Years to predict to
  # (can only use years in which at least 1 positive value was found)
  years <- data %>% 
    filter(catch_n>0) %>% 
    pull(year) %>% unique() %>% sort()
  
  # Compute "design-based" index of abundanc
  index_obs <- data %>% 
    group_by(year) %>% 
    summarize(p_present_obs=sum(catch_n>0)/n(),
              count_when_present_obs=mean(catch_n[catch_n>0]),
              expected_count_obs=mean(catch_n))
  
  # Build prediction grid
  pred_grid_yrs <- purrr::map_df(years, function(x){
    df <- pred_grid %>% 
      mutate(year=x) %>% 
      select(year, everything())
  }) %>% 
    mutate(yday=145)
  
  # Make predictions
  preds_pres <- predict(model_pres1, newdata = pred_grid_yrs, type="response")
  preds_pois <- predict(model_pois1, newdata = pred_grid_yrs, type="response")

  # An experiment to prove that I know what the inverse link function is
  # Because they are perfectly correlated, I do understand!
  preds_pres1 <- predict(model_pres1, newdata = pred_grid_yrs, type="link") %>% plogis()
  preds_pois1 <- predict(model_pois1, newdata = pred_grid_yrs, type="link") %>% exp()
  plot(preds_pres1 ~  preds_pres)
  plot(preds_pois1 ~  preds_pois)
  
  # Build clean predictions data frames
  preds_pres_df <- pred_grid_yrs %>% 
    # Add predictions from hurdle model components
    mutate(p_present=preds_pres,
           count_when_present=preds_pois) %>% 
    # Multiply to get average count
    mutate(expected_count=p_present*count_when_present) %>% 
    # Summarize by year
    group_by(year) %>% 
    summarize(p_present=mean(p_present),
              count_when_present=mean(count_when_present),
              expected_count=mean(count_when_present)) %>% 
    ungroup() %>% 
    # Add raw index
    left_join(index_obs, by="year") %>% 
    # Gather
    gather(key="metric_long", value="value", 2:ncol(.)) %>% 
    # Add columsn
    mutate(type=ifelse(grepl("obs", metric_long), "Observed", 'Predicted'),
           metric=gsub("_obs", "", metric_long)) %>% 
    # Recode metric
    mutate(metric=recode_factor(metric,
                                "p_present"="P(presence)",
                                "count_when_present"="Count when present",
                                "expected_count"="Expected count"))
  
  ggplot(preds_pres_df, aes(x=year, y=value, color=type)) +
    facet_wrap(~metric, nrow=1, scales="free_y") +
    geom_line() +
    # Labels
    labs(x="", y="") +
    scale_x_continuous(breaks=seq(1980,2025, 5)) +
    # Theme
    theme_bw()

  
  # Return predictions
  return(preds)
  
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
    mutate(yday=145) 
  
  
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
  area_km2 <- 5 * 5
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
  

  