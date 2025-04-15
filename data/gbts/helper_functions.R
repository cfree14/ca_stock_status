

# Function to build data
################################################################################

# Function to build and visualize data
# tow_data <- tows_orig; count_data <- data_orig; species <- "Northern anchovy"
build_data <- function(hauls, catch, species_do){
  
  # Build data
  ###########################################################
  
  # Subset data
  sdata <- catch %>% 
    # Filter to species of interest
    filter(comm_name_orig==species_do)
  
  # Build full data
  data <- hauls %>% 
    # Add common name
    mutate(comm_name=species_do) %>% 
    # Add larval densities
    left_join(sdata %>% select(trawl_id, cpue_kg_ha), by="trawl_id") %>% 
    # Change NAs to zeros
    mutate(cpue_kg_ha=ifelse(is.na(cpue_kg_ha), 0, cpue_kg_ha)) %>% 
    # Scale pass
    mutate(pass_scaled=ifelse(pass=="Summer", -0.5, 0.5))
  
  # Plot data
  ################################################################################
  
  # Map theme
  map_theme <- theme(axis.ticks=element_blank(),
                     axis.text=element_blank(),
                     axis.title=element_blank(),
                     legend.text=element_text(size=7, angle = 45, hjust = 0.5, vjust = 1),
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
  g <- ggplot(data %>% filter(cpue_kg_ha>0), aes(x=long_dd, y=lat_dd, color=cpue_kg_ha)) +
    facet_wrap(~year, ncol=7) +
    # Plot land
    geom_sf(data=foreign, fill="grey60", color="white", lwd=0.1, inherit.aes = F) +
    geom_sf(data=usa, fill="grey60", color="white", lwd=0.1, inherit.aes = F) +
    # Zero values
    geom_point(data %>% filter(cpue_kg_ha==0), mapping=aes(x=long_dd, y=lat_dd), color="grey90", size=0.3) +
    # Positive values
    geom_point(size=0.3) + 
    # Labels
    labs(title=species_do) +
    # Legend
    scale_color_gradientn(name="CPUE (kg/ka)", 
                          trans="log10",
                          breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                          labels=c("0.01", "0.1", "1", "10", "100", "1000", "10000"),
                          colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
    # Crop
    coord_sf(xlim = c(-126, -116), ylim = c(32, 42)) + # coastwide
    # coord_sf(xlim = c(-126, -116), ylim = c(32, 49)) + # coastwide
    # Theme
    theme_bw() + map_theme
  g
  
  # Export figure
  figname <- species_do %>% tolower() %>% gsub(" ", "_", .) %>% paste0("data_", ., ".png")
  ggsave(g, filename=file.path(plotdir, figname ), 
         width=6.5, height=5.0, units="in", dpi=600) 
  
  # Return
  ################################################################################
  
  # Return
  return(data)
  
}


# Function to fit index of abundace
################################################################################

# Function to build and visualize data
# https://github.com/pfmc-assessments/indexwc
fit_model <- function(data){

  # Fit model
  m <- sdmTMB(
    data = data,
    formula = cpue_kg_ha ~ 0 + as.factor(year) + pass_scaled,
    spatiotemporal = "iid",
    time = "year",
    spatial="on",
    mesh = mesh,
    family = delta_lognormal())
  
  
  # Return model
  return(m)

}


# Function validate model
################################################################################

parse_sanity_check <- function(x){
  
  hess_out <- ifelse(x$hessian_ok==T, 
                     "Hessian matrix is positive definite", 
                     "***Non-positive-definite Hessian matrix: model may not have converged***")
  eigen_out <- ifelse(x$eigen_values_ok==T, 
                      "No extreme or very small eigenvalues detected", 
                      "Extreme or very small eigenvalues detected: model may not have converged")
  nlminb_out <- ifelse(x$nlminb_ok==T, 
                       "Non-linear minimizer suggests successful convergence", 
                       "***Non-linear minimizer did not converge: do not trust this model***")
  range_out <- ifelse(x$range_ok==T, 
                      "Range parameters don't look unreasonably large", 
                      "A `range` parameter looks fairly large (> 1.5 the greatest distance in data)")
  gradients_out <- ifelse(x$gradients_ok==T, 
                          "No gradients with respect to fixed effects are large", 
                          "***See ?run_extra_optimization(), standardize covariates, and/or simplify the model***")
  se_mag_out <- ifelse(x$se_magnitude_ok==T, 
                       "No standard errors look unreasonably large", 
                       "***Some standard errors may be large***")
  se_na_out <- ifelse(x$se_na_ok==T, 
                      "No fixed-effect standard errors are NA", 
                      "***Some fixed-effect standard errors are NA***")
  sigmas_out <- ifelse(x$sigmas_ok==T, 
                       "Sigmas look okay", 
                       "***Sigmas are messed up")

  out <- paste(hess_out, eigen_out, nlminb_out, range_out, 
               gradients_out, se_mag_out, se_na_out, sigmas_out, sep="\n")
  return(out)
}


# model <- m
inspect_model <- function(model, data, species){
  
  # Sanity check
  san_out <- sanity(model)
  
  san_out_txt <- parse_sanity_check(san_out)
  
  # Record and inspect residuals
  resids1 <- residuals(model, type="mle-mvn", model=1)
  resids2 <- residuals(model, type="mle-mvn", model=2)
  mdata <- model$data
  mdata$resid1 <- resids1
  mdata$resid2 <- resids2
  species <- unique(mdata$comm_name)
  
  # Format for plotting
  resids <- mdata %>% 
    select(trawl_id, resid1, resid2) %>% 
    gather(key="model", value="resid", 2:ncol(.)) %>% 
    mutate(model=recode(model, 
                        "resid1"="Model 1",
                        "resid2"="Model 2"))

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
  g1 <- ggplot(resids, aes(x=resid, fill=model)) +
    facet_wrap(~model, scale="free_y") +
    geom_histogram() +
    # Reference line
    geom_vline(xintercept=0, linetype="dashed", color="grey30") +
    # Labels
    labs(x="Residual", y="# of tows", title=species) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="none", 
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  g1
  
  # QQ-plot
  qq1 <- qqnorm(mdata$resid1) %>% as.data.frame() %>% mutate(model="Model 1")
  qq2 <- qqnorm(mdata$resid2) %>% as.data.frame() %>% mutate(model="Model 2")
  qq_df <- rbind(qq1, qq2)
  g2 <- ggplot(qq_df, aes(x=x, y=y, color=model)) +
    geom_point(pch=1) +
    annotate("text",
             x=min(qq_df$x, na.rm=T),
             y=max(qq_df$y, na.rm=T)*0.7,
             label=san_out_txt, size=2, hjust=0) +
    # Reference line
    geom_abline(slope=1) +
    # Labels
    labs(x="Theoretical quantiles", y="Sample quantiles") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "none")
  g2
  

  # Spatial residuals
  g3 <- ggplot(mdata, aes(x=long_dd, y=lat_dd, color=resid1)) +
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
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0("diagnostics_", .,  ".png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=6.5, units="in", dpi=600)
  
  
}

# Function to make predictions
################################################################################

make_preds <- function(model, pred_grid){
  
  # Build prediction grid
  years <- sort(unique(model$data$year))
  pred_grid_yrs <- purrr::map_df(years, function(x){
    df <- pred_grid %>% 
      mutate(year=x) %>% 
      select(year, everything())
  }) %>% 
    mutate(pass_scaled=0.5) # Fall
  
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
  index <- get_index(preds, area = preds$data$area_ha, bias_correct = TRUE)
  
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
  figname <- species %>% tolower() %>% gsub(" ", "_", .) %>% paste0("index_", ., ".png")
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
  

  