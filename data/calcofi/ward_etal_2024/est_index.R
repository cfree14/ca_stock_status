library(sdmTMB)
library(dplyr)
library(lubridate)
library(tidyr)
library(curl)

# Parameters that apply to multiple scripts
min_year <- 1985
pred_resolution <- 5 # resolution of prediction grid, km
top_species <- 50
use_seasons <- c(1,2,3,4)
min_years <- 30 # minimum number of years that a species needs to have data for
min_n <- 300

# load prediction grid
pred_grid <- readRDS("indices/pred_grid.rds")
pred_grid$season <- 2
pred_grid$yday <- 105 # Apr 15

# model function - edit to change form of model
# gam_fit <- function(df) {
#   gam(larvae_10m2 ~ as.factor(year) + s(latitude, longitude, by = year),
#       data = df,
#       family = tw()
#   )
# }

#url_str <- "https://github.com/ecosystem-state/ecodata/blob/main/inst/calcofi_index_data.rds"
#usethis::use_github_file(url_str,
#                         save_as = "data/index_data.rds")

githubURL <- ("https://raw.githubusercontent.com/ecosystem-state/ecodata/main/inst/calcofi_index_data.rds")
download.file(githubURL,"data/calcofi/ward_etal_2024/index_data.rds", method="curl")

dat = readRDS("data/calcofi/ward_etal_2024/index_data.rds")
for(spp in 1:length(unique(dat$scientific_name))) {
  # fit sdmTMB model
  newdat <- dplyr::filter(dat,
                          scientific_name==unique(dat$scientific_name)[spp])
  
  if(spp == 1) {
    # 10 ~ 181 knots
    mesh = make_mesh(newdat, xy_cols = c("longitude","latitude"),
                     cutoff = 10)
    plot(mesh)
  }
  
  newdat$fyear = as.factor(newdat$year)
  #newdat$present = ifelse(newdat$larvae_10m2 > 0, 1, 0)
  m <- sdmTMB(larvae_10m2 ~ -1 + s(yday) + fyear,
              spatiotemporal = "iid",
              time="year",
              spatial="on",
              family = tweedie(),
              mesh=mesh,
              data=newdat)
  
  pred_grid$fyear <- as.factor(pred_grid$year)
  pred = predict(m,
                 pred_grid,
                 return_tmb_object = TRUE)
  index = get_index(pred, bias_correct = TRUE) # note bias correction not used
  
  summaries <- dplyr::rename(dat, species = scientific_name) %>%
    dplyr::filter(species == unique(dat$scientific_name)[spp]) %>%
    dplyr::group_by(species, year) %>%
    dplyr::summarize(mean_cpue = mean(larvae_10m2),
                     n_pos_cpue = length(which(larvae_10m2 > 0)))
  pred = left_join(summaries, index[,c("year","est","lwr","upr","log_est","se")])
  
  if(spp == 1) {
    predictions_all = pred
  } else {
    predictions_all = rbind(predictions_all, pred)
  }
} # end spp loop

saveRDS(predictions_all, "indices/predicted_indices_sdmtmb.rds")