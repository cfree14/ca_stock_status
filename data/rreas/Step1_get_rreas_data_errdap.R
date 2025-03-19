
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

# Datasets on ERDDAP
# https://oceanview.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=Rockfish+Recruitment

# Ward et al. (2025) code
# https://github.com/ecosystem-state/rreas-auto

# Get data
################################################################################

# CPUE data (but large groups)
cpue_info <- rerddap::info('cciea_EI_FBC', url = "https://oceanview.pfeg.noaa.gov/erddap/")
cpue_orig <- rerddap::tabledap(x=cpue_info, url="https://oceanview.pfeg.noaa.gov/erddap/") # fmt="csv"

# Catch data
catch_info <- rerddap::info('FED_Rockfish_Catch', url = "https://oceanview.pfeg.noaa.gov/erddap/")
catch_orig <- rerddap::tabledap(x=catch_info, url="https://oceanview.pfeg.noaa.gov/erddap/") # fmt="csv"


# Format catch data
################################################################################

# Format catch data
# Metadata: https://oceanview.pfeg.noaa.gov/erddap/info/FED_Rockfish_Catch/index.html
catch <- catch_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(date_time=time,
         lat_dd=latitude,
         long_dd=longitude,
         haul_num=haul_no,
         worms_id=aphiaid,
         worms_match_type=match_type,
         ls_id=lsid,
         spp_code=species_code,
         comm_name=common_name,
         station_id=station,
         station_lat_dd=station_latitude,
         station_long_dd=station_longitude,
         station_depth_m=station_bottom_depth,
         headrope_depth_m=tdr_depth,
         depth_m=bottom_depth,
         catch_n=catch,
         maturity_code=maturity) %>% 
  # Add date
  mutate(date=lubridate::ymd(substr(date_time, 1,10)),
         year=lubridate::year(date)) %>% 
  # Format strata
  mutate(strata=recode_factor(strata,
                              "S"="South",
                              "SC"="South Central",
                              "C"="Central", 
                              "NC"="North Central", 
                              "N"="North")) %>% 
  # Add haul id
  mutate(haul_id=paste(cruise, haul_num, sep="-")) %>% 
  # Format maturity
  mutate(maturity=recode(maturity_code,
                         "Y"="YOY",
                         "U"="Unknown",
                         "T"="Transitioning???",
                         "A"="Adult")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Recode missing GPS points
  mutate(lat_dd=ifelse(lat_dd==0, station_lat_dd, lat_dd),
         long_dd=ifelse(long_dd==0, station_long_dd, long_dd)) %>% 
  # Arrange
  select(cruise, haul_num, haul_id, vessel,
         year, date, date_time,
         strata, area, 
         station_id, station_lat_dd, station_long_dd, depth_strata, station_depth_m,
         station_active, station_notes,
         lat_dd, long_dd, depth_m, headrope_depth_m, ctd_index,
         species_group, spp_code, comm_name, sci_name, worms_id, worms_match_type, ls_id, species_notes,
         maturity_code, maturity, catch_n,
         everything())

# Inspect
str(catch)
freeR::complete(catch)

# Export
saveRDS(catch, file=file.path(outdir, "RREAS_catch_data.Rds"))


# Build station key
################################################################################

# Build station key
station_key <- catch %>% 
  select(strata, area, 
         station_id, station_lat_dd, station_long_dd, station_depth_m,
         station_active, station_notes) %>% 
  unique() %>% 
  rename(lat_dd=station_lat_dd,
         long_dd=station_long_dd,
         depth_m=station_depth_m,
         active_yn=station_active,
         notes=station_notes)

# Inspect
freeR::complete(station_key)
freeR::which_duplicated(station_key$station_id)

# Export
saveRDS(station_key, file=file.path(outdir, "RREAS_station_key.Rds"))


# Build species key
################################################################################

# Build species key
species_key <- catch %>% 
  # Unique
 count(species_group, spp_code, comm_name, sci_name, worms_id, worms_match_type, ls_id, species_notes)

# Check for duplicates
freeR::which_duplicated(species_key$spp_code)

# Export
saveRDS(species_key, file=file.path(outdir, "RREAS_species_key.Rds"))


# Build haul key
################################################################################

# Haul key
hauls <- catch %>% 
  count(cruise, haul_num, vessel,
        year, date, date_time,
        strata, area, 
        station_id, station_lat_dd, station_long_dd, depth_strata, station_depth_m,
        station_active, station_notes,
        lat_dd, long_dd, depth_m, headrope_depth_m, ctd_index) %>% 
  # Add haul id
  mutate(haul_id=paste(cruise, haul_num, sep="-")) %>% 
  # Add day of year
  mutate(yday=yday(date)) %>% 
  # Arrange
  select(cruise, haul_num, haul_id, everything())

# Check id
freeR::which_duplicated(hauls$haul_id)

# Export
saveRDS(hauls, file=file.path(outdir, "RREAS_haul_key.Rds"))




# Plot station maps
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
ggplot(station_key, aes(x=long_dd, y=lat_dd, color=strata)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Plot points
  geom_point(data=hauls, shape="x") +
  # Crop
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Theme
  theme_bw()

# Plot data
ggplot(station_key, aes(x=long_dd, y=lat_dd, color=area)) +
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

# Times of year
g1 <- ggplot(hauls, aes(x=yday, y=year, color=strata)) +
  geom_point()

# Times of year
g2 <- ggplot(hauls %>% filter(strata=="Central"), aes(x=yday, y=year, color=strata)) +
  geom_point()

g <- gridExtra::grid.arrange(g1, g2, nrow=1)


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
  group_by(spp_code, comm_name, sci_name, maturity_code) %>% 
  summarize(nyrs=n_distinct(year),
            ntows=n()) %>% 
  # Filter
  filter(nyrs>25 & ntows>250) %>% 
  # Compute percent of tows
  mutate(ptows=ntows/ntows_tot) %>% 
  # Add common name long
  mutate(comm_name_long=ifelse(maturity_code=="A", paste(comm_name, "(adults)"), comm_name))

# Plot data
ggplot(stats, aes(y=reorder(comm_name_long, desc(ptows)), x=ptows, fill=maturity_code)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tows", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw()


