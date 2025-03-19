
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(RREAS)
# devtools::install_github("tanyalrogers/RREAS")

# Directories
indir <- "data/rreas/raw"
outdir <- "data/rreas/processed"

# Details
#https://www.fisheries.noaa.gov/inport/item/17408

# Get data
RREAS::load_erddap()


# Format species key
################################################################################

# Format species key
spp_key <- SPECIES_CODES %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(spp_code=species,
         comm_name=common_name) %>% 
  # Format
  mutate(comm_name=stringr::str_to_sentence(comm_name))

freeR::complete(spp_key)

# Unique identifiers
freeR::which_duplicated(spp_key$spp_code)
freeR::which_duplicated(spp_key$sci_name)
freeR::which_duplicated(spp_key$comm_name)

# Inspect
table(spp_key$maturity_codes) # ??????
table(spp_key$species_group)

# Export
saveRDS(spp_key, file=file.path(indir, "RREAS_species_key.Rds"))


# Format hauls
################################################################################

# Format haul key
hauls <- HAULSTANDARD %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(yday=jday,
         lat_dd=latdd,
         long_dd=londd, 
         haul_num=haul_no,
         date_time=haul_date) %>% 
  # Add date
  mutate(date=lubridate::ymd(substr(date_time, 1,10))) %>% 
  # Format strata
  mutate(strata=recode_factor(strata,
                              "S"="South",
                              "SC"="South Central",
                              "C"="Central", 
                              "NC"="North Central", 
                              "N"="North")) %>% 
  # Arrange
  select(survey, cruise, haul_num, 
         year, month, date, date_time, yday,
         strata, area, station, net_in_latdd, net_in_londd, lat_dd, long_dd,
         everything())
  
# Inspect
str(hauls)
freeR::complete(hauls)

# Inspect
table(hauls$cruise)
table(hauls$survey)
table(hauls$strata)
table(hauls$active)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
ggplot(hauls, aes(x=long_dd, y=lat_dd, color=strata)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Crop
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Theme
  theme_bw()

# Plot data
ggplot(hauls, aes(x=long_dd, y=lat_dd, color=area)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Crop
  coord_sf(xlim = c(-125.5, -116.5), ylim = c(32, 42)) +
  # Theme
  theme_bw()

# Times of year
ggplot(hauls, aes(x=yday, y=year, color=strata)) +
  geom_point()

# Times of year
ggplot(hauls %>% filter(strata=="Central"), aes(x=yday, y=year, color=area)) +
  geom_point()


# Format catch
################################################################################

# Format catch
catch <- CATCH %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(spp_code=species,
         haul_num=haul_no, 
         catch_n=total_no) %>% 
  # Add species info
  left_join(spp_key %>% select(spp_code, comm_name, sci_name), by=c("spp_code")) %>% 
  # Add haul info
  left_join(hauls, by=c("cruise", "haul_num")) %>% 
  # Arrange
  select(survey, cruise, haul_num, year:active, 
         spp_code, comm_name, sci_name, catch_n, maturity, everything())

# Inspect
freeR::complete(catch)


# Species
################################################################################

# Nyr
n_distinct(catch$year)
ntows_tot <- hauls %>% 
  # Focus on Central
  filter(strata=="Central") %>% 
  nrow()

# Species
stats <- catch %>% 
  # Focus on Central
  filter(strata=="Central") %>% 
  # Summarize
  group_by(spp_code, comm_name, sci_name) %>% 
  summarize(nyrs=n_distinct(year),
            ntows=n()) %>% 
  # Filter
  filter(nyrs>25 & ntows>300) %>% 
  # Compute percent of tows
  mutate(ptows=ntows/ntows_tot)


