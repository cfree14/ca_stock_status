
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(RREAS)

# Directories
indir <- "data/gbts/raw"
outdir <- "data/gbts/processed"

# Read data
hauls_orig <- read.csv(file.path(indir, "hauls.csv"), as.is=T, na.strings=c("", " "))
catch_orig <- read.csv(file.path(indir, "catch.csv"), as.is=T, na.strings=c("", " ", "NA"))

# Data are from here
# https://www.webapps.nwfsc.noaa.gov/data/map
# https://www.fisheries.noaa.gov/inport/item/18418


# Format hauls
################################################################################

# Format data
hauls <- hauls_orig %>% 
  # Rename
  rename(area_swept_ha=area_swept_ha_der,
         date=date_yyyymmdd,
         depth_type=mean_seafloor_dep_position_type,
         depth_m=depth_hi_prec_m,
         inverts_kg=invertebrate_weight_kg,
         fish_kg=vertebrate_weight_kg,
         organics_kg=nonspecific_organics_weight_kg,
         lat_dd=latitude_hi_prec_dd,
         long_dd=longitude_hi_prec_dd,
         duration_hr=sample_duration_hr_der,
         sampling_time1=sampling_start_hhmmss,
         sampling_time2=sampling_end_hhmmss,
         tow_time1=tow_start_timestamp,
         tow_time2=tow_end_timestamp, 
         station_id=station_code, 
         position_type=midtow_position_type) %>% 
  # Format
  mutate(date=ymd(date)) %>% 
  # Arrange
  select(program, project, year, date,
         vessel_id, vessel,
         sampling_time1, sampling_time2, tow_time1, tow_time2, duration_hr, 
         station_id, position_type, lat_dd, long_dd, depth_type, depth_m, area_swept_ha, performance, trawl_id,
         fish_kg, inverts_kg, organics_kg,
         everything())

# Inspect
str(hauls)

range(hauls$date, na.rm=T)

table(hauls$program)
table(hauls$project)
         
table(hauls$depth_type)
table(hauls$position_type)

# Export
saveRDS(hauls, file=file.path(outdir, "GBTS_hauls.Rds"))


# Format catch
################################################################################

# Format catch
catch <- catch_orig %>% 
  # Rename
  rename(comm_name_orig=common_name,
         cpue_kg_ha=cpue_kg_per_ha_der,
         cpue_n_ha=cpue_numbers_per_ha_der,
         date=date_yyyymmdd,
         lat_dd=latitude_dd,
         long_dd=longitude_dd,
         spp_code_pacfin=pacfin_spid,
         sampling_time2=sampling_end_hhmmss,
         sampling_time1=sampling_start_hhmmss,
         sci_name=scientific_name,
         station_id=station_code,
         subsample_n=subsample_count,
         subsample_kg=subsample_wt_kg,
         total_catch_n=total_catch_numbers,
         total_catch_kg=total_catch_wt_kg,
         tow_time2=tow_end_timestamp,
         tow_time1=tow_start_timestamp,
         notes=partition) %>% 
  # Format date
  mutate(date=ymd(date), 
         year=year(date)) %>% 
  # Format common name
  mutate(comm_name_orig=ifelse(is.na(comm_name_orig), sci_name, comm_name_orig),
         comm_name_orig=stringr::str_to_sentence(comm_name_orig)) %>% 
  # Arrange
  select(program, project, year, date,
         vessel_id, vessel,
         station_id, trawl_id, performance, lat_dd, long_dd, depth_m,
         sampling_time1, sampling_time2, tow_time1, tow_time2,
         catch_id, comm_name_orig, sci_name, spp_code_pacfin, 
         subsample_n, subsample_kg, total_catch_n, total_catch_kg, cpue_n_ha, cpue_kg_ha, notes,
         everything())

# Inspect
str(catch)

# Export
saveRDS(catch, file=file.path(outdir, "GBTS_catch.Rds"))



# Species key
################################################################################

# Species key
spp_key <- catch %>% 
  # Unique species
  count(comm_name_orig, sci_name, spp_code_pacfin) %>% 
  # Mark level
  mutate(level=ifelse(freeR::nwords(sci_name)>1, "species", "group")) 
  # Correct scientific names

freeR::which_duplicated(spp_key$spp_code_pacfin)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)

# Check species names
freeR::check_names(spp_key$sci_name[spp_key$level=="species"])

# Export
write.csv(spp_key, file=file.path(outdir, "gbts_species_key_but_needs_much_work.csv"), row.names = F)







