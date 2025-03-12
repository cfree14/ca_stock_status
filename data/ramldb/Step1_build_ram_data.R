
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ramldb"

# Read RAM Legacy Database v4.65
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAMLDB v4.65/R Data/DBdata[asmt][v4.65].RData")


# Build stock key
################################################################################

# Identify most recent assessments
assessments <- assessment %>% 
  # Reduce to most recent
  filter(mostrecent %in% c(999, -1)) %>% 
  # Simplify
  select(stockid, assessid, assessorid, assessmethod, assessyear) %>% 
  # Rename
  rename(assess_range=assessyear) %>% 
  # Extract last year
  mutate(assess_year=substr(assess_range, 6, 10) %>% as.numeric())

# All unique? Yes!
anyDuplicated(assessments$stockid)
anyDuplicated(assessments$assessid)


# Build stock key
stock_key <- stock %>% 
  # Reduce to most recent assessment
  filter(stockid %in% assessments$stockid) %>% 
  # Elimintate useless columns
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Add assessment info
  left_join(assessments, by="stockid") %>% 
  rename(assessor_id=assessorid, assess_method=assessmethod) %>% 
  # Add family name
  left_join(taxonomy %>% select(family, scientificname)) %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=recode(species, 
                        "Chrysophrys auratus"="Pagrus auratus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Etrumeus teres"="Etrumeus sadina",
                        "Loligo bleekeri"="Heterololigo bleekeri",
                        "Loligo pealeii"="Doryteuthis pealeii",
                        "Merluccius gayi"="Merluccius gayi gayi",
                        "Mullus barbatus"="Mullus barbatus barbatus",
                        "Mulloides vanicolensis"="Mulloidichthys vanicolensis",
                        "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Tetrapturus albidus"="Kajikia albida",
                        "Sardinops melanostictus"="Sardinops sagax",
                        "Clupea bentincki"="Strangomera bentincki",
                        "Parupeneus bifasciatus"="Parupeneus trifasciatus",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja rhina"="Beringraja rhina",
                        "Theragra chalcogramma"="Gadus chalcogrammus")) %>% 
  # Rearrange columns
  select(stockid, stocklong, 
         assessid, assessor_id, assess_method,
         country, region, area, 
         family, species, comm_name)

# Check names
freeR::check_names(stock_key$species) # THESE ARE CORRECT: Beringraja binoculata, Beringraja rhina, Gadus chalcogrammus, Loligo reynaudii, Platichthys solemdali, Strangomera bentincki

freeR::complete(stock_key)

# Export stock key
write.csv(stock_key, file=file.path(datadir, "RAM_stock_key.csv"), row.names=F)


# West Coast stocks and data
################################################################################

# Regions
table(stock_key$region)

# West Coast stocks
wc_stocks <- stock_key %>% 
  filter(region%in%c("US West Coast", "Pacific Ocean")) %>% 
  filter(!grepl("South Pacific", stocklong))

# West Coast data
wc_data <- timeseries_values_views %>% 
  # Reduce to WC stocks
  filter(stockid %in% wc_stocks$stockid) %>% 
  # Add meta-data
  left_join(wc_stocks %>% select(stockid, region, area, species, comm_name), by="stockid") %>% 
  mutate(species_label=paste0(comm_name, " (", species, ")")) %>% 
  # Arrange
  select(stockid, stocklong, region, area, species_label, comm_name, species, 
         year, BdivBmsypref, UdivUmsypref) %>% 
  rename(bbmsy=BdivBmsypref, ffmsy=UdivUmsypref) %>% 
  # Delete years where both B/BMSY and F/FMSY are missing
  filter(! (is.na(bbmsy) & is.na(ffmsy)))

# Export data
################################################################################

# Export
saveRDS(wc_data, file=file.path(datadir, "WC_RAM_status_time_series.Rds"))






