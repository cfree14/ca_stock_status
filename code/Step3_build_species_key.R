
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(mblm)
library(zyp)

# Directories
datadir <- "data/merged"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "abundance_indices_merged.Rds"))

# Read trophic level
tls <- readRDS("data/fish_species/raw/allen/allen_trophic_levels.Rds")

# Read species mgmt key
mgmt <- readRDS("data/mgmt_plans/spp_mgmt_key.Rds")

# Read harvest key
harvest <- readRDS("data/fish_species/processed/ca_finfish_annual_harvest.Rds")

# Build assessed species
assessed_spp <- data_orig %>% 
  filter(dataset=="StockSMART") %>% 
  pull(sci_name) %>% unique()


# Build data
################################################################################

# Species key
spp_key <- data_orig %>% 
  select(comm_name, sci_name) %>% 
  unique()

# Get taxa
taxa <- freeR::taxa(spp_key$sci_name)

# FishBase
fb <- freeR::fishbase(species=spp_key$sci_name, dataset="species", cleaned=T)
fb_use <- fb %>% 
  filter(database=="FishBase") %>% 
  unique()

# FishLife
fl <- freeR::fishlife(spp_key$sci_name)

# Add taxa to species key
spp_key1 <- spp_key %>% 
  # Add taxa info
  left_join(taxa %>% select(class:genus, sciname), by=c("sci_name"="sciname")) %>% 
  # Add missing genus
  mutate(genus=ifelse(is.na(genus), stringr::word(sci_name, 1), genus)) %>% 
  # Fill missing taxa by genus
  group_by(genus) %>% 
  fill(class:family, .direction = "updown") %>% 
  ungroup() %>% 
  # Fill missing family
  mutate(family=ifelse(sci_name=="Lampanyctus ritteri", "Myctophidae", family)) %>% 
  group_by(family) %>% 
  fill(class:order, .direction = "updown") %>% 
  ungroup() 

# Check
freeR::complete(spp_key1)

# Add other things
spp_key2 <- spp_key1 %>% 
  # Add trophic level
  left_join(tls %>% select(sci_name, trophic_level, tl_long), by=c("sci_name")) %>% 
  rename(tl=trophic_level) %>% 
  mutate(tl_long=ifelse(is.na(tl_long), "Unknown", tl_long)) %>% 
  # Add managed?
  mutate(managed_yn=ifelse(sci_name %in% mgmt$species, "Managed", "Not managed")) %>% 
  # Add assessed?
  mutate(assessed_yn=ifelse(sci_name %in% assessed_spp, "Assessed", "Not assessed")) %>% 
  # Add habitat
  left_join(fb_use %>% select(species, habitat), by=c("sci_name"="species")) %>% 
  # Fill missing habitats
  mutate(habitat=case_when(sci_name=="Bathyraja kincaidii" ~ "bathydemersal",
                           sci_name=="Phanerodon vacca" ~ "demersal",
                           sci_name=="Raja inornata" ~ "demersal", # Caliraja inornata on FishBase
                           sci_name=="Raja rhina" ~ "bathydemersal", # Caliraja rhina on FishBase
                           sci_name=="Sebastes crocotulus" ~ "reef-associated", # because similar to vermillion (Sebastes miniatus)
                           T ~ habitat)) %>% 
  # Add tmax and linf
  left_join(fl %>% select(sci_name, tmax_yr, linf_cm), by="sci_name") %>% 
  # Fill missing tmax
  mutate(tmax_yr=case_when(sci_name=="Lampanyctus ritteri" ~ 5.5, # FB max age 
                           sci_name=="Bathyraja kincaidii" ~ 18, # FB max age
                           sci_name=="Phanerodon vacca" ~ 44.2, # FB max length  
                           sci_name=="Sebastes crocotulus" ~ 45.16273, # because similar to vermillion (Sebastes miniatus)
                           T ~ tmax_yr)) %>%
  # Fill missing linf
  mutate(linf_cm=case_when(sci_name=="Lampanyctus ritteri" ~ 12, # FB max length
                           sci_name=="Bathyraja kincaidii" ~ 60, # FB average m/f max length
                           sci_name=="Phanerodon vacca" ~ 44.2, # FB max length  
                           sci_name=="Sebastes crocotulus" ~ 39.14426, # because similar to vermillion (Sebastes miniatus)
                             T ~ linf_cm)) %>%
  # Add harvest stats
  left_join(harvest %>% select(sci_name, landings_lbs), by="sci_name") %>% 
  mutate(landings_lbs=ifelse(is.na(landings_lbs), 0, landings_lbs))

# Check
freeR::complete(spp_key2)

# Export
saveRDS(spp_key2, file=file.path(datadir, "species_key.Rds"))




