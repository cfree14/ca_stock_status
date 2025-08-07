
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/fish_species/raw/allen"
datadir1 <- "data/fish_species/raw/miller_lea_1972"
outdir <- "data/fish_species/processed"

# Read data
spp_miller <- read.csv(file=file.path(datadir1, "ca_fish_species_miller_lea_1972.csv"), as.is=T)
spp_allen <- readxl::read_excel(file.path(datadir, "Allen_ca_fish_species_final.xlsx"))
spp_other <- readxl::read_excel(file.path(datadir1, "ca_species_not_in_miller_lea_1972.xlsx"))
tl_orig <- readRDS(file=file.path(datadir, "allen_trophic_levels.Rds"))
size_orig <- readRDS(file=file.path(datadir, "larry_allen_size_age_data.Rds"))


# Merge species
################################################################################

# Which Allen species aren't in Miller and Lea?
spp_allen_use <- spp_allen %>% 
  filter(!species %in% spp_miller$species)

# Which species from monitoring data?
spp_other_use <- spp_other %>% 
  filter(species %in% c(spp_miller$species, spp_allen$species))

# Build species data
spp <- bind_rows(spp_miller, spp_allen_use, spp_other_use) %>% 
  # Simplify
  select(comm_name:genus) %>% 
  # Remove duplicates
  unique() %>% 
  # Remove duplicated scientific names
  filter(!comm_name %in% c("Spiny dogfish")) %>% 
  # Remove duplicated common names
  filter(!species %in% c("Semicossyphus pulcher", "Clupea harengus", "Raja binoculata", "Ganoideus vulsus",
                         "Torpedo californica", "Symphurus atricaudus", "Urolophus halleri")) %>% 
  # Recode some common name
  mutate(comm_name=case_when(species=="Lampris megalopsis" ~ "Bigeye opah",
                             species=="Lampris guttatus" ~ "Opah",
                             T ~ comm_name)) %>% 
  # Recode some scientific
  mutate(species=recode(species, 
                        "Lepidocybuim flavobrunneum"="Lepidocybium flavobrunneum"))

# Make sure unique
freeR::which_duplicated(spp$species)
freeR::which_duplicated(spp$comm_name) 

# Check names *all good
#' "Anchoviella miarcha"        "Beringraja binoculata"      "Bodianus pulcher"           "Brachygenys californiensis"
#' "Gadus chalcogrammus"        "Ganoideus vulsus"           "Hypanus dipterurus"         "Hysterocarpus traskii"     
#' "Kyphosus azureus"           "Lampris megalopsis"         "Mobula birostris"           "Pseudobatos productus"     
#' "Symphurus atricauda"        "Tetronarce californica"     "Urobatis halleri"
freeR::check_names(spp$species)


# Merge
################################################################################

# Prep age/size
size <- size_orig %>%
  filter(!is.na(linf_cm)) %>% 
  group_by(species) %>%
  summarize(linf_cm=max(linf_cm, na.rm=T)) %>%
  ungroup()
age <- size_orig %>%
  filter(!is.na(tmax_yr)) %>% 
  group_by(species) %>%
  summarize(tmax_yr=max(tmax_yr, na.rm=T)) %>%
  ungroup()


# Make FishLife predictions
fl <- freeR::fishlife(data$species)

# Get FishBase data
fb <- freeR::fishbase(species=spp$species, dataset="species", cleaned=T)
fb_use <- fb %>% 
  filter(database=="FishBase") %>% 
  unique()

# Merge data
data <- spp %>% 
  # Add trophic level
  left_join(tl_orig %>% select(-comm_name), by=c("species"="sci_name")) %>% 
  # Add FishBase info
  left_join(fb_use %>% select(species, habitat), by="species") %>% 
  # Fill missing habitats
  mutate(habitat=case_when(species=="Nannobrachium ritteri" ~ "bathypelagic",   #  Lampanyctus ritteri
                           species=="Raja inornata" ~ "demersal",   # Caliraja inornata
                           species=="Raja rhina" ~ "bathydemersal",   # Caliraja rhina 
                           species=="Raja stellulata"  ~ "demersal", # Caliraja stellulata   
                           species=="Anchoviella miarcha"  ~ "pelagic-neritic", # based on family   
                           species=="Gadus chalcogrammus" ~ "benthopelagic",  
                           species=="Brachygenys californiensis" ~ "demersal",   
                           species=="Bodianus pulcher"  ~ "reef-associated",   
                           species=="Lampris megalopsis"   ~ "bathypelagic",   
                           species=="Urolophus maculatus" ~ "demersal", # Urobatis maculatus
                           TRUE ~ habitat)) %>% 
  # Add Allen age/size
  left_join(size, by="species") %>% 
  left_join(age, by="species") %>% 
  rename(linf_cm_allen=linf_cm,
         tmax_yr_allen=tmax_yr) %>% 
  # Add FishLife age/size
  left_join(fl %>% select(sci_name, linf_cm, tmax_yr), by=c("species"="sci_name")) %>% 
  rename(linf_cm_fl=linf_cm,
         tmax_yr_fl=tmax_yr) %>% 
  # Finalize age/size
  mutate(linf_cm=ifelse(!is.na(linf_cm_allen), linf_cm_allen, linf_cm_fl),
         tmax_yr=ifelse(!is.na(tmax_yr_allen), tmax_yr_allen, tmax_yr_fl))

# Inspect
freeR::complete(data)

data$species[is.na(data$habitat)]

# Export
saveRDS(data, file=file.path(outdir, "ca_finfish_species.Rds"))


