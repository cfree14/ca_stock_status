
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)

# Directories
indir <- "data/mgmt_plans/raw"
outdir <- "data/mgmt_plans"

# Read data
ca_orig <- readxl::read_excel(file.path(indir, "ca_esr_species.xlsx"))
pfmc_cps <- readxl::read_excel(file.path(indir, "PFMC_species.xlsx"), sheet="CPS")
pfmc_gf <- readxl::read_excel(file.path(indir, "PFMC_species.xlsx"), sheet="Groundfish")
pfmc_salmon <- readxl::read_excel(file.path(indir, "PFMC_species.xlsx"), sheet="Salmon")
pfmc_hms <- readxl::read_excel(file.path(indir, "PFMC_species.xlsx"), sheet="HMS")


# Build data
################################################################################

# CPS
cps <- pfmc_cps %>%
  # Filter
  filter(type=="Target") %>% 
  # Add
  mutate(authority="Federal",
         fmp="PFMC Coastal Pelagic Species") %>% 
  # Arrange
  select(authority, fmp, comm_name, species)

# Groundfish
gf <- pfmc_gf %>%
  # Filter
  filter(type=="Target") %>% 
  # Add
  mutate(authority="Federal",
         fmp="PFMC Groundfish") %>% 
  # Arrange
  select(authority, fmp, comm_name, species)

# Salmon
salmon <- pfmc_salmon %>%
  # Filter
  filter(type=="Target") %>% 
  # Add
  mutate(authority="Federal",
         fmp="PFMC Salmon") %>% 
  # Arrange
  select(authority, fmp, comm_name, species)

# Salmon
hms <- pfmc_hms %>%
  # Filter
  filter(type=="Target") %>% 
  # Add
  mutate(authority="Federal",
         fmp="PFMC Highly Migratory Species") %>% 
  # Arrange
  select(authority, fmp, comm_name, species)

# Merge PFMC data
pfmc <- bind_rows(gf, cps, hms, salmon)
freeR::which_duplicated(pfmc$species)
freeR::which_duplicated(pfmc$comm_name)
freeR::check_names(pfmc$species)

# CA
ca <- ca_orig %>% 
  mutate(authority="State") %>% 
  rename(fmp=ca_mgmt_type, species=sci_name) %>% 
  select(authority, fmp, comm_name, species) %>% 
  mutate(fmp=recode(fmp,
                    "ESR" = "CDFW Enhanced Status Report",                              
                    "ESR, California Spiny Lobster FMP" = "CDFW California Spiny Lobster", 
                    "ESR, Market Squid FMP" = "CDFW Market Squid",            
                    "ESR, Nearshore FMP"  = "CDFW Nearshore",               
                    "ESR, Pink (Ocean) Shrimp FMP" = "CDFW Pink (Ocean) Shrimp",     
                    "ESR, White Seabass FMP" = "CDFW White Seabass",          
                    "Nearshore FMP" = "CDFW Nearshore",                     
                    "Pacific Herring FMP" = "CDFW Pacific Herring",              
                    "Red Abalone FMP" = "CDFW Red Abalone")) %>% 
  # Format species names
  mutate(species = recode(species,
                          "Bodianus pulcher" = "",    
                          "Clupea pallasii"  = "Clupea pallasii pallasii",     
                          # "Metacarcinus anthonyi"  = "",
                          # "Romaleon antennarium" = "",  
                          "Seriola dorsalis"  = "Seriola lalandi")) %>% 
  # Remove federally managed species
  filter(!species %in% pfmc$species)

# Some species are both state/federal managed
ca$species[ca$species %in% pfmc$species]

# Check
freeR::check_names(ca$species)

# Merge data
data <- bind_rows(ca, pfmc)

count(data, fmp)

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "spp_mgmt_key.Rds"))



