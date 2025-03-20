
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
hauls_orig <- readRDS(file=file.path(outdir, "GBTS_hauls.Rds"))
catch_orig <- readRDS(file=file.path(outdir, "GBTS_catch.Rds"))


# Subset data
################################################################################

# Subset hauls
hauls <- hauls_orig %>% 
  filter(project=="Groundfish Slope and Shelf Combination Survey") %>% 
  filter(performance=="Satisfactory")

# Subset catch
catch <- catch_orig %>% 
  filter(project=="Groundfish Slope and Shelf Combination Survey") %>% 
  filter(performance=="Satisfactory")


# Subset data
################################################################################

# Total number of tows
ntows_tot <- n_distinct(hauls$trawl_id)

# All species
spp_key <- catch %>% 
  # Stats
  group_by(comm_name_orig, sci_name, spp_code_pacfin) %>% 
  summarize(nyrs=n_distinct(year),
            ntows=n_distinct(trawl_id)) %>% 
  # Mark level
  mutate(level=ifelse(freeR::nwords(sci_name)>1, "species", "group")) %>% 
  # Percent of tows
  mutate(ptows=ntows/ntows_tot)

# Species to analyse
spp_key_use <- spp_key %>% 
  filter(level=="species" & !sci_name %in% c("unsorted shab", "Echinoidea (crushed urchin)")) %>% 
  filter(nyrs>=18 & ptows>0.05) %>% 
  mutate(sci_name=recode(sci_name,
                         # "Bathyraja kincaidii"="Raja kincaidii",      
                         "Cancer magister"="Metacarcinus magister",          
                         "Clupea pallasii"="Clupea pallasii pallasii",           
                         # "Crossaster borealis"="",     
                         "Parastichopus californicus"="Apostichopus californicus",
                         "Parastichopus leukothele"="Apostichopus leukothele",
                         "Zoroaster evermanni"="Sagenaster evermanni" ))


# Check names
freeR::check_names(spp_key_use$sci_name)

# Gte taxa
taxa <- freeR::taxa(spp_key_use$sci_name)

# Add taxa
spp_key_use1 <- spp_key_use %>% 
  left_join(taxa, by=c("sci_name"="sciname"))

