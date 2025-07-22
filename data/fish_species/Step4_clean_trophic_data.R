
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/fish_species/allen"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Allen Ecofish Trophic Category DB.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names() %>% 
  rename(spp_code=spcode,
         sci_name=scientific_name, 
         comm_name=common_name) %>% 
  # Subset
  select(spp_code:trophic_level) %>% 
  # Format
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format species
  mutate(sci_name=recode(sci_name, 
                        "Abudefduf troscheli" = "Abudefduf troschelii",       
                        "Alepocephalus tenebrosum" = "Alepocephalus tenebrosus",   
                        "Amphisticus argenteus" = "Amphistichus argenteus",       
                        "Amphisticus koelzi" = "Amphistichus koelzi",         
                        "Anisotremus davidsoni" = "Anisotremus davidsonii", 
                        "Bryx arctos" = "Cosmocampus arctus",               
                        "Chirolophus nugator" = "Chirolophis nugator",        
                        "Coryphaena hippus"  = "Coryphaena hippurus",         
                        "Coryphaenoides pectoralis" ="Albatrossia pectoralis", 
                        "Eptatretus stouti" = "Eptatretus stoutii",         
                        "Galeorhinus zyopterus" = "Galeorhinus galeus",     
                        "Heterodontus francisi" ="Heterodontus francisci",      
                        "Hexagrammus decagrammus" = "Hexagrammos decagrammus",    
                        "Hyperprosopon argentuem" = "Hyperprosopon argenteum",   
                        "Isurus oxyrhynchus" = "Isurus oxyrinchus",              
                        "Leicottus hirundo" = "Leiocottus hirundo",         
                        "Medialuna californica" = "Medialuna californiensis",      
                        "Opidion scrippsae"  = "Ophidion scrippsae",        
                        "Plagiogrammus hopkinsi"  = "Plagiogrammus hopkinsii",   
                        "Platyrhinoides triseriatus" = "Platyrhinoidis triseriata", 
                        "Quietula ycauda"="Quietula y-cauda",            
                        "Rhinogobiops nicholsi"  = "Rhinogobiops nicholsii",     
                        "Seabastes melanostomus" = "Sebastes melanostomus",    
                        "Sebastes chlorosticus" = "Sebastes chlorostictus",      
                        "Sebastes dalli" = "Sebastes dallii",         
                        "Spirinchus starski"  = "Spirinchus starksi",       
                        "Tetrapterus audax"   = "Kajikia audax",        
                        "Xenertmus latifrons" = "Xeneretmus latifrons",
                        # New
                        "Amphisticus rhodoterus"="Amphistichus rhodoterus")) %>% 
  # Reduce
  select(-spp_code) %>% 
  unique() %>% 
  # Take highest
  arrange(sci_name, desc(trophic_level)) %>% 
  group_by(sci_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Recode
  mutate(tl_long=recode(trophic_level,
                        "6"="Quaternary carnivores",
                        "5"="Tertiary carnivores",
                        "4"="Secondary carnivores",
                        "3"="Primary carnivores",
                        "2"="Herbivores/omnivores"))



# Inspect
str(data)
freeR::complete(data)
#freeR::which_duplicated(data$spp_code)
freeR::which_duplicated(data$sci_name)
freeR::which_duplicated(data$comm_name)

# Check names
# These names are all correct:
# Lampris megalopsis, Pseudobatos productus, Symphurus atricauda, "Urobatis halleri"      
freeR::check_names(data$sci_name)

# Export
saveRDS(data, file=file.path(datadir, "allen_trophic_levels.Rds"))


