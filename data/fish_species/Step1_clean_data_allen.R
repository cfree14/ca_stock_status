
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/fish_species/raw/allen"
outdir <- "data/fish_species_processed"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "California Marine SPCODEs_LAllen.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name=common_name,
         species=scientific_name) %>% 
  # Remove not species
  filter(!species %in% c("Sebastes YOY")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  mutate(comm_name=case_when(species=="Zapteryx exasperata" ~ "Banded guitarfish",
                             T ~ comm_name)) %>% 
  # Format species
  mutate(species=recode(species, 
                        "Abudefduf troscheli" = "Abudefduf troschelii",       
                        "Alepocephalus tenebrosum" = "Alepocephalus tenebrosus",   
                        "Amphisticus argenteus" = "Amphistichus argenteus",       
                        "Amphisticus koelzi" = "Amphistichus koelzi",         
                        "Anisotremus davidsoni" = "Anisotremus davidsonii",       
                        # "Bodianus pulcher" = "",            
                        # "Brachygenys californiensis"="",  
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
                        # "Kyphosus azureus"  = "",          
                        # "Lampris megalopsis" ="",        
                        "Leicottus hirundo" = "Leiocottus hirundo",         
                        "Medialuna californica" = "Medialuna californiensis",      
                        "Opidion scrippsae"  = "Ophidion scrippsae",        
                        "Plagiogrammus hopkinsi"  = "Plagiogrammus hopkinsii",   
                        "Platyrhinoides triseriatus" = "Platyrhinoidis triseriata", 
                        # "Pseudobatos productus" = "",    
                        "Quietula ycauda"="Quietula y-cauda",            
                        "Rhinogobiops nicholsi"  = "Rhinogobiops nicholsii",     
                        "Seabastes melanostomus" = "Sebastes melanostomus",    
                        "Sebastes chlorosticus" = "Sebastes chlorostictus",      
                        "Sebastes dalli" = "Sebastes dallii",         
                        "Spirinchus starski"  = "Spirinchus starksi",       
                        # "Symphurus atricauda"  = "",      
                        "Tetrapterus audax"   = "Kajikia audax",        
                        # "Urobatis halleri" = "",           
                        "Xenertmus latifrons" = "Xeneretmus latifrons"))

# Check scientific names
# All correct: Bodianus pulcher Brachygenys californiensis Kyphosus azureus, Lampris megalopsis, Pseudobatos productus, Symphurus atricauda, Urobatis halleri 
freeR::check_names(data$species)

# Any duplicated?
freeR::which_duplicated(data$species)

# Get taxa
taxa <- freeR::taxa(data$species)

# Add taxa
data1 <- data %>% 
  # Remove family
  select(-family) %>% 
  # Add taxa
  left_join(taxa %>% select(-c(type, species)), by=c("species"="sciname")) %>% 
  # Mark whether missing tax
  mutate(taxa_yn=ifelse(is.na(order), "no", "yes")) %>% 
  # Arrange
  arrange(class, order, family, genus, species)

# Export
write.csv(data1, file=file.path(datadir, "Allen_ca_fish_species_temp.csv"), row.names=F)
  
  
  

# Format data
################################################################################

data2 <- readxl::read_excel(file.path(datadir, "Allen_ca_fish_species_final.xlsx"))

stats_order <- data2 %>% 
  count(order)
stats_family <- data2 %>% 
  count(order, family)

ggplot(stats_order, aes(x=n, y=reorder(order, desc(n)))) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="# of species", y="", tag="A") +
  # Theme
  theme_bw()

ggplot(stats_family, aes(x=n, y=reorder(family, desc(n)))) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="# of species", y="", tag="B") +
  # Theme
  theme_bw()


