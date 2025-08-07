
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(taxize)

# Directories
datadir <- "data/fish_species/raw/miller_lea_1972"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Miller_Lea_1972_scientific_name_index.xlsx"), sheet=2)


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  filter(is.na(include)) %>% 
  select(-include) %>% 
  # Format species
  rename(species_orig=species)

# All unique?
freeR::which_duplicated(data1$species_orig)

# Check scientific names
freeR::check_names(data1$species_orig)

# Correct name key
correct_names_full <- taxize::gna_verifier(names = data1$species_orig) 
corret_names_simple <- correct_names_full %>% 
  filter(matchType=="Exact" & currentCanonicalFull!="") %>% 
  select(submittedName, currentCanonicalFull) %>% 
  rename(species_orig=submittedName, species=currentCanonicalFull)

# Add corrected names and correct more
data2 <- data1 %>% 
  # Add corrected names
  left_join(corret_names_simple, by=c("species_orig")) %>% 
  # Fill in missing
  mutate(species=ifelse(is.na(species), species_orig, species)) %>% 
  # Correct a few more
  mutate(species=recode(species,
                        "Allormerus elongatus"="Allosmerus elongatus",
                        "Apristurus karnpae"="Apristurus kampae",
                        "Argyropelecus lychrtus" = "Argyropelecus lychnus",
                        "Desmodema polysticta"  = "Desmodema polystictum",
                        "Enophrys terrurina"="Enophrys taurina",
                        "Hermosillo azurea" = "Kyphosus azureus",
                        "Inopsetta ischyra" = "Parophrys vetulus",
                        "Plagiogrammus hopkinsi"  = "Plagiogrammus hopkinsii", 
                        "Psammobatis spinosissima" = "Bathyraja spinosissima",
                        "Radulinus cuprellus" = "Radulinus asprellus"))

# Check scientific names
freeR::check_names(data2$species)

# Any duplicated?
freeR::which_duplicated(data2$species)

# Get taxa
taxa <- freeR::taxa(data2$species)

# Add taxa
data3 <- data2 %>% 
  # Add taxa
  left_join(taxa %>% select(-c(type, species)), by=c("species"="sciname")) %>% 
  # Arrange
  arrange(class, order, family, genus, species)

# Look up common names
comm_names <- freeR::fb_comm_name(data3$species)

# Add common names
data4 <- data3 %>% 
  # Add common names
  left_join(comm_names %>% select(-source), by="species") %>% 
  # Fill missing common names
  mutate(comm_name=case_when(species_orig=="Manta hamiltoni" ~ "Pacific manta",
                             species_orig=="Lampanyctus ritteri" ~ "Broadfin lampfish",
                             species_orig=="Hermosillo azurea" ~ "Zebraperch",
                             species_orig=="Symphurus atricauda" ~ "California tonguefish",
                             species_orig=="Urolophus halleri" ~ "Round stingray",
                             species_orig=="Raja inornata" ~ "California skate",
                             species_orig=="Raja rhina" ~ "Longnose skate",
                             species_orig=="Raja stellulata" ~ "Starry skate",
                             species_orig=="Anchoviella miarcha" ~ "Slim anchovy",
                             species_orig=="Enophrys terrurina" ~ "Bull sculpin",
                             species_orig=="Ganoideus vulsus" ~ "Beardless spearnose",
                             species_orig=="Lepidocybuim flavobrunneum" ~ "Escolar",
                             species_orig=="Sebastes vexillaris" ~ "Whitebelly rockfish",
                             T ~ comm_name)) %>% 
  # Add missing genera
  mutate(genus=ifelse(is.na(genus), gsub(" .*", "", species), genus)) %>% 
  # Fill missing taxa based on genus
  group_by(genus) %>% 
  fill(class:family, .direction = "updown") %>% 
  ungroup() %>% 
  # Fill missing families
  mutate(family=case_when(species=="Anchoviella miarcha" ~ "Engraulidae",
                          species=="Beringraja binoculata" ~ "Rajidae",
                          species=="Ganoideus vulsus" ~ "Agonidae",
                          species=="Hypanus dipterurus" ~ "Dasyatidae",
                          species=="Hysterocarpus traskii" ~ "Embiotocidae",
                          species=="Kyphosus azureus" ~ "Kyphosidae", 
                          species=="Lepidocybuim flavobrunneum" ~ "Gempylidae",
                          species=="Pseudobatos productus" ~ "Rhinobatidae",
                          species=="Tetronarce californica" ~ "Torpedinidae",
                          T ~ family)) %>% 
  # Fill missing taxa based on genus
  group_by(family) %>% 
  fill(class:order, .direction = "updown") %>% 
  ungroup() %>% 
  # Fill missing orders and classes
  mutate(order=case_when(species=="Tetronarce californica" ~ "Torpediniformes",
                          T ~ order),
         class=case_when(species=="Tetronarce californica" ~ "Elasmobranchii",
                         T ~ class)) %>% 
  # Reduce
  select(-species_orig) %>% 
  unique() %>% 
  # Format some common names
  mutate(comm_name=case_when(species=="Rathbunella alleni" ~ "Rough ronquil",
                             species=="Rathbunella hypoplecta" ~ "Smooth ronquil",
                             T ~ comm_name)) %>% 
  # Remove whitebelly rock fish (Sebastes vexillaris) = same as copper
  filter(species!="Sebastes vexillaris") %>% 
  # Arrange
  select(comm_name, species, everything())

# Inspect
freeR::complete(data4)

# Unique?
freeR::which_duplicated(data4$species)
freeR::which_duplicated(data4$comm_name)

# Check scientific names
freeR::check_names(data4$species)

# Export
write.csv(data4, file=file.path(datadir, "ca_fish_species_miller_lea_1972.csv"), row.names=F)
  

# Plot data
################################################################################


stats_order <- data4 %>% 
  count(order)
stats_family <- data4 %>% 
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

