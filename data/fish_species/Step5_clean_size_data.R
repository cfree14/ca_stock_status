
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/fish_species/allen"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "APPENDIX TABLE 17.A.1.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names() %>% 
  rename(species=sp_sci_name,
         lmax_cm=t_lmax,
         wmax_g=wmax, 
         l50_cm=l50, 
         tmax_yr=amax, 
         a50_yr=a50, 
         k=vb_k, 
         linf_cm=vb_linf,
         lat_dd=n_lat) %>% 
  # Format class
  mutate(class=recode(class, 
                      "Pinon et al 2009;"="Actinopteri")) %>% 
  # Format order
  mutate(order=gsub("\\(", " \\(", order) %>% stringr::str_squish(),
         order=recode(order, 
                      "Perciformes_sedis_mutabilis"="Perciformes sedis mutabilis")) %>% 
  # Format family
  mutate(family=gsub("\\(", " \\(", family),
         family=recode(family,
                       " (Scorpaenidae (Sebastolobinae)"="Scorpaenidae (Sebastolobinae)")) %>% 
  # Format sex
  mutate(sex=toupper(sex),
         sex=recode(sex,
                    "M"="male",
                    "F"="female",
                    "P"="pooled")) %>% 
  # Format species
  mutate(species=gsub("_", " ", species)) %>% 
  mutate(species=recode(species, 
                        "Albula sp A" = "Albula gilberti",                                  
                        "Antennarius avalonis" = "Fowlerichthys avalonis",                            
                        "Apodichthys sanctaerosae" = "Ulvicola sanctaerosae",                      
                        "Asterotheca pentacanthus" = "Bathyagonus pentacanthus",                       
                        "Bathylagus milleri" = "Pseudobathylagus milleri",                              
                        # "Bathyraja kincaidii" = "",                            
                        # "Beringraja binoculata" = "",                           
                        # "Beringraja rhina" = "",                                
                        # "Brachygenys californiensis" = "",                     
                        # "Cephalopholis colonus" = "",                           
                        # "Cheilopogon pinnatibarbatus" = "",                     
                        "Clupea pallasii"  = "Clupea pallasii pallasii",                               
                        "Euthynnus(Katsuwonus)  pelamis"  = "Katsuwonus pelamis",                 
                        "Facciolella equitorialis" = "Facciolella equatorialis",                       
                        # "Gadus chalcogrammus" = "",                            
                        "Galeocerdo cuvieri" = "Galeocerdo cuvier",                              
                        "Hyperprosopon(Hypocritichthys) anale(analis)" = "Hypocritichthys analis",    
                        "Hypsurus(Embiotoca) caryi" = "Hypsurus caryi",                      
                        "Istiompax(Makaira) indica" = "Istiompax indica",                       
                        "Kethetostema averruncus" = "Kathetostoma averruncus",                         
                        # "Lampanyctus ritteri" = "",                            
                        # "Lampris incognitus" = "",                              
                        "Myoxocephalus scorpius(verrucosus)" = "Myoxocephalus scorpius",              
                        "Notorhynchus cepedianus" = "Notorynchus cepedianus",                        
                        # "Paraliparis pelagicus" = "",                           
                        "Percis japonicus" = "Percis japonica",                                
                        # "Phanerodon vacca" = "",                               
                        "Pleuronectes glacialis" = "Liopsetta glacialis",                          
                        "Podothecus acipenserinus" = "Podothecus accipenserinus",                        
                        # "Pseudobatos productus" = "",                          
                        "Sebastes  alutus" = "Sebastes alutus",                                
                        "Sebastes  goodei" = "Sebastes goodei",                                
                        "Sebastes carnatus-chrysomelas" = "Sebastes carnatus, Sebastes chrysomelas",                  
                        "Sebastes miniatus/S. crocotulus" = "Sebastes miniatus, Sebastes crocotulus",                 
                        "Sebastes mystinus-diaconus" = "Sebastes mystinus, Sebastes diaconus",                      
                        "Seriola dorsalis" = "Seriola lalandi",                               
                        "Sphaeroides annulatus" = "Sphoeroides annulatus",                          
                        # "Stichaeus punctatus" = "",                            
                        "Tetrapterus angustirostris" = "Tetrapturus angustirostris",                     
                        "Tetrapterus audax" = "Kajikia audax",                            
                        "Trichiurus nitens" = "Trichiurus lepturus",                               
                        # "Urobatis halleri" = ""
                        )) %>% 
  # Format n lat ???
  mutate(lat_dd=as.numeric(lat_dd)) %>% 
  # Arrange
  select(class, order, family, species, sex:sources)

# Inspect
str(data)

# Species
freeR::check_names(data$species)

# Inspect
table(data$class)
table(data$order)
table(data$family)
table(data$sex)


# Export
saveRDS(data, file=file.path(datadir, "larry_allen_size_age_data.Rds"))






