
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
plotdir <- "data/stock_smart/figures"

# Read data
calcofi_orig <- readRDS("data/calcofi/processed/calcofi_indices_of_abundance.Rds")
stocksmart_orig <- readRDS("data/stock_smart/processed/stock_smart_data_use.Rds")
ccfrp_orig <- readRDS("data/ccfrp/processed/CCFRP_abundance_indices.Rds")
# rreas_orig <- readRDS("data/rreas/processed/rreas_indices_of_abundance.Rds")
# gbts_orig <- readRDS("data/gbts/processed/rreas_indices_of_abundance.Rds")
# scuba_orig <- readRDS("data/scuba/processed/rreas_indices_of_abundance.Rds")


# Prep data
################################################################################

# Prep calcofi - add sci name
calcofi <- calcofi_orig %>% 
  # Add
  mutate(dataset="CalCOFI") %>%
  # Simplify
  select(dataset, comm_name, sci_name, year, index, index_lo, index_hi)

# Prep CCFRP - add sci name
ccfrp <- ccfrp_orig %>% 
  # Add
  mutate(dataset="CCFRP") %>%
  # Simplify
  select(dataset, comm_name, sci_name, year, index, index_lo, index_hi) %>% 
  # Remove species
  filter(!comm_name %in% c("Unid blue rockfish"))


# Prep StockSMART
################################################################################

# Prep StockSMART - add common/scinae
stocksmart1 <- stocksmart_orig %>% 
  # Add
  mutate(dataset="StockSMART") %>%
  # Rename
  rename(index=value) %>% 
  # Simplify
  select(dataset, stock, comm_name, sci_name, year, index)

# Species
stocksmart1_spp <- stocksmart1 %>% 
  filter(!grepl(",", comm_name))

# Complexes
stocksmart1_comp1 <- stocksmart1 %>% 
  filter(grepl(",", comm_name)) %>% 
  mutate(stock=paste(stock, 1), 
         comm_name=paste(comm_name, 1),
         sci_name=paste(sci_name, 1))
stocksmart1_comp2 <- stocksmart1 %>% 
  filter(grepl(",", comm_name)) %>% 
  mutate(stock=paste(stock, 2), 
         comm_name=paste(comm_name, 2),
         sci_name=paste(sci_name, 2))
stocksmart1_comp <- bind_rows(stocksmart1_comp1, stocksmart1_comp2) %>% 
  mutate(comm_name=recode(comm_name,
                          "Gopher rockfish, Black-and-yellow rockfish 1" = "Gopher rockfish",
                          "Vermilion rockfish, Sunset rockfish 1" = "Vermilion rockfish",      
                          "Gopher rockfish, Black-and-yellow rockfish 2" = "Black-and-yellow rockfish",
                          "Vermilion rockfish, Sunset rockfish 2" = "Sunset rockfish"),
         sci_name=recode(sci_name,
                         "Sebastes carnatus, Sebastes chrysomelas 1" = "Sebastes carnatus",
                         "Sebastes miniatus, Sebastes crocotulus 1" = "Sebastes miniatus",
                         "Sebastes carnatus, Sebastes chrysomelas 2" = "Sebastes chrysomelas", 
                         "Sebastes miniatus, Sebastes crocotulus 2" = "Sebastes crocotulus"))

unique(stocksmart1_comp$comm_name)
unique(stocksmart1_comp$sci_name)

# Merge species and complexes
stocksmart2 <- bind_rows(stocksmart1_spp, stocksmart1_comp)


# Merge data
################################################################################

# Merge data
data <- bind_rows(calcofi, ccfrp, stocksmart2) %>% 
  # Arrange
  select(dataset, comm_name, sci_name, stock, year, everything()) %>% 
  # Fill stocks
  mutate(stock=ifelse(is.na(stock), paste(dataset, comm_name), stock)) %>% 
  # Sort
  arrange(dataset, comm_name, year)

# Species key
spp_key <- data %>% 
  count(comm_name, sci_name)

# Make sure unique
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "abundance_indices_merged.Rds"))




