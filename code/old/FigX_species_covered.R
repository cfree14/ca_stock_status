
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "output"

# Read species
smart_orig <- readRDS("data/stock_smart/processed/stock_smart_species_to_evaluate.Rds")
gbts_orig <- readRDS("data/gbts/processed/GBTS_species_to_evaluate.Rds")
calcofi_orig <- read.csv("data/calcofi/processed/calcofi_species_to_evaluate.csv", as.is=T)
rreas_orig <- readRDS("data/rreas/processed/RREAS_species_to_evaluate.Rds")
scuba_orig <- readRDS("data/kelp_scuba/processed/scuba_species_to_evaluate.Rds")
ccfrp_orig <- readRDS("data/ccfrp/processed/CCFRP_species_to_evaluate.Rds")

# Read
spp_list <- read.csv("data/fish_species/raw/miller_lea_1972/ca_fish_species_miller_lea_1972.csv")


# Build data
################################################################################

# Format
smart <- smart_orig %>% 
  select(comm_name, sci_name)
gbts <- gbts_orig %>% 
  select(comm_name_orig, sci_name) %>% 
  rename(comm_name=comm_name_orig)
calcofi <- calcofi_orig %>% 
  select(comm_name, sci_name)
rreas <- rreas_orig %>% 
  select(comm_name, sci_name)
scuba <- scuba_orig %>% 
  select(comm_name, sci_name)
ccfrp <- ccfrp_orig %>% 
  select(comm_name, sci_name)

# Generate list of all species
data <- bind_rows(smart, gbts, calcofi, rreas, scuba, ccfrp) %>% 
  # Recode sci name
  mutate(sci_name=recode(sci_name,
                         "Sebastes dalli"="Sebastes dallii")) %>% 
  # Recode common name
  mutate(comm_name=recode(comm_name, 
                          "Pacific sardine (pilchard)"="Pacific sardine",
                          "Pacific hake or whiting"="Pacific hake",
                          "North pacific hake"="Pacific hake",
                          "Rockfish, other"="Rockfishes",
                          "Black and yellow rockfish"="Black-and-yellow rockfish")) %>% 
  # Make unique
  unique() %>% 
  # Mark which species are in which datasets
  mutate(smart=sci_name %in% smart$sci_name,,
         gbts=sci_name %in% gbts$sci_name,
         calcofi=sci_name %in% calcofi$sci_name,
         rreas=sci_name %in% rreas$sci_name,
         scuba=sci_name %in% scuba$sci_name,
         ccfrp=sci_name %in% ccfrp$sci_name)

# Check
freeR::which_duplicated(data$sci_name)

# Add
taxa <- freeR::taxa(data$sci_name)

# Add taxa
data1 <- data %>% 
  left_join(taxa %>% select(-species), by=c("sci_name"="sciname")) %>% 
  select(type, class, order, family, genus, sci_name, comm_name, everything())


# Any not in master list?
# Raja binoculata" to Beringraja binoculata 
data1$sci_name[!data1$sci_name %in% spp_list$species] %>% sort()

# Export species list
saveRDS(data1, file=file.path(outdir, "species_in_report_card.Rds"))



# Build data
################################################################################

# Data plot
data_plot <- data1 %>% 
  # Gather
  gather(key="survey", value="present_yn", 8:ncol(.)) %>% 
  # Recode survey
  mutate(survey=recode_factor(survey,
                            "smart"="SS",
                             "gbts"= "GBTS",
                             "scuba"= "SCUBA",
                             "calcofi"= "CalCOFI",
                             "rreas"="RREAS",
                             "ccfrp"= "CCFRP"))

order1 <- c("Scorpaeniformes", "Pleuronectiformes")
order2 <- c("Osmeriformes", "", "Myctophiformes", "Stomiiformes", NA)
data_plot1 <- data_plot %>% 
  filter(order %in% order1)
data_plot2 <- data_plot %>% 
  filter(order %in% order2)
data_plot3 <- data_plot %>% 
  filter(!order %in% c(order1, order2))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g1 <- ggplot(data_plot1, aes(x=survey, y=comm_name, fill=present_yn)) +
  facet_grid(order~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Survey", y="") +
  # Legend
  scale_fill_manual(values=c("white", "darkred"), guide="none") +
  # Theme
  theme_bw() + my_theme
g1

g2 <- ggplot(data_plot2, aes(x=survey, y=comm_name, fill=present_yn)) +
  facet_grid(order~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Survey", y="") +
  # Legend
  scale_fill_manual(values=c("white", "darkred"), guide="none") +
  # Theme
  theme_bw() + my_theme
g2

g3 <- ggplot(data_plot3, aes(x=survey, y=comm_name, fill=present_yn)) +
  facet_grid(order~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Survey", y="") +
  # Legend
  scale_fill_manual(values=c("white", "darkred"), guide="none") +
  # Theme
  theme_bw() + my_theme
g3


# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_species_coverage.png"), 
       width=10.5, height=6.5, units="in", dpi=600)

