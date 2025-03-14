
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/calcofi/processed"
outdir <- "data/calcofi/output"
plotdir <- "data/calcofi/figures"

# Export
data_orig <- readRDS(file=file.path(datadir, "calcofi_indices_of_abundance.Rds"))
trends_orig <- readRDS(file=file.path(datadir, "calcofi_index_trends.Rds"))

# Read species key
spp_key <- read.csv(file.path(datadir, "calcofi_species_key.csv"), as.is=T) %>% 
  mutate(comm_name=recode(comm_name, 
                        "Pacific hake or whiting"="Pacific hake",
                        "Pacific mackerel (chub mackerel)"="Pacific mackerel",
                        "Pacific sardine (pilchard)"="Pacific sardine",
                        "Unidentified Teliost"="Unidentified teleost"))


# Build data
################################################################################

# Trends
trends <- trends_orig %>% 
  select(species, slope, pvalue) %>% 
  unique() %>% 
  # Mark significance and direction
  mutate(dir=ifelse(slope<0, "decreasing", "increasing"), 
         sig_yn=ifelse(pvalue<0.05, "yes", "no")) %>% 
  # Create category
  mutate(catg=paste(sig_yn, dir, sep="-"),
         catg=recode_factor(catg, 
                            "yes-increasing"="Increase",
                            "no-increasing"="Increase (non-sig)",
                            "no-decreasing"="Decrease (non-sig)",
                            "yes-decreasing"="Decrease")) %>% 
  # Add species
  rename(comm_name=species) %>% 
  left_join(spp_key, by=c("comm_name"))

# Look up taxa
freeR::check_names(trends$sci_name)
taxa <- freeR::taxa(trends$sci_name)

# Add order to trends
trends1 <- trends %>% 
  # Add order
  left_join(taxa %>% select(sciname, order), by=c("sci_name"="sciname")) %>% 
  # Fill missing order
  mutate(order=case_when(
                         sci_name == "Citharichthys" ~ "Carangiformes",       
                         sci_name == "Cyclothone" ~ "Stomiiformes",           
                         sci_name == "Diaphus" ~ "Myctophiformes",              
                         sci_name == "Lampanyctus" ~ "Myctophiformes",          
                         sci_name == "Lampanyctus ritteri" ~ "Myctophiformes",    
                         sci_name == "Melamphaes" ~ "Beryciformes",           
                         sci_name == "Microstoma" ~ "Argentiniformes",            
                         sci_name == "Myctophidae" ~ "Myctophiformes",          
                         sci_name == "Sebastes" ~ "Perciformes",             
                         sci_name == "Sternoptyx" ~ "Stomiiformes",            
                         sci_name == "Teleostei" ~ NA,   
                         T ~ order))


 # Order stats
stats_order <- trends1 %>% 
  filter(!is.na(order)) %>% 
  count(order, catg, sig_yn, dir) %>% 
  group_by(order) %>% 
  mutate(p=n/sum(n)) %>%
  ungroup()

# Overall stats
stats_tot <- trends1 %>% 
  count(catg, sig_yn, dir) %>% 
  mutate(p=n/sum(n)) %>% 
  mutate(order="Overall")

# Merge
stats <- bind_rows(stats_order, stats_tot)

# N stats
nstats <- stats %>% 
  group_by(order) %>% 
  summarize(n=sum(n)) %>% 
  arrange(desc(n))

# Order stats
stats_ordered <- stats %>% 
  mutate(order=factor(order, rev(nstats$order)))

fl <- freeR::fishlife(trends1$sci_name)

ggplot(fl, aes(x=tmax_yr)) +
  geom_histogram(binwidth = 1) +
  # Label
  labs(x="Maximum age (yr)", y="Number of species") +
  # Theme
  theme_bw()

ggplot(fl, aes(x=g_yr)) +
  geom_histogram(binwidth = 1) +
  # Label
  labs(x="Generation time (yr)", y="Number of species") +
  # Theme
  theme_bw()


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats_ordered, aes(y=order, x=p, fill=catg)) +
  geom_bar(stat="identity") +
  geom_text(data=nstats, mapping=aes(y=order, label=paste0("n=", n)), 
            inherit.aes=F, x=1.01, hjust=0, size=2.5, color="grey50") +
  # Theme
  labs(x="Percent of species", y="") +
  scale_x_continuous(labels=scales::percent_format(), lim=c(0, 1.02)) +
  # Legend
  scale_fill_manual(name="Trend", values=RColorBrewer::brewer.pal(4, "RdBu") %>% rev(),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export figure
ggsave(g, filename=file.path(plotdir,"FigX_calcofi_report_card.png"),
       width=6.5, height=3.5, units="in", dpi=600)


# Examples
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   axis.title.x = element_blank(),
                   axis.text.y=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Decline
spp1 <- "Pacific sardine"
g1 <- ggplot(data_orig %>% filter(species==spp1),
       aes(x=year, y=index)) +
  # Time series
  geom_ribbon(mapping=aes(ymin=index_lo, ymax=index_hi), fill="grey85") +
  geom_line() +
  # Recent trend
  geom_line(data=trends_orig %>% filter(species==spp1), color="red", linewidth=1) +
  # Labels
  labs(x="", y="Index of abundance", title=spp1) +
  scale_x_continuous(breaks=seq(1980,2030,5)) +
  # Theme
  theme_bw() + my_theme
g1

# Neutral
spp2 <- "Dusky pencilsmelt"
g2 <- ggplot(data_orig %>% filter(species==spp2),
             aes(x=year, y=index)) +
  # Time series
  geom_ribbon(mapping=aes(ymin=index_lo, ymax=index_hi), fill="grey85") +
  geom_line() +
  # Recent trend
  geom_line(data=trends_orig %>% filter(species==spp2), color="cyan2", linewidth=1) +
  # Labels
  labs(x="", y="Index of abundance", title=spp2) +
  scale_x_continuous(breaks=seq(1980,2030,5)) +
  # Theme
  theme_bw() + my_theme
g2

# Neutral
spp3 <- "Northern anchovy"
g3 <- ggplot(data_orig %>% filter(species==spp3),
             aes(x=year, y=index)) +
  # Time series
  geom_ribbon(mapping=aes(ymin=index_lo, ymax=index_hi), fill="grey85") +
  geom_line() +
  # Recent trend
  geom_line(data=trends_orig %>% filter(species==spp3), color="blue", linewidth=1) +
  # Labels
  labs(x="", y="Index of abundance", title=spp3) +
  scale_x_continuous(breaks=seq(1980,2030,5)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3)
g

# Export figure
ggsave(g, filename=file.path(plotdir,"FigX_calcofi_report_card_examples.png"),
       width=3, height=5.5, units="in", dpi=600)

