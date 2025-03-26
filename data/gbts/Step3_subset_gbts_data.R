
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
plotdir <- "data/gbts/figures"

# Read data
hauls_orig <- readRDS(file=file.path(outdir, "GBTS_hauls.Rds"))
catch_orig <- readRDS(file=file.path(outdir, "GBTS_catch.Rds"))


# Subset data
################################################################################

# Subset hauls
hauls <- hauls_orig %>% 
  # Filter
  filter(project=="Groundfish Slope and Shelf Combination Survey") %>% 
  filter(performance=="Satisfactory") %>% 
  # Add date dummy
  mutate(yday=yday(date),
         pass=case_when(yday<220 & year>=2004 ~ "Summer",
                        yday<230 & year==2003 ~ "Summer",
                        T ~ "Fall") %>% factor(., levels=c("Summer", "Fall")),
         date_dummy=paste("2000", month(date), day(date), sep="-") %>% ymd()) %>% 
  # Arrange
  relocate(pass, .before=date) %>% 
  select(program:date, date_dummy, yday, everything())

# Check pass work
g <- ggplot(hauls, aes(x=yday, y=year, color=pass)) +
  geom_point(size=0.5) +
  # Labels
  labs(x="Day of year", y="Year") +
  scale_y_reverse(breaks=seq(2002, 2026, 2)) +
  # Theme
  theme_bw()
g

# Subset catch
catch <- catch_orig %>% 
  filter(project=="Groundfish Slope and Shelf Combination Survey") %>% 
  filter(performance=="Satisfactory")

# Export data
saveRDS(hauls, file=file.path(outdir, "GBTS_hauls_use.Rds"))
saveRDS(catch, file=file.path(outdir, "GBTS_catch_use.Rds"))


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
                         # "Crossaster borealis"="Crossaster borealis",     
                         "Parastichopus californicus"="Apostichopus californicus",
                         "Parastichopus leukothele"="Apostichopus leukothele",
                         "Zoroaster evermanni"="Sagenaster evermanni" ))


# Check names
freeR::check_names(spp_key_use$sci_name)

# Gte taxa
taxa <- freeR::taxa(spp_key_use$sci_name)

# Add taxa
spp_key_use1 <- spp_key_use %>% 
  left_join(taxa, by=c("sci_name"="sciname")) %>% 
  filter(type=="fish")

# Export
saveRDS(spp_key_use1, file=file.path(outdir, "GBTS_species_to_evaluate.Rds"))


# Species
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(spp_key_use1, aes(x=ptows, y=reorder(comm_name_orig, desc(ptows)))) +
  # facet_grid(order~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x='Percent of tows', y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + base_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_gbts_species.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



# Hauls
################################################################################



g <- ggplot(hauls, aes(x=date, y=lat_dd)) +
  geom_point(size=0.5) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  # Theme
  theme_bw() + base_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_gbts_spatiotemporal_coverage.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


g <- ggplot(hauls, aes(x=date_dummy, y=year, color=pass)) +
  geom_point(size=0.5) +
  # Labels
  labs(x="Day of year", y="Year") +
  scale_y_reverse(breaks=seq(2002, 2026, 2)) +
  # Legend
  scale_color_discrete(name="Pass") +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position="top")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_gbts_seasonality.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

