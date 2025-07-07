
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)

# Directories
datadir <- "data/calcofi/processed"
outdir <- "data/calcofi/output"
plotdir <- "data/calcofi/figures"

# Species key
spp_key <- read.csv(file.path(datadir, "calcofi_species_to_evaluate.csv"))

# Merge data
################################################################################

#  Files 2 merge
files2merge <- list.files(outdir)

# Merge files
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  output <- readRDS(file.path(outdir, x))
  
  # Get index
  index <- output[[3]]
  
})

# Clean up
data <- data_orig %>% 
  rename(comm_name=species) %>% 
  left_join(spp_key %>% select(comm_name, sci_name, taxa_type)) %>% 
  mutate(comm_name=recode(comm_name, 
                        "Pacific hake or whiting"="Pacific hake",
                        "Pacific mackerel (chub mackerel)"="Pacific mackerel",
                        "Pacific sardine (pilchard)"="Pacific sardine",
                        "Unidentified Teliost"="Unidentified teleost")) %>% 
  select(comm_name, sci_name, everything()) %>% 
  filter(sci_name %in% spp_key$sci_name)

freeR::complete(data)

# Export
saveRDS(data, file=file.path(datadir, "calcofi_indices_of_abundance.Rds"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.background = element_blank(),
                   panel.border = element_blank(),
                   strip.text=element_text(size=5.5, margin = margin(t = 1, r = 1, b = 1, l = 1)),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.line.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=index/1e6)) +
  facet_wrap(~species, scales="free_y", ncol=8, labeller = label_wrap_gen(width=10)) +
  # CI
  geom_ribbon(mapping=aes(ymin=index_lo/1e6, ymax=index_hi/1e6), fill="grey80") +
  # Median
  geom_line(linewidth=0.3) +
  # Labels
  labs(x="Year", y="Index of abundance") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y = element_blank())
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_abundance_indices.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

