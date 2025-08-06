
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(taxize)

# Directories
plotdir <- "figures"

# Read data
data <- readRDS("data/stock_smart/processed/stock_smart_data_use.Rds")
stocks_orig <- readRDS("data/stock_smart/processed/stock_smart_stocks_use.Rds")

# Build data
stocks <- stocks_orig %>% 
  # Simplify
  select(stock_id, stock, fmp, comm_name, sci_name, area, last_data_year, 
         f_year:msy_unit) %>% 
  # Reduce
  filter(!is.na(b_bmsy) & !is.na(f_fmsy)) %>% 
  # Remove old assessments
  filter(!stock %in% c("Cabezon - California", "Black rockfish - Southern Pacific Coast")) # Cabezon gets split

# Stocks 2 label
stocks_label <- stocks %>% 
  filter(b_bmsy<=0.5 | f_fmsy > 1)

freeR::which_duplicated(stocks$comm_name)

# Manuscript stats
nrow(stocks)
sum(stocks$b_bmsy>=1 & stocks$f_fmsy<=1)
sum(stocks$b_bmsy>=0.5 & stocks$f_fmsy<=1)

# Read data
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
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stocks, aes(x=b_bmsy, y=f_fmsy, fill=last_data_year)) +
  # Reference lines
  geom_vline(xintercept=0.5, linetype="dotted", color="grey60") +
  geom_vline(xintercept=1, linetype="dashed", color="grey30") +
  geom_hline(yintercept=1, linetype="dashed", color="grey30") +
  # Point
  geom_point(pch=21, size=2) +
  # Label overfished
  # ggrepel::geom_text_repel(data=stocks_label, 
  #                          mapping=aes(x=b_bmsy, y=f_fmsy, label=stock)) +
  geom_text(data=stocks_label, 
           mapping=aes(x=b_bmsy, y=f_fmsy, label=stock),
           hjust=-0.1, size=2.0) +
  # Labels
  labs(x=expression("B / B"["MSY"]), 
       y=expression("F / F"["MSY"])) +
  # Legend
  scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Axes
  scale_x_continuous(lim=c(0, NA), breaks=c(0:5, 0.5)) +
  scale_y_continuous(lim=c(0, NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.9,  0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_ca_stock_status.png"), 
       width=4.5, height=4.5, units="in", dpi=600)


