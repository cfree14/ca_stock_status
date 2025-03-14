
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/calcofi/processed"
plotdir <- "data/calcofi/figures"

# Read data
tows_orig <- readRDS(file=file.path(datadir, "calcofi_fish_larvae_tows_use.Rds"))


# Format data
################################################################################

# Format data
tows <- tows_orig %>% 
  # Date dummy
  mutate(date_dummy=paste("2000", month(date1), day(date1), sep="-") %>% ymd()) %>% 
  # Line
  mutate(pattern=ifelse(line>=76.7, "Southern", "Northern"),)


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
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(tows, aes(x=date_dummy, y=year, color=pattern)) +
  geom_point(shape=1, size=0.8) +
  # Labels
  labs(x="Day of year", y="Year") +
  scale_y_continuous(breaks=seq(1985,2025,5)) +
  scale_x_date(date_label="%b", 
               breaks=seq(ymd("2000-01-01"), ymd("2001-01-01"), by="1 month")) +
  # Legend
  scale_color_discrete(name="Station type") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_tows_seasonality.png"), 
       width=6.5, height=3.0, units="in", dpi=600)


