
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(mblm)
library(zyp)

# Directories
indir <- "data/stock_smart/raw"
outdir <- "data/stock_smart/processed"
plotdir <- "data/stock_smart/figures"

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "Assessment_TimeSeries_Data_Part_1.xlsx"), sheet=1)
data_orig2 <- readxl::read_excel(file.path(indir, "Assessment_TimeSeries_Data_Part_2.xlsx"), sheet=1)


# Function to format data
################################################################################

format_data <- function(data_orig){
  
  # Format header
  stocks <- data_orig %>% 
    # Reduce to header
    slice(1:4) %>% 
    # Eliminate useless column
    select(-"...2") %>% 
    # Gather
    gather(key="stock", value="value", 2:ncol(.)) %>% 
    # Format stock
    mutate(stock=gsub("\\.\\.\\.[0-9].*", "", stock)) %>% 
    # Unique
    unique() %>% 
    # Spread
    rename("metric"="Stock Name") %>% 
    spread(key="metric", value="value") %>% 
    # Rename
    janitor::clean_names("snake") %>% 
    # Convert to numeric 
    mutate_at(vars(assessment_id:stock_id), as.numeric)
  
  # Inspect
  str(stocks)  
  
  # Check unique identifiers (already most recent assessment)
  freeR::which_duplicated(stocks$stock)
  freeR::which_duplicated(stocks$stock_id)
  freeR::which_duplicated(stocks$assessment_id)
  
  # Build header key
  key <- data_orig %>%
    # Reduce
    slice(5:7) %>% 
    select(-"...2") %>% 
    # Rename and reshape
    rename("metric"="Stock Name") %>% 
    gather(key="colname", value="units", 2:ncol(.)) %>% 
    spread(key="metric", value="units") %>% 
    # Rename
    janitor::clean_names("snake") %>% 
    rename("units"="unit",
           "param_catg"="parameter",
           "param"="description") %>% 
    # Format
    mutate(param_catg=recode(param_catg, 
                             "Fmort"="Fishing mortality"))
  
  # Format time series
  data <- data_orig %>% 
    # Remove header
    slice(8:nrow(.)) %>% 
    # Remove 1st column
    select(-"Stock Name") %>% 
    # Rename
    rename("year"="...2") %>% 
    mutate(year=as.numeric(year)) %>% 
    # Gather
    gather(key="colname", value="value", 2:ncol(.)) %>% 
    # Add units
    left_join(key, by="colname") %>% 
    rename(stock=colname) %>% 
    # Format stock
    mutate(stock=gsub("\\.\\.\\.[0-9].*", "", stock)) %>% 
    # Convert to numberic
    mutate(value=as.numeric(value)) %>% 
    # Arrange
    select(stock, param_catg, param, units, year, value) %>% 
    arrange(stock, param_catg, param, year)

  # Output
  out <- list(data=data, stocks=stocks)
  return(out)
  
}

# Format data
out1 <- format_data(data_orig1)
out2 <- format_data(data_orig2)

# Merge stocks
stocks <- bind_rows(out1$stocks, out2$stocks)
data <- bind_rows(out1$data, out2$data)

# Confirm that all are in stock key
stocks_in_data <- freeR::uniq(data$stock)
stocks_in_data[!stocks_in_data %in% stocks$stock] # should be zero

# Export data
saveRDS(data, file=file.path(outdir, "stock_smart_time_series.Rds"))

# Format summary
################################################################################

# Read data
stocks_orig <- readxl::read_excel(file.path(indir, "Assessment_Summary_Data.xlsx"), sheet=1, na=c("NA", "N/A"))

# Format data
stocks1 <- stocks_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(stock=stock_name,
         ecosystem=regional_ecosystem,
         council=jurisdiction,
         center=science_center, 
         fssi_yn=fssi_stock, 
         taxa_id=itis_taxon_serial_number,
         sci_name=scientific_name,
         comm_name=common_name,
         area=stock_area) %>% 
  # Stocks of interest
  filter(stock_id %in% stocks$stock_id) %>% 
  # Convert to numeric
  mutate_at(vars(last_data_year, f_year, estimated_f, f_flimit, ftarget, f_ftarget, flimit, fmsy, f_fmsy,
                 b_year, estimated_b,
                 blimit, bmsy, b_blimit, b_bmsy, msy), as.numeric) %>% 
  # Clean scientific name
  mutate(sci_name=recode(sci_name,
                         "Reinhardtius stomias"="Atheresthes stomias"))


str(stocks1)

# Export data
saveRDS(stocks1, file=file.path(outdir, "stock_smart_stocks.Rds"))


# Plot data
################################################################################

ggplot(stocks1, aes(y=f_fmsy, x=b_bmsy)) +
  # Reference lines
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1) +
  # Points
  geom_point() +
  # Labels
  labs(x="B/BMSY", y="F/BMSY") +
  # Limits
  lims(x=c(0, 5)) +
  # Theme
  theme_bw()




