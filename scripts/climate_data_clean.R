#-------------------------------------------------------------------------------
# Script: climate_data_clean.R
# Author: Nolan Young Zabala
# Description: - format climate data in usable way
#              - compute metrics like old averages/changes/etc
#              - output data for use in dashboard
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(gridExtra)

# Set working directory
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-turkiye/intermediate_data")


#-------------------------------- 2. LOAD DATA ---------------------------------

# Read sf_joined
sf_joined <- read_sf("sf_joined.shp")

# Read climate data
climate <- read.csv("Turk19_FirmAndClimate.csv")

# Get rid of vars I don't need
not_needed <- c("X", "a2x", "a3ax", "size", "a7", "b6", "stra_sector", "b4", "b4a")

climate <- climate %>% 
  select(-all_of(not_needed))

# Create dataframe to store clean data, will merge results in
climate_clean <- climate %>% 
  select(idstd, lat_mask, lon_mask)


#------------------------ 3. PRE/CURRENT/ENTIRE PERIOD -------------------------

# Pre-period
pre_period <- 1981:2019

# Current period
current_period <- 2020:2022

# Entire period
entire_period <- 1981:2022


#---------------------------- 4. GENERAL FUNCTIONS -----------------------------


# METHOD: take yearly total e.g. hot days; find average up to 2019; find average of
# 2020/21/22; compute change (keep all 3 columns)


# Function which subsets for given climate var
climate_var_subset <- function(climate_var){
  
  relevant_cols <- c("idstd", grep(climate_var, names(climate), value = TRUE))
  
  climate_var_df <- climate[, relevant_cols, drop = FALSE]
  
  return(climate_var_df)
}


# Function which computes yearly sum for given year
calculate_yearly_total <- function(climate_var_df, year) {

  # Subset to relevant year
  relevant_cols <- c("idstd", grep(year, names(climate_var_df), value = TRUE))
  varyear_df <- climate_var_df[, relevant_cols, drop = FALSE]
  
  year <- as.character(year)
  
  # Sum all columns except idstd
  result <- varyear_df %>%
    mutate(!!year := rowSums(select(., -idstd))) %>% 
    select(idstd, !!year)
    
  return(result)
}


# Period means function
period_means <- function(df){
  
  result <- df %>%
    mutate(pre_mean = rowMeans(select(., as.character(pre_period)), na.rm = TRUE),
           current_mean = rowMeans(select(., as.character(current_period)), na.rm = TRUE)) %>%
    select(idstd, pre_mean, current_mean)  
}


# Calculate percent change
percent_change <- function(df){
  result <- df %>%
    mutate(percent_change = ((current_mean - pre_mean) / pre_mean) * 100)
  
  return(result)
} 


# Function which puts it all together and merges with sf data
climate_compute <- function(variable){
  
  # Subset to only climate var
  var_df <- climate_var_subset(variable)
  
  
  # Calculate yearly totals for each year
  wrapper <- function(year) {
    calculate_yearly_total(climate_var_df = var_df, year)
  }
  
  var_totals <- lapply(entire_period, wrapper)
  
  var_totals <- Reduce(function(x, y) merge(x, y, by = "idstd", all = TRUE), var_totals)
  
  
  # Calculate period means and percent change
  var_means <- period_means(var_totals)
  
  var_means <- percent_change(var_means)
  
  
  # Merge with sf_joined
  sf_var <- left_join(sf_joined, var_means, by = "idstd")
  
  # Find average percent change per grid
  sf_var <- sf_var %>% 
    group_by(geometry) %>% 
    summarize(HeatVar = mean(percent_change))
}

heatplot_climate <- function(df, plot_title, legend_title){
  ggplot(df) +     
    geom_sf(aes(fill = HeatVar), size = 0.2) +     
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +     
    labs(title = plot_title, fill = legend_title) 
}


#------------------------------- 5. TEMPERATURE --------------------------------

#Hotdays
sf_hotdays <- climate_compute("hotdays")

#spei
sf_spei <- climate_compute("spei")


#-------------------------------- 6. RAINFALL ----------------------------------

#Cwd
sf_cwd <- climate_compute("cwd")

#Precip
sf_precip <- climate_compute("precip")


#------------------------------- 7. DROUGHT ------------------------------------

#Drydays
sf_drydays <- climate_compute("drydays")

#Precip
sf_cdd <- climate_compute("cdd")

