#-------------------------------------------------------------------------------
# Script: enterprise_prep.R
# Author: Nolan Young Zabala
# Description: - load data for Turkiye
#              - select relevant variables
#              - output intermediate data files for analysis
#-------------------------------------------------------------------------------


#---------------------------------- 1. SET UP ----------------------------------

# Load libraries
library(haven)
library(dplyr)
library(sf)

# Set working directory 
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-turkiye/raw_data")


#-------------------------------- 2. LOAD DATA ---------------------------------

# PERFORM ONCE AND NEVER AGAIN - SUBSET COMPREHENSIVE

# Comprehensive dataset
# master <- read_dta("master_analysis.dta")

# turk19 <- master %>% 
#  filter(cy == 162) # Filter country var from comprehensive dataset

#write.csv(turk19, "Turk_19.csv")

# Remove "master" from memory - don't need anymore and too big
#rm(master)

# Load Turkiye Enterprise Survey 2019
turk19 <- read.csv("Turk_19.csv")

# Load shapefile for adding coordinates
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-bangladesh/raw_data")
point <- read_sf("shapefiles/firm_pointfile.shp")

# Load SPEI file
spei <- read.csv("wld_cli_terraclimate_spei12_1959_2021_point.csv")

#------------------------ 3. ADD COORDINATES + SPEI ----------------------------

# Merge with shapefile
turk19 <- left_join(turk19, point, by = "idstd")

# Remove shapefile to save memory
rm(point)

# Add "spei_" to the start of each column name in the SPEI dataframe
# to more easily identify the variable once merged to the bangla13 dataframe.
new_colnames <- paste("spei", colnames(spei), sep = "_")
colnames(spei) <- new_colnames

# Rename firm identifier in SPEI dataframe
spei <- spei %>% 
  rename("idstd" = "spei_firm_point")

# Merge SPEI with turk19
turk19 <- left_join(turk19, spei, by = "idstd")

# Remove spei to save memory
rm(spei)

#-------------------------- 4. SELECT RELEVANT VARS ----------------------------

# Create list of relevant vars I want to use for analysis
# Divide them up by type for ease of reference

# Firm vars
firm_location_vars <- c("idstd", "lat_mask", "lon_mask", "a2x", "a3ax")
firm_size_vars <- c("size", "a7", "b6")
firm_sector_vars <- c("stra_sector")
firm_gender_vars <- c("b4", "b4a")

# Climate vars

# The vars are repeated across many columns for different years... So this is
# a function which will create a regular expression to use when selecting
# multiple columns starting with the same string, e.g. "hotdays"
prefix <- function(prefix_list){
  paste("^", paste(prefix_list, collapse = "|"), sep = "")
    }

climate_temp_vars <- c("hotdays_mon_sum")
climate_temp_pattern <- prefix(climate_temp_vars)

climate_rain_vars <- c("cwd_5mm_mon_max", "precip_mon")
climate_rain_pattern <- prefix(climate_rain_vars)

climate_drought_vars <- c("cdd_1mm_mon", "drydays_5mm_mon", "spei")
climate_survey_drought_vars <- c("c15", "c16")
climate_drought_pattern <- prefix(climate_drought_vars)

turk19_subset <- turk19 %>% 
  select(firm_location_vars, firm_size_vars, firm_sector_vars,
         firm_gender_vars, matches(climate_temp_pattern), 
         matches(climate_rain_pattern), climate_survey_drought_vars,
         matches(climate_drought_pattern))


#---------------------- 5. WRITE INTERMEDIATE DATA FILES -----------------------

# Set working directory for writing files
setwd("C:/Users/young/OneDrive/Escritorio/wb-climate-pilots-turkiye/intermediate_data")

# Write turk19_subset to csv
write.csv(turk19_subset, "Turk19_FirmAndClimate.csv")

# Subset to just firm vars and write to csv
turk19_just_firm <- turk19_subset %>% 
  select(firm_location_vars, firm_size_vars, firm_sector_vars,
         firm_gender_vars)

write.csv(turk19_just_firm, "Turk19_Firm.csv")

