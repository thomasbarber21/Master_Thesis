# Description: Load necessary libraries and create directories

# Create Directories and load necessary packages ----
rm(list=ls())
want <- c("RColorBrewer","plm","lfe","sandwich","lmtest","multiwayvcov","nlme","boot", "sf","terra","RColorBrewer","Matrix","tmaptools","rworldmap", "av", "magick", "maps", "mapproj", "utils", "hdm", "glmnet", "fastDummies", "data.table", "nngeo", "googlePolylines", "zoo", "gsynth", "fixest")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

dir <- list()
dir$root <- dirname(getwd())
dir$data   <- paste(dir$root,"/data",sep="")
dir$output <- paste(dir$root,"/output",sep="")
dir$raster <- paste(dir$root,"/data/raster",sep="")
dir$shape <- paste(dir$root,"/data/shapefile",sep="")


# Section 1: Load Air Quality Data ----
months_list <- c("may2024", "june2024", "july2024", "aug2024", "sept2024", "oct2024", "nov2024", "dec2024", "jan2025", "feb2025", "march2025", "april2025")
air_quality <- list()
for (i in months_list) {
  file_path <- paste(dir$data,"/Air_Quality/",i,"_airquality.csv", sep = "")
  air_quality[[i]] <- read.csv(file_path)
}


# Section 2: Load EZ Pass Data ----
months_list_new <- c("sept2024", "oct2024", "nov2024", "dec2024", "jan2025", "feb2025", "march2025", "april2025")
ez_pass <- list()
for (i in months_list_new) {
  file_path <- paste(dir$data, "/EZ_Pass/EZPass_",i,".csv", sep = "")
  ez_pass[[i]] <- read.csv(file_path)
}


# Section 3: Load Raster and Shape file Data for Borough Visualization and CBD----

# Load Raster data and shape files
fname <- paste(dir$shape,"/Borough Boundaries/geo_export_54d23f71-68b8-40ec-9f00-0492bfe7d77e.shp",sep="")

# Read the data and gather geometries
boroughs <- st_read(fname)
stgeo <- st_geometry(boroughs)

# Pull shapefile for Manhattan's Central Business District
fname_cbd <- paste(dir$shape, "/Manhattan_CBD_Shapefile/geo_export_cb73ae09-6d1e-4bfb-8533-3007a6378d15.shp", sep = "")
manhattan_cbd <- st_read(fname_cbd)


# Section 4: Load the MTA Ridership Data ----

# Load the Data
MTA_ridership_data <- paste(dir$data,"/MTA_Ridership/MTA_Daily_Ridership.csv", sep = "")
MTA_ridership_data <- read.csv(MTA_ridership_data)
MTA_ridership_data <- list(Daily_Ridership = MTA_ridership_data)


# Section 5: Load the NYC Arrests Data ----

# Load the data, might take a few minutes for a large data set
NYC_arrests_historic <- paste(dir$data,"/NYPD_Arrests/NYPD_Arrests_historic.csv", sep = "")
NYC_arrests_historic <- read.csv(NYC_arrests_historic)

NYC_arrests_2025 <- paste(dir$data,"/NYPD_Arrests/NYPD_Arrests_2025.csv", sep = "")
NYC_arrests_2025 <- read.csv(NYC_arrests_2025)

NYC_arrests <- list(Historic_Data = NYC_arrests_historic, Year_to_Date = NYC_arrests_2025)
rm(NYC_arrests_historic, NYC_arrests_2025)

# Section 6: Load Bus and Subway Location Data ----
bus_stop_locations <- paste(dir$data,"/BusStop_Locations/Bus_Stop_Locations_nyc.csv", sep = "")
bus_stop_locations <- read.csv(bus_stop_locations)

subway_locations <- paste(dir$data,"/Subway_Locations/Subway_Locations.csv", sep = "")
subway_locations <- read.csv(subway_locations)

Public_transport_locations <- list(Bus_Stops = bus_stop_locations, Subway_Stops = subway_locations)
rm(bus_stop_locations, subway_locations)

