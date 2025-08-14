# Description: Clean and manipulate the uploaded data, for plotting and regressing

# Section 1: Air Quality Data ----

# Add column for Month
air_quality[["may2024"]]$month <- "may"
air_quality[["june2024"]]$month <- "june"
air_quality[["july2024"]]$month <- "july"
air_quality[["aug2024"]]$month <- "aug"
air_quality[["sept2024"]]$month <- "sept"
air_quality[["oct2024"]]$month <- "oct"
air_quality[["nov2024"]]$month <- "nov"
air_quality[["dec2024"]]$month <- "dec"
air_quality[["jan2025"]]$month <- "jan"
air_quality[["feb2025"]]$month <- "feb"
air_quality[["march2025"]]$month <- "march"
air_quality[["april2025"]]$month <- "april"

# Combine the datasets
air_quality$air_quality_aggregated <- rbind(air_quality[["may2024"]], air_quality[["june2024"]], air_quality[["july2024"]], air_quality[["aug2024"]], air_quality[["sept2024"]], air_quality[["oct2024"]], air_quality[["nov2024"]], air_quality[["dec2024"]], air_quality[["jan2025"]], air_quality[["feb2025"]], air_quality[["march2025"]], air_quality[["april2025"]])
air_quality[["air_quality_aggregated"]]$ObservationTimeUTC <- as.Date(air_quality[["air_quality_aggregated"]]$ObservationTimeUTC)

# Create weekly data of means from each day
air_quality[["air_quality_aggregated"]]$week <- cut(air_quality[["air_quality_aggregated"]]$ObservationTimeUTC, breaks = "week")
air_quality[["air_quality_aggregated"]] <- aggregate(Value ~ week, data = air_quality[["air_quality_aggregated"]], FUN = mean)
air_quality[["air_quality_aggregated"]]$week <- as.Date(air_quality[["air_quality_aggregated"]]$week)


# Section 2: EZ Pass Data ----

# Convert time stamps to dates, create week breaks
ez_pass_aggregated <- list()

for (i in months_list_new) {
  ez_pass[[i]]$median_calculation_timestamp <- as.Date(ez_pass[[i]]$median_calculation_timestamp, format = "%m/%d/%Y %I:%M:%S %p")
  ez_pass[[i]]$week <- cut(ez_pass[[i]]$median_calculation_timestamp, breaks = "week")
  ez_pass_aggregated[[i]] <- aggregate(median_speed_fps ~ week + polyline + borough, data = ez_pass[[i]], FUN = mean)
}

# Find the weekly mean of the speed captured by EZ Pass tolls
ezpass_weekly_averages <- list()

for (i in months_list_new) {
  ez_pass[[i]]$median_calculation_timestamp <- as.Date(ez_pass[[i]]$median_calculation_timestamp, format = "%m/%d/%Y %I:%M:%S %p")
  ez_pass[[i]]$week <- cut(ez_pass[[i]]$median_calculation_timestamp, breaks = "week")
  ezpass_weekly_averages[[i]] <- aggregate(median_speed_fps ~ week, data = ez_pass[[i]], FUN = mean)
}
ezpass_weekly_averages <- do.call(rbind, ezpass_weekly_averages)
ezpass_weekly_averages$week <- as.Date(ezpass_weekly_averages$week)
ezpass_weekly_averages$week_number <- 1:nrow(ezpass_weekly_averages)
ez_pass_aggregated$ezpass_weekly_averages <- ezpass_weekly_averages
rm(ezpass_weekly_averages)


# Section 3: MTA Ridership Data ----

# Clean the MTA Ridership Data
MTA_ridership_data$Cleaned_Daily_Ridership <- MTA_ridership_data$Daily_Ridership
MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Date <- as.Date(MTA_ridership_data$Cleaned_Daily_Ridership[[1]], "%m/%d/%Y")
MTA_ridership_data[["Cleaned_Daily_Ridership"]] <- MTA_ridership_data[["Cleaned_Daily_Ridership"]][, -c(3, 5, 6, 7, 9, 11, 13, 15)]
MTA_ridership_data[["Cleaned_Daily_Ridership"]] <- MTA_ridership_data[["Cleaned_Daily_Ridership"]][MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Date >= as.Date("2024-01-01"), ]
MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week <- cut(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Date, breaks = "week")
MTA_ridership_data[["Cleaned_Daily_Ridership"]] <- MTA_ridership_data[["Cleaned_Daily_Ridership"]][, -c(1)]

# Condense the data to weekly means of total ridership
MTA_ridership_data$Subway_weekly_avg <- tapply(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Subways..Total.Estimated.Ridership, MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week, mean)
MTA_ridership_data$Bus_weekly_avg <- tapply(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Buses..Total.Estimated.Ridership, MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week, mean)
MTA_ridership_data$MetroNorth_weekly_avg <- tapply(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Metro.North..Total.Estimated.Ridership, MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week, mean)
MTA_ridership_data$Access_a_Ride_weekly_avg <- tapply(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Access.A.Ride..Total.Scheduled.Trips, MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week, mean)
MTA_ridership_data$Bridge_Tunnel_traffic_weekly_avg <- tapply(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Bridges.and.Tunnels..Total.Traffic, MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week, mean)
MTA_ridership_data$StatenIsland_Railway_weekly_avg <- tapply(MTA_ridership_data[["Cleaned_Daily_Ridership"]]$Staten.Island.Railway..Total.Estimated.Ridership, MTA_ridership_data[["Cleaned_Daily_Ridership"]]$week, mean)

MTA_ridership_data$weekly_averages <- data.frame(week = names(MTA_ridership_data[["Subway_weekly_avg"]]), Subways = MTA_ridership_data[["Subway_weekly_avg"]], Busses = MTA_ridership_data[["Bus_weekly_avg"]], Metro_North = MTA_ridership_data[["MetroNorth_weekly_avg"]], Access_A_Ride = MTA_ridership_data[["Access_a_Ride_weekly_avg"]], Bridges_Tunnels = MTA_ridership_data[["Bridge_Tunnel_traffic_weekly_avg"]], Staten_Island_railroad = MTA_ridership_data[["StatenIsland_Railway_weekly_avg"]])
MTA_ridership_data[["weekly_averages"]]$week <- as.Date(MTA_ridership_data[["weekly_averages"]][,1], "%Y-%m-%d")

# Date in which congestion charge went into effect
implementation_date <- as.Date("2025-01-03")


# Section 4: NYC Arrests Data ----

# Change Borough Labels
names(NYC_arrests$Year_to_Date)[names(NYC_arrests$Year_to_Date) == "New.Georeferenced.Column"] <- "Lon_Lat"

NYC_arrests$Cleaned_Arrests_Data <- rbind(NYC_arrests$Historic_Data, NYC_arrests$Year_to_Date)
NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_DATE <- as.Date(NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_DATE, "%m/%d/%Y")

NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO[NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO == "M"] <- "Manhattan"
NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO[NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO == "B"] <- "Bronx"
NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO[NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO == "K"] <- "Brooklyn"
NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO[NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO == "Q"] <- "Queens"
NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO[NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO == "S"] <- "Staten_Island"

NYC_arrests[["Cleaned_Arrests_Data"]]$Manhattan <- ifelse(NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_BORO == "Manhattan", 1, 0)
NYC_arrests[["Cleaned_Arrests_Data"]]$Post <- ifelse(NYC_arrests[["Cleaned_Arrests_Data"]]$ARREST_DATE >= "2025-01-03", 1, 0)

# Clean the data
NYC_arrests$Aggregated <- NYC_arrests$Cleaned_Arrests_Data
NYC_arrests[["Aggregated"]] <- NYC_arrests[["Aggregated"]][, c("ARREST_DATE", "ARREST_BORO")]
NYC_arrests[["Aggregated"]]$Arrests <- 1
NYC_arrests[["Aggregated"]]$ARREST_DATE <- as.Date(NYC_arrests[["Aggregated"]][,1], "%m/%d/%Y")
NYC_arrests[["Aggregated"]]$week <- cut(NYC_arrests[["Aggregated"]]$ARREST_DATE, breaks = "week")
NYC_arrests[["Aggregated"]] <- aggregate(Arrests ~ week + ARREST_BORO, data = NYC_arrests[["Aggregated"]], FUN = sum)

NYC_arrests[["Aggregated"]]$week <- as.Date(NYC_arrests[["Aggregated"]]$week, format = "%Y-%m-%d")

# Separate each Borough
NYC_arrests$Manhattan_crime <- NYC_arrests[["Aggregated"]][NYC_arrests[["Aggregated"]]$ARREST_BORO == "Manhattan",]
NYC_arrests$Bronx_crime <- NYC_arrests[["Aggregated"]][NYC_arrests[["Aggregated"]]$ARREST_BORO == "Bronx",]
NYC_arrests$Queens_crime <- NYC_arrests[["Aggregated"]][NYC_arrests[["Aggregated"]]$ARREST_BORO == "Queens",]
NYC_arrests$Brooklyn_crime <- NYC_arrests[["Aggregated"]][NYC_arrests[["Aggregated"]]$ARREST_BORO == "Brooklyn",]
NYC_arrests$StatenIsland_crime <- NYC_arrests[["Aggregated"]][NYC_arrests[["Aggregated"]]$ARREST_BORO == "Staten_Island",]


# Section 5: Data Manipulations for Regression ----

# Create a data frame with all the necessary information for initial regression
NYC_arrests[["Aggregated"]]$Manhattan <- ifelse(NYC_arrests[["Aggregated"]]$ARREST_BORO == "Manhattan", 1, 0)
NYC_arrests[["Aggregated"]]$pre_treatment_period <- ifelse(NYC_arrests[["Aggregated"]]$week < as.Date("2025-01-03"), 1, 0)

NYC_arrests[["Aggregated"]]$ARREST_BORO <- as.factor(NYC_arrests[["Aggregated"]]$ARREST_BORO)

# Data for Pre Treatment Plot
NYC_arrests$manhattan_pre_trends <- subset(NYC_arrests[["Aggregated"]], Manhattan == 1 & pre_treatment_period == 1)
NYC_arrests$non_manhattan_pretrends <- subset(NYC_arrests[["Aggregated"]], Manhattan == 0 & pre_treatment_period == 1)

NYC_arrests$non_manhattan_pretrends <- aggregate(Arrests ~ week, data = NYC_arrests$non_manhattan_pretrends, FUN = mean)


# Section 6: Subway, Bus, Arrests Locations and Distance Rings ----
fname <- paste(dir$shape,"/Borough Boundaries/geo_export_54d23f71-68b8-40ec-9f00-0492bfe7d77e.shp",sep="")

# Sift out invalid coordinates and zero values
valid_coordinates <- NYC_arrests$Cleaned_Arrests_Data$Longitude > -74.3 &
  NYC_arrests$Cleaned_Arrests_Data$Longitude < -73.6 &
  NYC_arrests$Cleaned_Arrests_Data$Latitude > 40.4 &
  NYC_arrests$Cleaned_Arrests_Data$Latitude < 41.0
NYC_arrests$Cleaned_Arrests_Data <- NYC_arrests$Cleaned_Arrests_Data[valid_coordinates, ]

Public_transport_locations$boroughs_shape <- st_read(fname)
st_crs(Public_transport_locations[["boroughs_shape"]]) <- 4326

# Convert Public Transportation values to similar geometry projection as arrests
Public_transport_locations$bus_stops_sf <- st_as_sf(Public_transport_locations$Bus_Stops, coords = c("Longitude", "Latitude"), crs = 4326) #4326
Public_transport_locations$bus_stops_sf <- st_transform(Public_transport_locations$bus_stops_sf, crs = st_crs(2263))

Public_transport_locations$subway_stops_sf <- st_as_sf(Public_transport_locations$Subway_Stops, coords = c("Entrance.Longitude", "Entrance.Latitude"), crs = 4326) #4326
Public_transport_locations$subway_stops_sf <- st_transform(Public_transport_locations$subway_stops_sf, crs = st_crs(2263))

Public_transport_locations$boroughs_proj <- st_transform(Public_transport_locations$boroughs_shape, crs = 2263)
Public_transport_locations$subway_proj <- st_transform(Public_transport_locations$subway_stops_sf, crs = 2263)
Public_transport_locations$bus_proj <- st_transform(Public_transport_locations$bus_stops_sf, crs = 2263)

NYC_arrests[["Cleaned_Arrests_Data"]] <- na.omit(NYC_arrests[["Cleaned_Arrests_Data"]])
NYC_arrests$arrests_sf <- st_as_sf(NYC_arrests[["Cleaned_Arrests_Data"]], coords = c("Longitude", "Latitude"), crs = 4326)
NYC_arrests$arrests_sf <- st_transform(NYC_arrests$arrests_sf, crs = st_crs(2263))

NYC_arrests$arrests_proj <- st_transform(NYC_arrests$arrests_sf, crs = 2263)

# Combine geometries of Busses and Subways
Public_transport_locations$combined_geom <- c(st_geometry(Public_transport_locations$subway_proj), st_geometry(Public_transport_locations$bus_proj))
Public_transport_locations$transit_points <- st_sf(geometry = Public_transport_locations$combined_geom, crs = st_crs(2263))

# Calculate the distance between arrests and public transportation
NYC_arrests$nn_result <- st_nn(NYC_arrests$arrests_proj, Public_transport_locations$transit_points, k = 1, returnDist = TRUE)
NYC_arrests$min_dist <- sapply(NYC_arrests[["nn_result"]]$dist, function(x) as.numeric(x))

# Look into changing distance rings
NYC_arrests[["arrests_proj"]]$distance_ring <- cut(NYC_arrests$min_dist, breaks = c(-Inf, 10, 25, 50, 100, Inf), labels = c("10ft", "25ft", "50ft", "100ft", ">100ft"), right = TRUE)

NYC_arrests$arrests_distance_data <- st_drop_geometry(NYC_arrests$arrests_proj)

