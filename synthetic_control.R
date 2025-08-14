# Description: Subset data to the Central Business District of Manhattan for Air Quality
# and EZ Pass Data. Utilize synthetic control to create a counterfactual for Manhattan 
# in the post congestion charge era.

# Section 1: Separate Air Quality Collection Sites within Manhattan CBD and Other Boroughs ----
air_quality$airquality_synthetic_control <- rbind(air_quality$may2024, air_quality$june2024, air_quality$july2024, air_quality$aug2024, air_quality$sept2024, air_quality$oct2024, air_quality$nov2024, air_quality$dec2024, air_quality$jan2025, air_quality$feb2025, air_quality$march2025, air_quality$april2025) #airquality_nycoriginal

air_quality[["airquality_synthetic_control"]]$Within_CBD <- ifelse(air_quality[["airquality_synthetic_control"]]$SiteID %in% c("36061NY10130", "36061NY09929", "36061NY09734", "36061NY08653", "36061NY08552", "36061NY08454"), 1, 0)

# Utilizing site location maps from NYC Open Portal, separate into CBD and non CBD
air_quality$airquality_CBD <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == 1,]
air_quality$airquality_nonCBD <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == 0,]


# Section 2: Separate EZ Pass Data into Manhattan CBD and non CBD ----
ez_pass$ezpass_speeds_synthetic_control <-  rbind(ez_pass_aggregated$sept2024, ez_pass_aggregated$oct2024, ez_pass_aggregated$nov2024, ez_pass_aggregated$dec2024, ez_pass_aggregated$jan2025, ez_pass_aggregated$feb2025, ez_pass_aggregated$march2025, ez_pass_aggregated$april2025) #ez_pass_speeds_nyc

# First break up data into Manhattan and non Manhattan Data
ez_pass[["ezpass_speeds_synthetic_control"]]$Within_CBD <- ifelse(ez_pass[["ezpass_speeds_synthetic_control"]]$borough == "Manhattan" , 1, 0)

ez_pass$ezpass_speeds_Manhattan <- ez_pass[["ezpass_speeds_synthetic_control"]][ez_pass[["ezpass_speeds_synthetic_control"]]$Within_CBD == 1,]
ez_pass$ezpass_speeds_nonManhattan <- ez_pass[["ezpass_speeds_synthetic_control"]][ez_pass[["ezpass_speeds_synthetic_control"]]$Within_CBD == 0,]

# Utilize Polyline data and GooglePolyline package to break polyline into latitudes and longitudes
lat <- numeric(length(ez_pass[["ezpass_speeds_Manhattan"]]$polyline))
lon <- numeric(length(ez_pass[["ezpass_speeds_Manhattan"]]$polyline))

for (i in 1:length(ez_pass[["ezpass_speeds_Manhattan"]]$polyline)) {
  ez_pass$decoded <- decode(ez_pass[["ezpass_speeds_Manhattan"]]$polyline[i])
  
  lat[i] <- ez_pass[["decoded"]][[1]]$lat[1]
  lon[i] <- ez_pass[["decoded"]][[1]]$lon[1]
}
ez_pass[["ezpass_speeds_Manhattan"]]$lat <- lat
ez_pass[["ezpass_speeds_Manhattan"]]$lon <- lon

lat <- numeric(length(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline))
lon <- numeric(length(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline))

for (i in 1:length(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline)) {
  ez_pass$decoded <- decode(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline[i])
  
  lat[i] <- ez_pass[["decoded"]][[1]]$lat[1]
  lon[i] <- ez_pass[["decoded"]][[1]]$lon[1]
}

ez_pass[["ezpass_speeds_nonManhattan"]]$lat <- lat
ez_pass[["ezpass_speeds_nonManhattan"]]$lon <- lon

# Utilize shapefile of Manhattan's CBD to extract values within the CBD
ez_pass$ez_pass_sf <- st_as_sf(ez_pass[["ezpass_speeds_Manhattan"]], coords = c("lon", "lat"), crs = 4326)
ez_pass$inside_cbd <- st_within(ez_pass[["ez_pass_sf"]], manhattan_cbd, sparse = FALSE)

ez_pass$ezpass_speeds_CBD <- ez_pass[["ez_pass_sf"]][ez_pass[["inside_cbd"]], ]
ez_pass$ezpass_speeds_nonCBD_Manhattan <- ez_pass[["ez_pass_sf"]][!(ez_pass[["inside_cbd"]]), ]

# Drop geometries and bind data into CBD and non CBD dataframes
ez_pass[["ezpass_speeds_CBD"]] <- st_drop_geometry(ez_pass[["ezpass_speeds_CBD"]])
ez_pass[["ezpass_speeds_nonCBD_Manhattan"]] <- st_drop_geometry(ez_pass[["ezpass_speeds_nonCBD_Manhattan"]])
ez_pass[["ezpass_speeds_nonManhattan"]] <- ez_pass[["ezpass_speeds_nonManhattan"]][, -c(6,7)]

ez_pass[["ezpass_speeds_nonCBD"]] <- rbind(ez_pass[["ezpass_speeds_nonCBD_Manhattan"]], ez_pass[["ezpass_speeds_nonManhattan"]])

ez_pass[["ezpass_speeds_nonCBD"]]$week <- as.Date(ez_pass[["ezpass_speeds_nonCBD"]]$week)


# Section 3: Implement synthetic control for Air Quality Data ----
# Combine CBD and non-CBD data with an indicator
air_quality[["airquality_CBD"]]$Within_CBD <- "Yes"
air_quality[["airquality_nonCBD"]]$Within_CBD <- "No"
air_quality$airquality_synthetic_control <- rbind(air_quality[["airquality_CBD"]], air_quality[["airquality_nonCBD"]])

# IMPORTANT: If you include more data later on with more months you will need to edit this to prevent months from different years being included in pretreatment period
air_quality[["airquality_synthetic_control"]]$Year <- ifelse(air_quality[["airquality_synthetic_control"]]$month %in% c("may", "june", "july", "aug", "sept", "oct", "nov", "dec"), 2024, 2025)

air_quality[["airquality_synthetic_control"]]$month_num <- air_quality[["airquality_synthetic_control"]]$month
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "may"] <- 202405
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "june"] <- 202406
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "july"] <- 202407
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "aug"] <- 202408
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "sept"] <- 202409
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "oct"] <- 202410
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "nov"] <- 202411
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "dec"] <- 202412
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "jan"] <- 202501
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "feb"] <- 202502
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "march"] <- 202503
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "april"] <- 202504
air_quality[["airquality_synthetic_control"]]$month_num <- as.numeric(air_quality[["airquality_synthetic_control"]]$month_num)

# Had issues with balanced panels when using synth package, so utilize gsynth

# Split data into treatment and control groups, organize and aggregate data
treatment_group_AQ <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == "Yes", ]
control_group_AQ <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == "No", ]

treatment_group <- treatment_group_AQ
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]
treatment_group <- aggregate(Value ~ month_num + SiteID, data = treatment_group, FUN = mean, na.rm = TRUE)
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]

control_group <- control_group_AQ[, c("SiteID", "month_num", "Value")]
control_group <- aggregate(Value ~ month_num + SiteID, data = control_group, FUN = mean, na.rm = TRUE)
control_group <- control_group[, c("SiteID", "month_num", "Value")]

# Combine data, assign unique IDs to units of control and treated observations
AQ_synthetic_control_data <- rbind(control_group, treatment_group)
AQ_synthetic_control_data$unit_ID <- as.numeric(factor(AQ_synthetic_control_data$SiteID))

AQ_control_IDs <- unique(AQ_synthetic_control_data$unit_ID[AQ_synthetic_control_data$SiteID %in% control_group$SiteID])

AQ_treated_sites <- c("36061NY10130", "36061NY09734", "36061NY08653", "36061NY08552", "36061NY08454") # had to remove one site due to lack of data
AQ_synthetic_control_data$D <- ifelse(
  AQ_synthetic_control_data$SiteID %in% AQ_treated_sites & AQ_synthetic_control_data$month_num >= 202501,
  1,
  0
)

# Utilize gsynth to implement synthetic control
gsynth_AQ <- gsynth(Value ~ D, 
                     data = AQ_synthetic_control_data, 
                     index = c("unit_ID", "month_num"), 
                     force = "two-way",    # Unit and Time TWFE
                     inference = "nonparametric",    # recommended when number of treated units is small
                     estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                     #se = TRUE,
                     #nboots = 1000,
                     min.T0 = 7
)

# Pull out IDs for treated units, convert month_num to date object, plot a single treated unit 
colnames(gsynth_AQ$Y.tr)

AQ_treated_unit <- 6
AQ_treated_values <- gsynth_AQ$Y.tr[, as.character(AQ_treated_unit)]
AQ_counterfactual_values <- gsynth_AQ$Y.ct[, as.character(AQ_treated_unit)]

time_date <- rownames(gsynth_AQ$Y.tr)
time_date <- as.yearmon(time_date, "%Y%m")
time_date <- as.Date(time_date)

png(filename = "../output/Air_Quality_Synthetic_Control.png", width = 800, height = 600)
plot(time_date, AQ_treated_values, type = "l", main = "Air Quality Synthetic Control", xlab = "Time (months)", ylab = "PM2.5 Value")
lines(time_date, AQ_counterfactual_values, col = "red", type = "l", lty = 2)
abline(v = time_date[9])
legend("topleft", legend = c("Manhattan", "Synthetic Manhattan"), fill = c("black", "red"))
dev.off()

# Store ATTs in Data Table
time_values_AQ <- unique(AQ_synthetic_control_data$month_num)
post_AQ <- time_values_AQ[gsynth_AQ$post]
Air_Quality_synthetic_control_table <- data.frame(
  Time = unique(AQ_synthetic_control_data$month_num),
  ATT = gsynth_AQ$att
)
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202405"] <- "May 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202406"] <- "June 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202407"] <- "July 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202408"] <- "Aug 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202409"] <- "Sept 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202410"] <- "Oct 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202411"] <- "Nov 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202412"] <- "Dec 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202501"] <- "Jan 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202502"] <- "Feb 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202503"] <- "March 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202504"] <- "April 2025"


# Section 4: Implement Synthetic Control for EZ Pass Data ----

# Split data into treatment and control groups
ezpass_treatment_group <- ez_pass[["ezpass_speeds_CBD"]]
ezpass_control_group <- ez_pass[["ezpass_speeds_nonCBD"]]

# Pull, organize, and aggregate necessary data
ezpass_treatment_group <- ezpass_treatment_group[, c("polyline", "week", "median_speed_fps")]
ezpass_treatment_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_treatment_group, na.rm = TRUE)

ezpass_control_group <- ezpass_control_group[, c("polyline", "week", "median_speed_fps")]
ezpass_control_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_control_group, na.rm = TRUE)

# Combine the groups, assigning unique IDs for treated and control observations
ezpass_speeds_synthetic_control_data <- rbind(ezpass_control_group, ezpass_treatment_group)
ezpass_speeds_synthetic_control_data$unit_ID <- as.numeric(factor(ezpass_speeds_synthetic_control_data$polyline))

ezpass_control_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_control_group$polyline])

ezpass_treated_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_treatment_group$polyline])

ezpass_speeds_synthetic_control_data$week_num <- as.numeric(format(ezpass_speeds_synthetic_control_data$week, "%Y%m%d"))

ezpass_speeds_synthetic_control_data$D <- ifelse(
  ezpass_speeds_synthetic_control_data$unit_ID %in% ezpass_treated_IDs & ezpass_speeds_synthetic_control_data$week_num >= 20250101,
  1,
  0
)

# Implement gsynth
gsynth_ezpass <- gsynth(median_speed_fps ~ D, 
                     data = ezpass_speeds_synthetic_control_data, 
                     index = c("unit_ID", "week_num"), 
                     force = "two-way",    # Unit and Time TWFE
                     inference = "nonparametric",    # recommended when number of treated units is small
                     estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                     #se = TRUE,
                     #nboots = 1000,
                     min.T0 = 7
)

# Pull out IDs for treated units, convert week_num to date object, plot a single treated unit 
colnames(gsynth_ezpass$Y.tr)

ezpass_treated_unit <- 1
ezpass_treated_values <- gsynth_ezpass$Y.tr[, as.character(ezpass_treated_unit)]
ezpass_counterfactual_values <- gsynth_ezpass$Y.ct[, as.character(ezpass_treated_unit)]

time_label <- rownames(gsynth_ezpass$Y.tr)
time_label <- as.Date(time_label, "%Y%m%d")

png(filename = "../output/EZPass_Speeds_Synthetic_Control.png", width = 800, height = 600)
plot(time_label, ezpass_treated_values, type = "l", main = "EZ Pass Speeds Synthetic Control", xlab = "Time (weeks)", ylab = "Speed (fps)")
lines(time_label, ezpass_counterfactual_values, col = "red", type = "l", lty = 2)
abline(v = time_label[time_label == as.Date("2025-01-06")])
legend("topleft", legend = c("Manhattan", "Synthetic Manhattan"), fill = c("black", "red"))
dev.off()

# Store ATTs in Data Table
ezpass_time_values <- unique(ezpass_speeds_synthetic_control_data$week_num)
ezpass_post <- ezpass_time_values[gsynth_ezpass$post]
EZPass_Speeds_synthetic_control_table <- data.frame(
  Week_Number = unique(ezpass_speeds_synthetic_control_data$week_num),
  ATT = gsynth_ezpass$att
)

EZPass_Speeds_synthetic_control_table$Week_Number <- as.character(EZPass_Speeds_synthetic_control_table$Week_Number)
EZPass_Speeds_synthetic_control_table$Week_Number <- as.Date(EZPass_Speeds_synthetic_control_table$Week_Number, "%Y%m%d")

EZPass_Speeds_synthetic_control_table$Time <- format(EZPass_Speeds_synthetic_control_table$Week_Number, "%b %Y")

