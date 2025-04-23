## Spatial linkage
# AQI monitoring sites mapped to AQMD boundaries
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(sf)
library(ggplot2)


# (1) data calls
filepath <- "/Users/caitlin/Documents/UCSD Postdoc/Projects/AQA/Data"
years <- 1999:2021

file_syntax <- paste0("AQI data (", paste(years, collapse = "|"), ")CA")
files_to_read <- list.files(path = filepath, pattern = file_syntax, full.names = TRUE)
data_list <- lapply(files_to_read, fread)

aqi <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
aqi$Date <- mdy(aqi$Date) # read as date object

# (2) extract unique monitoring site coordinates
monitors <- unique(aqi[, c("Site ID", "Site Latitude", "Site Longitude")])


# (3) point within polygon
aqmd_sf <- st_read("/Users/caitlin/Documents/UCSD Postdoc/Projects/AQA/Data/AQMD/CaAirDistrict.shp")
monitors_sf <- st_as_sf(monitors, coords = c("Site Longitude", "Site Latitude"), crs = 4326) 
monitors_sf <- st_transform(monitors_sf, crs = 3310) # CRS is TealeAlbersNAD83

# check
ggplot() +
  geom_sf(data = aqmd_sf, fill = "lightblue", color = "black", lwd = 0.5) +  # AQMD polygons
  geom_sf(data = monitors_sf, color = "red", size = 1) +  # Monitor site points
  theme_minimal() +
  labs(title = "Air Quality Monitoring Sites and Air District Boundaries",
       subtitle = "1999-2021") +
  theme(legend.position = "none")

# link monitor id to AQMD
monitor_aqmd <- st_join(monitors_sf, aqmd_sf, join = st_intersects)
monitor_aqmd_x <- monitor_aqmd[,c(1,6)]
monitor_aqmd_x <- st_drop_geometry(monitor_aqmd_x)
colnames(monitor_aqmd_x)[2] <- "AQMD"

# (4) aggregate aqi data to AQMD level using crosswalk
aqi_aqmd <- left_join(aqi, monitor_aqmd_x, by = "Site ID")

aqi_aqmd <- aqi_aqmd %>%
  group_by(AQMD, Date) %>%
  summarize(aqmd_aqi = mean(`Daily AQI Value`),
            aqmd_pm = mean(`Daily Mean PM2.5 Concentration`),
            aqmd_dailyobs = sum(`Daily Obs Count`)
            )

# check
range(aqi_aqmd$aqmd_aqi)
mean(aqi_aqmd$aqmd_aqi)

# (5) summary statistics by air district
aqi_aqmd_summary <- aqi_aqmd %>%
  group_by(AQMD) %>%
  summarize(n_measureddays = n(),  # Total number of measured days
            n_dailyobs = sum(aqmd_dailyobs), # Total number of observations
            n_unhealthy = sum(aqmd_aqi >= 151, na.rm = TRUE),  # Count of AQI >= 151
            pct_unhealthy = (sum(aqmd_aqi >= 151, na.rm = TRUE) / n()) * 100  # Percent of AQI >= 151
            )

# (6) map summary statistics by AQMD
aqi_aqmd_sf <- left_join(aqmd_sf, aqi_aqmd_summary, by = c("NAME" = "AQMD"))

ggplot() +
  geom_sf(data = aqi_aqmd_sf, aes(fill = pct_unhealthy), color = "lightgray", linewidth = 0.0000000001) +
  scale_fill_gradient(low = "#FBCB8E", high = "darkred", na.value = "grey", name = "Percent Unhealthy AQI Days") +
  theme_minimal() +
  labs(title = "Percent of monitored days with unhealthy AQI by air district, 1999-2021") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggplot() +
  geom_sf(data = aqi_aqmd_sf, aes(fill = n_dailyobs), color = "lightgray", linewidth = 0.0000000001) +
  scale_fill_gradient(low = "#FBCB8E", high = "darkred", na.value = "grey", name = "Number of daily obs") +
  theme_minimal() +
  labs(title = "Number of daily AQI observations by air district, 1999-2021") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## additional considerations: separate summaries by year, number of monitor sites by AQMD, different thresholds of AQI
