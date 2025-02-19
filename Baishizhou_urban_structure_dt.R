# Load necessary libraries
install.packages(c("osmdata", "osrm", "geosphere"))
install.packages("dplyr")
options(timeout = 1200)
library(sf)
library(osmdata)
library(osrm)
library(geosphere)
library(dbplyr)
library(tidyverse)
# Define coordinates of the target village (Baishizhou Village, coordinate on Baishizhou Dalou)
BSZ_DL <- st_sfc(st_point(c(113.969,22.5329)), crs = 4326)
# Get public transport station locations from OpenStreetMap
bbox_shenzhen <- c(113.75, 22.45, 114.5, 22.85)  # Approximate bounding box for Shenzhen 
stations <- opq(bbox = bbox_shenzhen) %>%
  add_osm_feature(key = "public_transport", value = "station") %>%
  osmdata_sf()

# Extract station points
station_points <- stations$osm_points

# Check if stations were retrieved
if (nrow(station_points) == 0) {
  stop("No public transport stations found in the area. Check your query.")
}

# Transform the target village point to match station coordinates CRS
BSZ_DL_crs<- st_transform(BSZ_DL, st_crs(station_points))

# Compute distance from target village to each station
distances <- st_distance(BSZ_DL_crs, station_points)

# Convert distances to kilometers
distances_km <- as.numeric(distances) / 1000 

# Add distances to station data
station_points$distance_km <- distances_km

# Filter stations within 1.5 km
stations_within_1.5km <- station_points[distances_km <= 1.5, ]
stations_within_1.5km <- stations_within_1.5km %>% drop_na(name)
# Print results
print(stations_within_1.5km)

# Visualize the data
install.packages("ggplot2")
library(ggplot2)

ggplot() +
  geom_sf(data = stations_within_1.5km, color = "blue", size = 2, alpha = 0.5) +
  geom_sf(data = BSZ_DL, color = "red", size = 3) +
  theme_minimal()

# Satellite layer
install.packages("/Applications/rua2333/EverythingAbtPhD/PhD2.0/Research/Research_Code/terra_1.8-21.tgz", repos = NULL, type = "source")
library(terra)
install.packages("tmap", dependencies=TRUE)
library(tmap)
tmap_mode("plot")
tm_shape(stations_within_1.5km) +
  tm_bubbles(size = 0.3, col = "red") +
  tm_shape(BSZ_DL_crs) +
  tm_symbols(size = 0.4, col = "blue") +
  tm_basemap(server = "Esri.WorldImagery")


# Ensure both datasets are in the same projection (e.g., EPSG:4326 for lon/lat)
station_points_df <- st_transform(station_points, 3857)  # Convert to meters (Web Mercator)
BSZ_DL_df <- st_transform(BSZ_DL, 3857)  # Convert to meters

# Calculate distances from target village to each station
station_points_df <- station_points_df %>%
  mutate(distance_m = st_distance(geometry, st_geometry(BSZ_DL_df)))

# Convert distance column to numeric (from units class)
station_points_df$distance_m <- as.numeric(station_points_df$distance_m)

# Filter stations within 1.5 km (1500 meters)
nearby_stations <- station_points_df %>%
  filter(distance_m <= 1500) %>%
  arrange(distance_m)  # Sort by closest station

# Select relevant columns
nearby_stations_df <- nearby_stations %>%
  st_drop_geometry() %>%
  select(name, distance_m) %>% drop_na(name) # Keep only station name and distance

# Print result
print(nearby_stations_df)


# Calculate distances (straight-line)
village_matrix <- matrix(c(village$lon, village$lat), ncol = 2)
distances <- distHaversine(village_matrix, station_df)

# Find the nearest station
nearest_station_index <- which.min(distances)
nearest_station_distance <- distances[nearest_station_index]

# Print result
print(paste("Nearest station is", nearest_station_distance, "meters away"))
