# Load necessary libraries
install.packages(c("osmdata", "osrm", "geosphere"))
options(timeout = 1200)
library(sf)
library(osmdata)
library(osrm)
library(geosphere)

# Define coordinates of the target village (Baishizhou Village, coordinate on Baishizhou Dalou)
village <- data.frame(lon = 113.96, lat = 22.53)  

# Get public transport station locations from OpenStreetMap
bbox_shenzhen <- c(113.75, 22.45, 114.5, 22.85)  # Approximate bounding box for Shenzhen

stations <- opq(bbox = bbox_shenzhen) %>%
  add_osm_feature(key = "public_transport", value = "station") %>%
  osmdata_sf()

# Extract station coordinates
str(stations)
station_coords <- st_coordinates(stations$osm_points)

# Convert to data frame
station_df <- data.frame(lon = station_coords[, 1], lat = station_coords[, 2])

# Calculate distances (straight-line)
village_matrix <- matrix(c(village$lon, village$lat), ncol = 2)
distances <- distHaversine(village_matrix, station_df)

# Find the nearest station
nearest_station_index <- which.min(distances)
nearest_station_distance <- distances[nearest_station_index]

# Print result
print(paste("Nearest station is", nearest_station_distance, "meters away"))
