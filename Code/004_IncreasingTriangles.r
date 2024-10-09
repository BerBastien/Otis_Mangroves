# Load the Mexico coastline
mexico <- st_as_sf(rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf"))

# Calculate the nearest point on the coastline for each point in Guer_Mang
Guer_Mang_sample <- Guer_Mang %>% filter(!is.na(Percent_area_Damaged))

# Calculate the nearest point on the coastline for each point in Guer_Mang_sample
nearest_coast <- st_nearest_points(Guer_Mang_sample$Shape, st_geometry(mexico))
nearest_points <- st_cast(nearest_coast, "POINT")[seq(2, length(nearest_coast), by = 2)]

# Sanity check: Print the first few nearest points
print(nearest_points[1:5])

# Calculate the bearing from each point to the nearest coastal point
bearings <- mapply(function(point, coast_point) {
  point_coords <- st_coordinates(point)[1, 1:2]  # Extract only the first two columns (longitude, latitude)
  coast_coords <- st_coordinates(coast_point)[1, 1:2]  # Extract only the first two columns (longitude, latitude)
  
  # Sanity check: Print the coordinates being used to calculate the bearing
  print(paste("Point:", paste(point_coords, collapse = ", "), 
              "Coast Point:", paste(coast_coords, collapse = ", ")))
  
  bearing(point_coords, coast_coords)
}, st_geometry(Guer_Mang_sample$Shape), nearest_points)



raster_sf <- st_as_sf(raster_df_final, coords = c("x", "y"), crs = st_crs(Guer_Mang_sample))

# Sanity check: Print the first few rows of the raster_sf object
print(head(raster_sf))
# Filter the non-empty points from the raster data
non_empty_points <- raster_sf %>%
  filter(!is.na(mangrove_acapulco_final_2020_NoGMW) & mangrove_acapulco_final_2020_NoGMW != 0)


Dist <- c(1,3,6,9,12)*1000
# Define the distance for the triangle's legs (e.g., 1 km)
for (distance in Dist){


# Create the triangles for each point, returning a list of polygons
triangles <- mapply(function(point, bearing) {
  create_triangle(point, bearing, distance)
}, st_geometry(Guer_Mang_sample$Shape), bearings, SIMPLIFY = FALSE)

# Combine the list of polygons into a single sfc object
triangles_sfc <- st_sfc(triangles, crs = st_crs(Guer_Mang_sample))

# Convert to an sf object
triangles_sf <- st_sf(geometry = triangles_sfc)


# ggplot() +
#   geom_sf(data = Guer_Mang_perimeter, fill = NA, color = "black") +
#   geom_sf(data = triangles_sf, fill = "red", color = "red", alpha = 0.5) +
#   theme_void() +
#   labs(title = "Triangular Polygons Pointing Towards Nearest Coastline")


# Count the number of non-empty points within each triangle
point_counts <- sapply(st_geometry(triangles_sf), function(triangle) {
  sum(st_intersects(triangle, st_geometry(non_empty_points), sparse = FALSE))
})

new_var_name <- paste0("mangroves_triangle_", distance)
print(new_var_name)
Guer_Mang_sample[[new_var_name]] <- point_counts
}

glimpse(Guer_Mang_sample)
saveRDS(Guer_Mang_sample,"Data/Guer_Mang_Damaged.RDS")
ggplot(Guer_Mang_sample,aes(x=mangroves_triangle_1000,y=mangroves_triangle_3000))+geom_point()
summary(felm(Percent_area_Damaged ~ Dist2Coast*mangroves_triangle_1000|0|0|0,data=Guer_Mang_sample))
summary(felm(Percent_area_Damaged ~ Dist2Coast*mangroves_triangle_3000|0|0|0,data=Guer_Mang_sample))
summary(felm(Percent_area_Damaged ~ Dist2Coast*mangroves_triangle_6000|0|0|0,data=Guer_Mang_sample))
summary(felm(Percent_area_Damaged ~ Dist2Coast*mangroves_triangle_9000|0|0|0,data=Guer_Mang_sample))
summary(felm(Percent_area_Damaged ~ Dist2Coast*mangroves_triangle_12000|0|0|0,data=Guer_Mang_sample))

# Sanity check: Print the first few triangles with their counts
print(head(triangles_sf))
glimpse(triangles_sf)
glimpse(Guer_Mang_sample)
ggplot() +
  #geom_sf(data = mexico, fill = NA, color = "black") +
  geom_sf(data = triangles_sf, aes(fill = non_empty_points), color = "transparent", alpha = 0.5) +
  scale_fill_viridis_c(option = "magma") +
  theme_void() +
  labs(title = "Triangles with Non-Empty Raster Points",
       fill = "Number of Points")
