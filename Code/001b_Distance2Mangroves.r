## Distance to Mangroves
gdb_path <- paste0(datadir,"\\Otis.gdb")
Guerrero_Gridcells_Mangroves_withwater <- readRDS("Guerrero_Gridcells_Mangroves_nowater.rds")

## Create Mangrove Rasters with Same Resolutions (start)
    mangroves <- st_read(paste0(datadir,"\\MangrovesGuerrero.shp"))
    mangrove_acapulco3 <- raster(paste0(datadir,"\\gmw_v3_2020\\GMW_N17W101_2020_v3.tif"))
    mangrove_acapulco4 <- raster(paste0(datadir,"\\gmw_v3_2020\\GMW_N17W100_2020_v3.tif"))
    mangrove_acapulco <- merge(mangrove_acapulco3,mangrove_acapulco4)
    Manglares_Mex <- st_read(gdb_path, layer = "ManglaresdeMéxi_PairwiseClip")
    Manglares_Mex <- st_transform(Manglares_Mex, crs = crs(mangrove_acapulco))
    binary_raster <- raster(extent(mangrove_acapulco), 
                        resolution = res(mangrove_acapulco), 
                        crs = crs(mangrove_acapulco), 
                        ncol = ncol(mangrove_acapulco), 
                        nrow = nrow(mangrove_acapulco))
    #binary_raster <- rasterize(Manglares_Mex %>% filter(Mngl_2020=="Distribucion del Manglar en 2020"), binary_raster, field = 1, background = NA)
    #writeRaster(binary_raster, "Manglares_Mex_2020.tif", format = "GTiff", overwrite = TRUE)
    plot(binary_raster)
    plot(mangrove_acapulco)
    #mangrove_acapulco_final <- merge(mangrove_acapulco, binary_raster)
    #mangrove_acapulco_final <- raster("mangrove_acapulco_final_2020.tif")
    mangrove_acapulco_final <-  raster("Manglares_Mex_2020.tif")
    binary_raster <- raster(extent(mangrove_acapulco), 
                        resolution = res(mangrove_acapulco), 
                        crs = crs(mangrove_acapulco), 
                        ncol = ncol(mangrove_acapulco), 
                        nrow = nrow(mangrove_acapulco))

    #binary_raster81 <- rasterize(Manglares_Mex %>% filter(Mngl_1981=="Distribucion del Manglar en 1981"), binary_raster, field = 1, background = 0)
    #writeRaster(binary_raster81, "HistMang_Raster_1981.tif", format = "GTiff", overwrite = TRUE)
    
        # Reclassify mangrove_acapulco_final to have 1 for presence and NA for absence
        reclass_matrix <- matrix(c(-Inf, 0, NA, 0.00001, Inf, 1), ncol = 3, byrow = TRUE)
        mangrove_acapulco_final_reclass <- reclassify(mangrove_acapulco_final, reclass_matrix)
        writeRaster(mangrove_acapulco_final_reclass, "mangrove_acapulco_final_2020_NoGMW.tif", format = "GTiff", overwrite = TRUE)
    

        # Reclassify binary_raster81 to have 1 for presence and NA for absence
        binary_raster81 <- raster("HistMang_Raster_1981.tif")
        binary_raster81_reclass <- reclassify(binary_raster81, reclass_matrix)
                
            # Combine the two rasters by keeping the maximum value (1 or NA)
        mangrove_acapulco_past_present <- overlay(mangrove_acapulco_final_reclass, binary_raster81_reclass, fun = function(x, y) {
        return(ifelse(!is.na(x) | !is.na(y), 1, NA))
        })

    #writeRaster(mangrove_acapulco_past_present, "mangrove_acapulco_past_present_NoGMW.tif", format = "GTiff", overwrite = TRUE)
    glimpse(mangrove_acapulco_past_present)

    # Convert raster layers to data frames
    raster_df_past_present <- as.data.frame(rasterToPoints(mangrove_acapulco_past_present), xy = TRUE)
    glimpse(raster_df_past_present)
    raster_df_final <- as.data.frame(rasterToPoints(mangrove_acapulco_final_reclass), xy = TRUE)
    glimpse(raster_df_final)

   
    ggplot() +
  geom_raster(data = raster_df_past_present %>% filter(layer==1),
              aes(x = x, y = y),
              alpha = 0.5,fill="red") +
  geom_raster(data = raster_df_final %>% filter(Manglares_Mex_2020==1),
             aes(x = x, y = y),
             alpha = 0.5, fill="green") +
    # geom_raster(data = Manglares_Mex %>% filter(Mngl_2020=="Distribucion del Manglar en 2020"),
    #          aes(x = x, y = y),
    #          alpha = 0.5, fill="green") +
  labs(title = "Overlay of Mangrove Layers: Past and Final",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

## Create Mangrove Rasters with Same Resolutions (end)
    
## Get Distance to Mangrove PAST PRESENT
    Guerrero_Gridcells_Mangroves_withwater <- st_transform(Guerrero_Gridcells_Mangroves_withwater, crs = crs(mangrove_acapulco_past_present))
    non_empty_cells <- raster::rasterToPoints(mangrove_acapulco_past_present, fun = function(x) { x == 1 }, spatial = TRUE)
    non_empty_points <- st_as_sf(non_empty_cells)
    mangrove_binary <- reclassify(mangrove_acapulco_past_present, cbind(0, NA))
    mangrove_binary_terra <- terra::rast(mangrove_binary)

    Guerrero_Gridcells_Mangroves_withwater <- st_transform(Guerrero_Gridcells_Mangroves_withwater, crs = crs(mangrove_binary))
    centroids <- centroids(vect(Guerrero_Gridcells_Mangroves_withwater))
    extracted_values <- extract(mangrove_binary_terra, centroids)


    centroids$extracted_value <- extracted_values[, 2]
    centroids_with_mangrove <- centroids[!is.na(centroids$extracted_value) & centroids$extracted_value == 1, ]
    centroids_without_mangrove <- centroids[is.na(centroids$extracted_value) | centroids$extracted_value == 0, ]


    # Calculate the distance from centroids without mangroves to the nearest centroid with mangroves
    distances <- st_distance(st_as_sf(centroids_without_mangrove), st_as_sf(centroids_with_mangrove))

    # Get the minimum distance for each centroid without mangroves
    min_distances <- apply(distances, 1, min)


    # Add the minimum distances back to the original centroids
    centroids_without_mangrove$distance_to_nearest_mangrove <- min_distances

    centroids_without_mangrove_df <- as.data.frame(centroids_without_mangrove)

    Guerrero_Gridcells_Mangroves_withwater <- Guerrero_Gridcells_Mangroves_withwater %>% 
    left_join(centroids_without_mangrove_df %>% select("GRID_ID","distance_to_nearest_mangrove")) %>% 
    mutate(distance_to_nearest_mangrove = ifelse(is.na(distance_to_nearest_mangrove),0,distance_to_nearest_mangrove))

## 
    mangrove_acapulco_present <- raster("mangrove_acapulco_final_2020_noGMW.tif")
    non_empty_cells <- raster::rasterToPoints(mangrove_acapulco_present, fun = function(x) { x == 1 }, spatial = TRUE)
    non_empty_points <- st_as_sf(non_empty_cells)
    mangrove_binary <- reclassify(mangrove_acapulco_present, cbind(0, NA))
    mangrove_binary_terra <- terra::rast(mangrove_binary)

    centroids <- centroids(vect(Guerrero_Gridcells_Mangroves_withwater))
    extracted_values <- extract(mangrove_binary_terra, centroids)


    centroids$extracted_value <- extracted_values[, 2]
    centroids_with_mangrove <- centroids[!is.na(centroids$extracted_value) & centroids$extracted_value == 1, ]
    centroids_without_mangrove <- centroids[is.na(centroids$extracted_value) | centroids$extracted_value == 0, ]


    # Calculate the distance from centroids without mangroves to the nearest centroid with mangroves
    distances <- st_distance(st_as_sf(centroids_without_mangrove), st_as_sf(centroids_with_mangrove))

    # Get the minimum distance for each centroid without mangroves
    min_distances <- apply(distances, 1, min)


    # Add the minimum distances back to the original centroids
    centroids_without_mangrove$distance_to_nearest_mangrove_present <- min_distances

    centroids_without_mangrove_df <- as.data.frame(centroids_without_mangrove)

    Guerrero_Gridcells_Mangroves_withwater <- Guerrero_Gridcells_Mangroves_withwater %>% 
    left_join(centroids_without_mangrove_df %>% select("GRID_ID","distance_to_nearest_mangrove_present")) %>% 
    mutate(distance_to_nearest_mangrove_present = ifelse(is.na(distance_to_nearest_mangrove_present),0,distance_to_nearest_mangrove_present))

##

    glimpse(Guerrero_Gridcells_Mangroves_withwater)
    Guerrero_Gridcells_Mangroves_withwater <- Guerrero_Gridcells_Mangroves_withwater %>% 
        select(-Dist2Mang) %>% 
        rename(Dist2Mang_meters = distance_to_nearest_mangrove_present, 
            Dist2Mang_hist_meters = distance_to_nearest_mangrove)

#saveRDS(Guerrero_Gridcells_Mangroves_withwater, file = "Guerrero_Gridcells_Mangroves_withwater.rds")
#Guerrero_Gridcells_Mangroves_withwater <- readRDS("Guerrero_Gridcells_Mangroves_withwater.rds")


Guerrero_Gridcells_Mangroves <- readRDS("Guerrero_Gridcells_Mangroves.rds")
glimpse(Guerrero_Gridcells_Mangroves)


Guerrero_Gridcells_Mangroves <- Guerrero_Gridcells_Mangroves %>% 
    select(-Dist2Mang_hist_meters,-Dist2Mang_meters) %>% 
    left_join(Guerrero_Gridcells_Mangroves_withwater %>% st_drop_geometry() %>% select(GRID_ID,Dist2Mang_hist_meters,Dist2Mang_meters))



#saveRDS(Guerrero_Gridcells_Mangroves,"Guerrero_Gridcells_Mangroves.rds")


### END



glimpse(Guerrero_Gridcells_Mangroves_withwater)
    ggplot(Guerrero_Gridcells_Mangroves_withwater)+
    geom_point(aes(x=Dist2Mang_hist_meters/1000,y=Dist2Mang_meters/1000,col=Dist2Coast),alpha=0.5)+
    geom_abline(intercept = 0, slope = 1, color = "steelblue", linetype = "solid", size = 1) +
     labs(title = "Comparison of Historical and Current Distance to Mangroves",
       x = "Historical Distance to Mangroves (km)",
       col = "Distance to \nCoastline (km)",
       y = "Current Distance to Mangroves (km)") +
    theme_minimal() +
    scale_color_scico()+
    theme(
        plot.title = element_text(hjust = 0.5)
    )

ggsave("Figures/Guerrero_Gridcells_Mangroves_withwater_2.png")


# ggplot(Guerrero_Gridcells_Mangroves_withwater)+
# geom_sf(aes(fill=Dist2Mang_meters),col="transparent") +
# scale_fill_scico(palette="lajolla")

# ggplot(Guerrero_Gridcells_Mangroves_withwater)+
# geom_sf(aes(fill=distance_to_nearest_mangrove_present-distance_to_nearest_mangrove),col="transparent") +
# scale_fill_scico(palette="lajolla") + 
# labs(fill="Added Distance to Mangroves")

# #windows()
# ggplot(Guerrero_Gridcells_Mangroves_withwater)+
# geom_sf(aes(fill=Dist2Mang_hist_meters),col="transparent") +
# scale_fill_scico(palette="lajolla")

# #windows()
# ggplot(Guerrero_Gridcells_Mangroves_withwater)+
# geom_sf(aes(fill=Dist2Mang_meters - Dist2Mang_hist_meters),col="transparent") +
# #scale_fill_scico(palette="berlin",direction=-1)
# scale_fill_gradient2(low="blue",high="red",midpoint=0)

#             ggsave("Guerrero_Gridcells_Mangroves_withwater_2.png")












# glimpse(Guerrero_Gridcells_Mangroves_withwater)
# st_crs(Guerrero_Gridcells_Mangroves_withwater)
# crs(mangrove_binary_terra)
#     extent_guerrero <- extent(Guerrero_Gridcells_Mangroves_withwater)
#     cropped_binary_raster <- crop(mangrove_binary, extent_guerrero)
#     plot(cropped_binary_raster)
#     #masked_binary_raster <- mask(cropped_binary_raster, Guerrero_Gridcells_Mangroves_withwater)
#     #plot(masked_binary_raster)

#     # Convert the raster to a terra object
# mangrove_binary_terra <- terra::rast(cropped_binary_raster)

# # Convert the Guerrero grid cells to a terra SpatVector
# Guerrero_vect <- vect(Guerrero_Gridcells_Mangroves_withwater)
# dist_raster <- terra::distance(mangrove_binary_terra)



#     #dist_raster <- distance(masked_binary_raster)

    
#     centroids <- st_centroid(Guerrero_Gridcells_Mangroves)
#     # Extract the distance for each centroid
# centroid_coords <- st_coordinates(centroids)
# Guerrero_Gridcells_Mangroves$distance_to_mangroves <- extract(dist_raster, centroid_coords)
