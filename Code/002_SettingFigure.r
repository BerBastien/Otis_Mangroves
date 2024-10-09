
    
    polygon_grid_master <- st_read(paste0(datadir,"/PolygonLayer.shp"))
    gdb_path <- paste0(datadir,"\\Otis.gdb")
    Guer_Mangnw <- readRDS(file = "Guerrero_Gridcells_Mangroves_withwater.rds")
    Guer_Mang <- readRDS(file = "Guerrero_Gridcells_Mangroves.rds")
    Manglares_Mex <- st_read(gdb_path, layer = "ManglaresdeMéxi_PairwiseClip")
    mangrove_acapulco_past_present <- raster("mangrove_acapulco_past_present_NoGMW.tif")
    mangrove_acapulco_final <- raster("mangrove_acapulco_final_2020_NoGMW.tif")
    # raster_df_past_present <- as.data.frame(rasterToPoints(mangrove_acapulco_past_present), xy = TRUE)
    # raster_df_final <- as.data.frame(rasterToPoints(mangrove_acapulco_final), xy = TRUE)
    # saveRDS(raster_df_past_present,"Data//raster_df_past_present.rds")
    # saveRDS(raster_df_final,"Data//raster_df_final.rds")
    raster_df_final <- readRDS("Data//raster_df_final.rds")
    raster_df_past_present <- readRDS("Data//raster_df_past_present.rds")
    glimpse( raster_df_final)
    

    bbox <- st_bbox(Guer_Mangnw)
    Guer_Mang_union <- st_union(Guer_Mangnw)
    # Calculate the convex hull (or alternatively the boundary)
    Guer_Mang_perimeter <- st_boundary(Guer_Mang_union)


    plot_m81 <- ggplot(data = Guer_Mang_perimeter) +
    annotation_map_tile(zoom = 9, type = "osm",alpha=0.2) +
    geom_sf(fill = "transparent", col = "black", alpha = 0.85) +
    
    # Add a mapping for the fill aesthetic and provide names for the legend
    # geom_sf(data = Manglares_Mex %>% filter(Mngl_1981 == "Distribucion del Manglar en 1981"),
    #         aes(fill = "Mangroves 1981"), col = "transparent") +
    # geom_sf(data = Manglares_Mex %>% filter(Mngl_2020 == "Distribucion del Manglar en 2020"),
    #         aes(fill = "Mangroves 2020"), col = "transparent") +

    geom_raster(data = raster_df_past_present %>% filter(mangrove_acapulco_past_present_NoGMW==1),
            aes(x = x, y = y,fill = "Mangroves 1981")) +
    geom_raster(data = raster_df_final %>% filter(mangrove_acapulco_final_2020_NoGMW==1),
              aes(x = x, y = y,fill = "Mangroves 2020")) +
    
    # Add the line with a mapping for color and provide a name for the legend
    geom_sf(data = otis_line, aes(color = "Otis Route"), linewidth = 3, alpha = 0.5) +
    
    # Manually set the colors for the fill and color scales
    scale_fill_manual(name = "", values = c("Mangroves 1981" = "indianred", "Mangroves 2020" = "darkgreen")) +
    scale_color_manual(name = "", values = c("Otis Route" = "steelblue")) +
    
    labs(title = "Mangrove Distribution and Hurricane Otis Route",
        caption = "") +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "bottom",                        # Position the legend at the bottom
        legend.direction = "horizontal",                   # Make the legend horizontal
        legend.key.width = unit(0.8, "cm"),                # Adjust width of the legend color bar
        legend.key.height = unit(0.5, "cm")                # Adjust height of the legend color bar
    )
  plot_m81
    # Display the plot
    plot_m81


    glimpse(Guer_Mangnw)
    Guer_Mangnw_destroyed <- Guer_Mangnw %>% filter(!is.na(Percent_area_Destroyed))
    Guer_Mangnw_destroyed
    
    bbox_d <- st_bbox(Guer_Mangnw_destroyed)
    Guer_Mang_union_d <- st_union(Guer_Mangnw_destroyed)

    # Calculate the convex hull (or alternatively the boundary)
    Guer_Mang_perimeter_d <- st_boundary(Guer_Mang_union_d)
    
plot_destroyed <- ggplot(Guer_Mangnw_destroyed) +
  annotation_map_tile(zoom = 10, type = "osm", alpha = 0.2) +
  # Continuous fill for Percent_area_Destroyed
  geom_sf(data = Guer_Mangnw_destroyed, aes(fill = Percent_area_Destroyed), col = "transparent") +
  # Categorical fill with direct color setting
  geom_sf(data = otis_line, col = "steelblue", linewidth = 3, alpha = 0.5) +
  
  # Continuous scale for Percent_area_Destroyed
  scale_fill_scico(name = "Percent Area Destroyed", palette="lajolla",direction=-1,
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(3, "cm"),    # Adjust the width of the color bar
      barheight = unit(0.5, "cm")  # Adjust the height of the color bar
    )) +
  labs(title = "C. Area Destroyed",
       caption = "") +
  coord_sf(xlim = c(bbox_d["xmin"], bbox_d["xmax"]), ylim = c(bbox_d["ymin"], bbox_d["ymax"])) +
  theme_void() +
  theme(
    #plot.title = element_text(size = 4, hjust = 0.5),  # Smaller title size
    #legend.title = element_text(size = 3),             # Smaller legend title size
    #legend.text = element_text(size = 2),               # Smaller legend text size
    legend.position = "bottom",                         # Position the legend at the bottom
    legend.direction = "horizontal"     ,  
   
    panel.border = element_rect(color = "magenta", fill = NA, size = 1) 
  )

# Display the plot
plot_destroyed

glimpse(Guer_Mangnw)
Guer_Mangnw_house <- Guer_Mangnw %>% filter(!is.na(damaged_households))
bbox_h <- st_bbox(Guer_Mangnw_house)

plot_households <- ggplot(Guer_Mangnw_house) +
  annotation_map_tile(zoom = 10, type = "osm", alpha = 0.2) +
  geom_sf(data = Guer_Mangnw_house, aes(fill = damaged_households), col = "transparent") +
  geom_sf(data = otis_line,col="steelblue", linewidth = 3, alpha = 0.5) +
  scale_fill_scico(name = "No. of Destroyed Households", palette="lajolla",direction=-1,
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(3, "cm"),    # Adjust the width of the color bar
      barheight = unit(0.5, "cm")  # Adjust the height of the color bar
    )) +
  labs(title = "D. Household Destruction",
       caption = "") +
  coord_sf(xlim = c(bbox_h["xmin"], bbox_h["xmax"]), ylim = c(bbox_h["ymin"], bbox_h["ymax"])) +
  theme_void()+
  theme(
    #plot.title = element_text(size = 4, hjust = 0.5),  # Smaller title size
    #legend.title = element_text(size = 3),             # Smaller legend title size
    #legend.text = element_text(size = 2),               # Smaller legend text size
    legend.position = "bottom",                         # Position the legend at the bottom
    legend.direction = "horizontal",  
   
    panel.border = element_rect(color = "darkcyan", fill = NA, size = 1)  )

# Display the plot
plot_households



glimpse(Guer_Mang)
plot_veg <- ggplot(Guer_Mang) +
  annotation_map_tile(zoom = 10, type = "osm", alpha = 0.2) +
  # Continuous fill for Percent_area_Destroyed
  geom_sf(data = Guer_Mang, aes(fill = veg_change*100), col = "transparent", alpha = 0.85) +
  geom_sf(data = otis_line,color = "steelblue", linewidth = 3, alpha = 0.5) +
  scale_fill_scico(name = "Percent Change in Vegetation", palette="berlin",direction=-1,
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,
      barwidth = unit(3, "cm"),    # Adjust the width of the color bar
      barheight = unit(0.5, "cm")  # Adjust the height of the color bar
    )) +
  labs(title = "B. Vegetation Change",
       caption = "") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void() +
  theme(
    #plot.title = element_text(size = 4, hjust = 0.5),  # Smaller title size
    #legend.title = element_text(size = 3),             # Smaller legend title size
    #legend.text = element_text(size = 2),               # Smaller legend text size
    legend.position = "bottom",                         # Position the legend at the bottom
    legend.direction = "horizontal"                     # Make the legend horizontal
  )

# Display the plot
plot_veg
bbox_h_sf <- st_as_sfc(bbox_h)
bbox_d_sf <- st_as_sfc(bbox_d)
bbox_h_centroid <- st_centroid(bbox_h_sf)
bbox_d_centroid <- st_centroid(bbox_d_sf)

plot_m81 <- ggplot(data = Guer_Mang_perimeter) +
    annotation_map_tile(zoom = 9, type = "osm", alpha = 0.2) +
    geom_sf(fill = "transparent", col = "black", alpha = 0.85) +
    
    # Add a mapping for the fill aesthetic and provide names for the legend
    # geom_sf(data = Manglares_Mex %>% filter(Mngl_1981 == "Distribucion del Manglar en 1981"),
    #         aes(fill = "Mangroves 1981"), col = "transparent") +
    # geom_sf(data = Manglares_Mex %>% filter(Mngl_2020 == "Distribucion del Manglar en 2020"),
    #         aes(fill = "Mangroves 2020"), col = "transparent") +

    geom_raster(data = raster_df_past_present %>% filter(mangrove_acapulco_past_present_NoGMW==1),
            aes(x = x, y = y,fill = "Mangroves 1981")) +
    geom_raster(data = raster_df_final %>% filter(mangrove_acapulco_final_2020_NoGMW==1),
              aes(x = x, y = y,fill = "Mangroves 2020")) +
    
    # Add the line with a mapping for color and provide a name for the legend
    geom_sf(data = otis_line, aes(color = "Otis Route"), linewidth = 3, alpha = 0.5) +
    
    # Add the bounding boxes as solid line perimeters
    geom_sf(data = bbox_h_sf, color = "darkcyan", fill = NA, linetype = "solid", size = 1) +
    geom_sf(data = bbox_d_sf, color = "magenta", fill = NA, linetype = "solid", size = 1) +
    #geom_text(data = as.data.frame(st_coordinates(bbox_h_centroid)),
     #         aes(X, Y, label = "Area in Panel D"), color = "blue", size = 3, fontface = "bold", nudge_y = 0.05) +
    #geom_text(data = as.data.frame(st_coordinates(bbox_d_centroid)),
     #         aes(X+0.08, Y, label = "Area in Panel C"), color = "magenta", size = 3, fontface = "bold", nudge_y = 0.05) +
   
    # Manually set the colors for the fill and color scales
    scale_fill_manual(name = "", values = c("Mangroves 1981" = "indianred", "Mangroves 2020" = "darkgreen")) +
    scale_color_manual(name = "", values = c("Otis Route" = "steelblue")) +
    
    labs(title = "A. Otis Route and Mangroves Historical Distribution",
        caption = "") +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        #legend.position = "right",                        # Position the legend at the bottom
        #legend.direction = "horizontal",                   # Make the legend horizontal
        legend.key.width = unit(0.8, "cm"),                # Adjust width of the legend color bar
        legend.key.height = unit(0.5, "cm")                # Adjust height of the legend color bar
    )


    plot_households2 <- plot_households + theme(plot.margin = margin(10, 10, 10, 10))
    plot_destroyed2 <- plot_destroyed + theme(plot.margin = margin(10, 10, 10, 10))
    plot_veg2 <- plot_veg + theme(plot.margin = margin(10, 10, 10, 10))

ggarrange(plot_m81,
    ggarrange(plot_veg2,plot_destroyed2,plot_households2,ncol=3,align="hv"),
        nrow=2)

ggsave("Figures/Study_Area_Mangroves_NoGMW.jpg",dpi=400)


mexico <- st_as_sf(rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf"))

# Plot the map of Mexico with the perimeter only
ggplot() +
  # Add the perimeter of Mexico
  geom_sf(data = st_boundary(mexico), fill = NA, color = "black") +
  
  geom_sf(data = Guer_Mang_union_d, fill = "red", color = "indianred",linewidth=4) +
  
  geom_sf(data = otis_line, aes(color = "Otis Route"), linewidth = 3, alpha = 0.5) +
  theme_void() 
