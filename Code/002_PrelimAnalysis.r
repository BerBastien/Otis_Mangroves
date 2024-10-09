    Guer_Mang <- readRDS(file = "Guerrero_Gridcells_Mangroves.rds")
    glimpse(Guer_Mang)

   
    Guer_Mang <- Guer_Mang %>%
          #filter(NEAR_DIST<6) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")))
    
    plot_d2m <- ggplot(data = Guer_Mang) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = Dist2Mang), col = "transparent", alpha = 0.85) +
        scale_fill_viridis_c(option = "viridis", guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
        labs(title = "Distance to Mangroves",
            fill = "Distance (km)",
            caption = "") +
        theme_void() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            legend.position = "bottom",                        # Position the legend at the bottom
            legend.direction = "horizontal",                   # Make the legend horizontal
            legend.key.width = unit(0.8, "cm"),                  # Adjust width of the legend color bar
            legend.key.height = unit(0.5, "cm")                # Adjust height of the legend color bar
        )
    #plot_d2m
    
    
    # plot_d2c <- ggplot(data = Guer_Mang) +
    #       annotation_map_tile(zoom = 13, type = "osm") +
    #       geom_sf(aes(fill = Dist2Coast),col="transparent",alpha=0.85) +
    #       scale_fill_viridis_c(option = "viridis") +
    #       labs(title = "Distance to Coastline",
    #            fill = "Distance (km)",
    #            caption = "") +
    #       theme_void() +
    #       theme(
    #         plot.title = element_text(hjust = 0.5),
    #         plot.caption = element_text(hjust = 0.5)
    #       )

    plot_d2c_bin <- ggplot(data = Guer_Mang) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = Dist2Coast_bin), col = "transparent", alpha = 0.85) +
        scale_fill_scico_d(begin = 0.1, end = 0.9, 
                            guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
        labs(title = "Distance to Coastline",
            fill = "Distance bin",
            caption = "") +
        theme_void() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            legend.position = "bottom",                        # Position the legend at the bottom
            legend.direction = "horizontal",                   # Make the legend horizontal
            legend.key.width = unit(0.5, "cm"),                  # Adjust width of the legend key
            legend.key.height = unit(0.5, "cm")                # Adjust height of the legend key
        )

    plot_veg <- ggplot(data = Guer_Mang) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = 100 * veg_change), col = "transparent", alpha = 0.85) +
        scale_fill_gradient2(low = "indianred", mid = "white", high = "darkgreen", 
                            midpoint = 0,
                            oob = scales::squish,
                            guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
        labs(title = "Vegetation Change",
            fill = "(% Change)",
            caption = "") +
        theme_void() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            legend.position = "bottom",                        # Position the legend at the bottom
            legend.direction = "horizontal",                   # Make the legend horizontal
            legend.key.width = unit(0.8, "cm"),                  # Adjust width of the legend key
            legend.key.height = unit(0.5, "cm")                # Adjust height of the legend key
        )

    veg_dist_scatter_bins <- ggplot(Guer_Mang %>% filter(veg_change!=0),aes(x=Dist2Mang,y=100*veg_change,col=Dist2Coast_bin)) +
        geom_point(alpha=0.1) + 
        scale_color_scico_d(begin=0.1,end=0.9) + 
        geom_smooth() + 
        theme_minimal() + 
        coord_cartesian(ylim = c(-40, 5)) + 
        labs(color="Distance to Coast",y="Vegetation Change \nAfter Hurricane Otis (%)",x = "Distance to Closest Mangrove (km)") 



    # veg_dist_scatter <- ggplot(Guer_Mang %>% filter(veg_change!=0),aes(x=Dist2Mang,y=100*veg_change)) +
    #     geom_point(alpha=0.1) + 
    #     scale_color_scico_d(begin=0.1,end=0.9) + 
    #     geom_smooth() + 
    #     theme_minimal() + 
    #     coord_cartesian(ylim = c(-50, 12.50)) + 
    #     labs(color="Distance to Coast")

    plot_Guerrero_sc <- ggarrange(
                            
                            ggarrange(plot_d2c_bin,plot_d2m,plot_veg,ncol=1,align="hv"),veg_dist_scatter_bins,
                            ncol=2,widths=c(2,3)) 
    plot_Guerrero_sc
    #ggsave("Figures/VegChange_Dist2Coast_Dist2Mang.png",dpi=600)
    # Adjust the margin of each plot
    plot_d2c_bin2 <- plot_d2c_bin + theme(plot.margin = margin(10, 10, 10, 10))
    plot_d2m2 <- plot_d2m + theme(plot.margin = margin(10, 10, 10, 10))
    plot_veg2 <- plot_veg + theme(plot.margin = margin(10, 10, 10, 10))

    plot_Guerrero_sc <- ggarrange(
                            
                            ggarrange(plot_d2c_bin2,plot_d2m2,plot_veg2,ncol=3,align="hv"),veg_dist_scatter_bins,
                            ncol=1,widths=c(2,3)) 
    plot_Guerrero_sc
    ggsave("Figures/VegChange_Dist2Coast_Dist2Mang_horizontal.png",dpi=600)



plot_data <- ggplot_build(veg_dist_scatter_bins)$data[[2]]# Calculate the first derivative (difference between consecutive y values)
first_derivative <- diff(plot_data$y) / diff(plot_data$x)

# Find the index where the first derivative changes from negative to positive
local_min_index <- which(diff(sign(first_derivative)) == 2) + 1

# Identify the local minima point
local_min_point <- plot_data[local_min_index, ]
local_min_df <- data.frame(
  x = local_min_point$x,
  y = local_min_point$y,
  label = paste("Local Minima:", round(local_min_point$x, 2))
)

local_min_df <- local_min_df[-c(1),]
arrow_df <- data.frame(
  x = local_min_point$x,
  y = local_min_point$y - 4,   # Start the arrow slightly below the point
  xend = local_min_point$x,
  yend = local_min_point$y-0.2
)

veg_dist_scatter_bins_annotations <- veg_dist_scatter_bins +
  geom_segment(data = arrow_df, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")),linewidth=1, color = "indianred")
  
  plot_Guerrero_sc <- ggarrange(
                            
                            ggarrange(plot_d2c_bin2,plot_d2m2,plot_veg2,ncol=3,align="hv"),veg_dist_scatter_bins_annotations,
                            ncol=1,widths=c(2,3)) 
    plot_Guerrero_sc
    
    ggsave("Figures/VegChange_Dist2Coast_Dist2Mang_horizontal.jpg",dpi=600)










    
    ggplot(Guer_Mang %>% filter(veg_change != 0), 
                                aes(x = Dist2Mang, y = 100 * veg_change, col = Dist2Coast_bin)) +
  geom_point(alpha = 0.1) + 
  scale_color_scico_d(begin = 0.1, end = 0.9) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) + 
  theme_minimal() + 
  coord_cartesian(ylim = c(-50, 12.50)) + 
  labs(color = "Distance to Coast", 
       y = "Vegetation Change \nAfter Hurricane Otis (%)", 
       x = "Distance to Closest Mangrove (km)")


mexico <- ne_countries(scale = "medium", country = "Mexico", returnclass = "sf")

# Calculate the centroid of the Guer_Mang object
Guer_Mang_centroid <- st_centroid(st_union(Guer_Mang))

# Plot the map with a star at the centroid
map_mex <- ggplot() +
  geom_sf(data = mexico, fill = "white", color = "black") +
  geom_sf(data = Guer_Mang_centroid, shape = 18, color = "red", size = 15) +
  labs(title = "Map of Mexico with Guer_Mang Centroid") +
  theme_minimal() +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", color = NA)) 

map_grob <- ggplotGrob(map_mex)

plot_d2c_bin_with_map <- plot_d2c_bin +
  annotation_custom(
    grob = map_grob ) +
  theme(plot.margin = margin(0, 10, 0, 10, "cm"))
    
plot_d2c_bin_with_map

# Combine the plots as before, but using the modified first plot
plot_Guerrero_sc <- ggarrange(
  ggarrange(
    plot_d2c_bin_with_map,          # The first plot with the overlay
    plot_d2m,                       # Your second plot
    plot_veg,                       # Your third plot
    ncol = 1, align = "hv"
  ),
  veg_dist_scatter_bins,            # Your scatter plot
  ncol = 2, widths = c(2, 3)
)






    summary(felm(veg_change~Dist2Mang:Dist2Coast_bin + I(Dist2Mang^2):Dist2Coast_bin| 0|0|0,Guer_Mang ))

    # Create a sequence of values for Dist2Mang
Dist2Mang_seq <- seq(min(Guer_Mang$Dist2Mang, na.rm = TRUE), max(Guer_Mang$Dist2Mang, na.rm = TRUE), length.out = 100)

# Create a data frame for predictions
prediction_data <- expand.grid(Dist2Mang = Dist2Mang_seq,
                               Dist2Coast_bin = unique(Guer_Mang$Dist2Coast_bin))

# Add predictions based on the model coefficients
prediction_data <- prediction_data %>%
  mutate(veg_change_pred = -0.1202863 +
           -0.0398034 * Dist2Mang +
           0.0052692 * I(Dist2Mang^2) +
           ifelse(Dist2Coast_bin == "Intermediate", -0.0090042 + 0.0269972 * Dist2Mang + -0.0036126 * I(Dist2Mang^2),
           ifelse(Dist2Coast_bin == "Far", 0.0825855 + 0.0146678 * Dist2Mang + -0.0034208 * I(Dist2Mang^2), 0))
         )

# Plot the predicted veg_change across Dist2Mang for different Dist2Coast_bin categories
ggplot(prediction_data, aes(x = Dist2Mang, y = veg_change_pred, color = Dist2Coast_bin)) +
  geom_line(size = 1) +
  labs(title = "Predicted Vegetation Change by Distance to Mangroves and Coast",
       x = "Distance to Mangroves",
       y = "Predicted Vegetation Change",
       color = "Distance to Coast") +
  theme_minimal()

    


summary(felm(Percent_area_Destroyed ~ Dist2Mang+Dist2Coast|0|0,data=Guer_Mang))
    
    ggplot(Guer_Mang %>% filter(!is.na(Percent_area_Destroyed)), aes(x=Dist2Mang,y=Dist2Coast,color=Percent_area_Destroyed))+
        geom_point() + 
        geom_point(data = Guer_Mang %>% filter(Mangrove_Pixels>0 & !is.na(Percent_area_Destroyed)),
            aes(x=Dist2Mang,y=Dist2Coast),col="red")


        # Create bins for NEAR_DIST
        polygon_grid3 <- polygon_grid3 %>%
          filter(NEAR_DIST<6) %>%
          mutate(NEAR_DIST_BIN = cut(NEAR_DIST, breaks = 3, labels = c("Cercano", "Intermedio", "Lejano")))
        
        # Calculate the average of Percent_area_Possibly within each bin
        averaged_data <- polygon_grid3 %>%
          group_by(NEAR_DIST_BIN) %>%
          summarize(Avg_Percent_P = mean(Percent_area_Possibly, na.rm = TRUE), 
                    Avg_Percent_Dam = mean(Percent_area_Damaged, na.rm = TRUE), 
                    Avg_Percent_Des = mean(Percent_area_Destroyed, na.rm = TRUE))
        glimpse(averaged_data)
        
        # Reshape data to long format
        long_data <- averaged_data %>%
          pivot_longer(cols = c(Avg_Percent_P, Avg_Percent_Dam, Avg_Percent_Des), 
                       names_to = "Damage_Type", 
                       values_to = "Percent")
        
        # Create a more descriptive label for the Damage_Type
        long_data$Damage_Type <- recode(long_data$Damage_Type,
                                        "Avg_Percent_P" = "Possibly Damaged",
                                        "Avg_Percent_Dam" = "Damaged",
                                        "Avg_Percent_Des" = "Destroyed")
        
        # Reorder the factors
        long_data$Damage_Type <- fct_relevel(long_data$Damage_Type, "Possibly Damaged", "Damaged", "Destroyed")
        
        # Define a custom color palette
        custom_colors <- c("Possibly Damaged" = "yellow", "Damaged" = "orange","Destroyed" = "red" )
        
        # Plot the results
        ggplot(long_data, aes(x = NEAR_DIST_BIN, y = Percent, fill = Damage_Type)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
          scale_fill_manual(values = custom_colors) +
          labs(x = "Distancia al Manglar", y = "Porcentaje de Daño", 
               fill = "Tipo de Daño") +
          theme_minimal() +
          theme(
            legend.position = "right",
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10)
          )
        
        ggplot(polygon_grid3) +
          xlim(c(0, 5)) +
          geom_area(aes(x = NEAR_DIST, y = Percent_area_Possibly), fill = "green", alpha = 0.3) +
          labs(x = "NEAR_DIST", y = "Density")
        
        ggplot(polygon_grid3) +
          xlim(c(0,5))+
          geom_density(aes(y=Percent_area_Possibly,x = NEAR_DIST),
                       col="green",alpha=0.3,stat = "identity")+
          #geom_point(aes(y=Percent_area_Possibly,x = NEAR_DIST),col="green",alpha=0.3)+
          geom_point(aes(y=Percent_area_Damaged,x = NEAR_DIST),col="yellow",alpha=0.3)+
          geom_point(aes(y=Percent_area_Destroyed,x = NEAR_DIST),col="red",alpha=0.3)
        
        
####### END
          
          ggplot(polygon_grid) +
            geom_point(aes(y=Percent_area_Possibly,x = NEAR_DIST),col="green",alpha=0.3)+
            geom_point(aes(y=Percent_area_Damaged,x = NEAR_DIST),col="yellow",alpha=0.3)+
            geom_point(aes(y=Percent_area_Destroyed,x = NEAR_DIST),col="red",alpha=0.3)
          
          polygon_grid %>% 
            filter(Percent_area_Damaged > 101) %>% 
            select(GRID_ID)
          
          # ---- Damaged Roads ----
      
      
          points_in_polygons <- st_join(damaged_roads, polygon_grid, join = st_within)
          
          counts <- points_in_polygons %>%
            group_by(GRID_ID) %>%
            summarize(damaged_roads = n()) %>% dplyr::select(-GRID_ID)
          
          polygon_grid <- st_join(polygon_grid, counts)
        
      # ---- Damaged Bridges ----
        
        
        points_in_polygons <- st_join(damaged_bridges, polygon_grid, join = st_within)
        
        counts <- points_in_polygons %>%
          group_by(GRID_ID) %>%
          summarize(damaged_bridges = n()) %>% dplyr::select(-GRID_ID)
        
        polygon_grid <- st_join(polygon_grid, counts)
        
        ggplot(data = polygon_grid %>% filter(!is.na(damaged_bridges))) +
          geom_sf(aes(fill = damaged_bridges)) +
          scale_fill_viridis_c(option = "viridis") +
          labs(title = "Damaged roads",
               fill = "Damaged roads",
               caption = "Source: Polygon Grid Data") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)
          )
        
        ## Add mangro9ves Global Mangrove Watch
        
        ## OTHER
        length(levels(factor(polygon_grid$GRID_ID)))
        length(levels(factor(polygon_grid$Id)))
        polygon_grid <- st_transform(polygon_grid, crs = st_crs(mangroves))
        pol <- polygon_grid
          glimpse(mangroves)
        levels(factor(mangroves$Cmbio81_20))  
        
        intersection <- st_intersection(pol, mangroves)
        glimpse(intersection)
        #ggplot(intersection) +
         # geom_sf(aes(fill=factor(Cmbio81_20)) +
          #scale_fill_manual(values=c("Ganancia de manglar" = "darkgreen","Manglar sin cambios"="darkblue","Pérdida de manglar"="darkred"))
                
        ggplot(intersection) +
          geom_point(aes(x=damaged_households,y=Area_ha,col=Cmbio81_20)) +
          scale_color_manual(values=c("Ganancia de manglar" = "darkgreen","Manglar sin cambios"="darkblue","Pérdida de manglar"="darkred"))
          
        # Calculate the area of each intersection
        intersection$intersection_area <- st_area(intersection)
      
        # Calculate the area of each polygon in the grid
        polygon_grid$polygon_area <- st_area(polygon_grid)
        
        # Summarize the intersection areas by grid ID
        intersection_summary_ganancia <- intersection %>% filter(Cmbio81_20=="Ganancia de manglar") %>% 
            group_by(GRID_ID) %>%
            summarize(area_manglar_ganancia = sum(intersection_area))%>% as.data.frame() %>% st_drop_geometry()
          # Join the summarized intersection areas back to the polygon grid
          polygon_grid2 <- left_join(polygon_grid, intersection_summary_ganancia,by="GRID_ID") 
          polygon_grid2$area_manglar_ganancia <- as.numeric(polygon_grid2$area_manglar_ganancia)
          #plot
          ggplot(polygon_grid2 %>% filter(!is.na(area_manglar_ganancia)))+
            geom_sf(aes(fill=area_manglar_ganancia),color=NA) +
            scale_fill_viridis_b()
          
          # Summarize the intersection areas by grid ID
          intersection_summary_perdida <- intersection %>% filter(Cmbio81_20=="Pérdida de manglar") %>% 
            group_by(GRID_ID) %>%
            summarize(area_manglar_perdida = sum(intersection_area))%>% as.data.frame() %>% st_drop_geometry()
          # Join the summarized intersection areas back to the polygon grid
          polygon_grid2 <- left_join(polygon_grid2, intersection_summary_perdida,by="GRID_ID") 
          polygon_grid2$area_manglar_perdida <- as.numeric(polygon_grid2$area_manglar_perdida)
          #plot
          ggplot(polygon_grid2 %>% filter(!is.na(area_manglar_ganancia)))+
            geom_sf(aes(fill=area_manglar_ganancia))
          
          # Summarize the intersection areas by grid ID
          intersection_summary_sincambios <- intersection %>% filter(Cmbio81_20=="Manglar sin cambios") %>% 
            group_by(GRID_ID) %>%
            summarize(area_manglar_sincambios = sum(intersection_area))%>% as.data.frame() %>% st_drop_geometry()
          # Join the summarized intersection areas back to the polygon grid
          polygon_grid2 <- left_join(polygon_grid2, intersection_summary_sincambios,by="GRID_ID") 
          polygon_grid2$area_manglar_sincambios <- as.numeric(polygon_grid2$area_manglar_sincambios)
          #plot
          ggplot(polygon_grid2 %>% filter(!is.na(area_manglar_sincambios)))+
            geom_sf(aes(fill=area_manglar_sincambios))
          
          
        
        glimpse(polygon_grid2)
        
        polygon_grid2$Area_Manglar <- polygon_grid2$area_manglar_ganancia + polygon_grid2$area_manglar_sincambios
        polygon_grid2 <- polygon_grid2 %>% 
          mutate(Area_Manglar_Todo = sum(Area_Manglar,area_manglar_perdida,na.rm=TRUE))
        
        
        ggplot(polygon_grid2 %>% filter(!is.na(damaged_households)),
               aes(x=Area_Manglar,y=damaged_households)) +
          #geom_smooth()+
          #geom_point(aes(x=Area_Manglar,y=damaged_households),col="darkgreen")+
          geom_point(aes(x=area_manglar_sincambios,y=damaged_households),col="darkgreen")+
          geom_point(aes(x=area_manglar_perdida,y=damaged_households),col="darkred")+
          geom_point(data=polygon_grid2 %>% filter(is.na(Area_Manglar) & is.na(area_manglar_perdida)),aes(x=damaged_households*0+1,y=damaged_households),col="darkred",shape="cross")+
          #scale_x_continuous(trans="log")+
          theme_minimal()
        
        ggplot(polygon_grid2 %>% filter(!is.na(damaged_households))) +
          geom_sf(aes(fill=Area_Manglar))+
          theme_minimal()+
          scale_fill_viridis_c(option = "viridis") 
        
        
        polygon_grid2 %>% filter(!is.na(damaged_households) & !is.na(Area_Manglar))
        
        
        ggplot(polygon_grid2) +
          geom_point(aes(x=area_manglar_ganancia,y=area_manglar_perdida))
        
        polygon_
        
        
        ggplot(polygon_grid2) +
          geom_point(aes(x=damaged_households,y=Mangrove_Area,col=Cmbio81_20)) +
          scale_color_manual(values=c("Ganancia de manglar" = "darkgreen","Manglar sin cambios"="darkblue","Pérdida de manglar"="darkred"))
        
        
        class(mangroves)
        
        ##
        
        glimpse(polygon_grid)
        glimpse(damaged_buildings)
        levels(factor(damaged_buildings$simplified))
          ggplot(damaged_buildings)+
            geom_sf(aes(fill=damage_gra))
          
          intersection <- st_intersection(polygon_grid, damaged_buildings)
        glimpse(intersection)
          ##
      
      
      polygon_grid <- st_join(polygon_grid, damaged_households, join = st_intersects) %>% rename(damaged_household = DAÑO)
      polygon_grid <- st_join(polygon_grid, damaged_households, join = st_intersects) %>% rename(damaged_household = DAÑO)
      levels(factor(polygon_grid$damaged_household))
      
      
      
  # ---- Mangroves ----
      plot(mangrove_acapulco3)
      mangrove_acapulco_df <- as.data.frame(mangrove_acapulco, xy = TRUE)
      table(mangrove_acapulco_df$layer)
      mangrove_acapulco_df <- mangrove_acapulco_df %>%
        mutate(layer = ifelse(is.na(layer), 0, layer))
      #raster_sf <- st_as_sf(mangrove_acapulco_df, coords = c("x", "y"), crs = st_crs(polygon_grid), remove = FALSE)
      # Convert dataframe with raster values back to raster
      mangrove_acapulco_df2 <- mangrove_acapulco_df %>% filter(y>16.50 & x>-100.5)
      raster_from_df <- rasterFromXYZ(mangrove_acapulco_df2[, c("x", "y", "layer")])
      
      ggplot(data = mangrove_acapulco_df2) +
        geom_raster(aes(x = x, y = y, fill = layer)) +
        scale_fill_viridis_c() +  # Optional: Use a color scale from the viridis package for better visualization
        coord_equal() +
        labs(title = "Raster Plot",
             x = "Longitude",
             y = "Latitude",
             fill = "Value") +
        theme_minimal()
      
      # Assign the same CRS as the original raster
      crs(raster_from_df) <- crs(mangrove_acapulco)
      summaries <- exact_extract(raster_from_df, polygon_grid, fun = 'sum', progress = TRUE)
      glimpse(summaries)
      table(summaries)
      glimpse(polygon_grid)
      polygon_grid$GMW_covered_area <- summaries
      
      database_grid <- polygon_grid %>% st_drop_geometry() %>% as.data.frame()
      glimpse(database_grid)
      database_grid_long <- database_grid %>%
        pivot_longer(
          cols = c(damaged_households, damaged_roads, damaged_bridges),
          names_to = "infrastructure",
          values_to = "damage"
        ) %>%
        mutate(infrastructure = recode(infrastructure,
                                       "damaged_households" = "Hogar",
                                       "damaged_roads" = "Camino",
                                       "damaged_bridges" = "Puente"))
      
      # View the result
      glimpse(database_grid_long)
      
      ggplot(database_grid_long , aes(x=GMW_covered_area,y=damage)) +
        geom_point(aes(color=infrastructure))+
        geom_smooth(method="lm")+
        theme_minimal()+
        geom_hline(aes(yintercept=0),linetype="dashed")+
        labs(title="Infraestructura dañada por Otis", color="Tipo", x = "Cobertura de Manglar", y="Número de \n edificaciones  dañadas")+
        xlim(c(0,700))+
        scale_color_viridis_d()
        
      lm(damage~)
      
      glimpse(extracted_values)
# Load other datasets
#mangrove <- st_read("path_to_mangrove_data.shp")
#wind_speed <- raster("path_to_wind_speed_data.tif")
#infrastructure <- st_read("path_to_infrastructure_data.shp")

# Step 3: Prepare the Polygon Grid
# Check and transform CRS if necessary
#polygon_grid <- st_transform(polygon_grid, crs = st_crs(mangrove))
#wind_speed <- projectRaster(wind_speed, crs = st_crs(polygon_grid))
#infrastructure <- st_transform(infrastructure, crs = st_crs(polygon_grid))

# Step 4: Spatial Join
# Spatial join for vector data
# Mangrove data join
#polygon_grid <- st_join(polygon_grid, mangrove, join = st_intersects)

# Infrastructure data join
#polygon_grid <- st_join(polygon_grid, infrastructure, join = st_intersects)

# For raster data, aggregate the values within each polygon
# Wind speed data extraction
#polygon_grid$wind_speed_mean <- exact_extract(wind_speed, polygon_grid, 'mean')

# Step 5: Export the Combined Dataset
# Save the combined dataset as a new shapefile
#st_write(polygon_grid, "path_to_output_combined_data.shp")

# Print message upon completion
#print("Data integration completed and exported successfully.")