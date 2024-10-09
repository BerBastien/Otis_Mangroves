dir <- "C:\\Users\\basti\\Documents\\GitHub\\Otis_Mangroves"
datadir <- "C:\\Users\\basti\\Box\\Data\\Oceans\\Otis"

setwd(dir)
    install.packages("exactextractr")
    libraries <- c("sf", "raster", "sp", "exactextractr", "tidyverse", "ggpubr", 
                "rnaturalearth", "ggmap", "ggspatial", "prettymapr", "terra","tidyverse")

    # Load all libraries in one command
    lapply(libraries, library, character.only = TRUE)


## Read Datasets (STAR)
    # Step 2: Load Data into R
    # Load polygon grid
    polygon_grid_master <- st_read(paste0(datadir,"/PolygonLayer.shp"))
    gdb_path <- paste0(datadir,"\\Otis.gdb")
    # List the layers in the Geodatabase
    #layers <- st_layers(gdb_path)
    #print(layers)
    Acapulco_MasterGrid <- st_read(gdb_path, layer = "AcapulcoPolygon_MangroveStats_Coast2")
    #plot(Acapulco_MasterGrid)

    DestroyedOnly_Final <- st_read(gdb_path, layer = "AcapulcoPolygon_Intersect_DestroyedOnly_Final")
    PossiblyDamagedOnly_Final <- st_read(gdb_path, layer = "AcapulcoPolygon_Intersect_PossiblyDamagedOnly_Final")
    DamagedOnly_Final <- st_read(gdb_path, layer = "AcapulcoPolygon_Intersect_DamagedOnly_Final")


    # Load wind dataset
    wind_speed_U25 <- st_read(paste0(datadir,"\\uwind_shapefile25.shp"))
    wind_speed_U24 <- st_read(paste0(datadir,"\\uwind_shapefile24.shp"))
    wind_speed_U23 <- st_read(paste0(datadir,"\\uwind_shapefile23.shp"))
    wind_speed_U22 <- st_read(paste0(datadir,"\\uwind_shapefile22.shp"))

    wind_speed_V25 <- st_read(paste0(datadir,"\\vwind_shapefile25.shp"))
    wind_speed_V24 <- st_read(paste0(datadir,"\\vwind_shapefile24.shp"))
    wind_speed_V23 <- st_read(paste0(datadir,"\\vwind_shapefile23.shp"))
    wind_speed_V22 <- st_read(paste0(datadir,"\\vwind_shapefile22.shp"))

    # Buildings Damaged (Points)
    damaged_households <- st_read(paste0(datadir,"\\vap-974-16-product\\Damaged_households.shp"))
    damaged_bridges <- st_read(paste0(datadir,"\\INfrastructure\\EMSR703_AOI01_GRA_MONIT01_transportationP_v2.shp"))
    damaged_roads <- st_read(paste0(datadir,"\\INfrastructure\\EMSR703_AOI02_GRA_MONIT01_transportationL_v2.shp"))

    # Buildings Damaged (Polygons)
    damaged_buildings <- st_read(paste0(datadir,"\\INfrastructure\\EMSR703_AOI01_GRA_MONIT01_builtUpA_v2.shp"))

    # Flash Flood (Polygons)
    flash_flood <- st_read(paste0(datadir,"\\INfrastructure\\EMSR703_AOI01_GRA_MONIT01_observedEventA_v2.shp"))

    # Mangroves
    mangroves <- st_read(paste0(datadir,"\\MangrovesGuerrero.shp"))
    mangrove_acapulco3 <- raster(paste0(datadir,"\\gmw_v3_2020\\GMW_N17W101_2020_v3.tif"))
    mangrove_acapulco4 <- raster(paste0(datadir,"\\gmw_v3_2020\\GMW_N17W100_2020_v3.tif"))

    mangrove_acapulco <- merge(mangrove_acapulco3, mangrove_acapulco4)
    #plot(mangrove_acapulco3)

    # vegetation
    crf_file_path <- paste0(datadir,"\\OTISimpact_3.tif")
    # Read the .crf file
    veg_raster <- rast(crf_file_path)
## Read Datasets (END)
  
# ---- Merge Datasets ----

    # Prepare the Polygon Grid: Check and transform CRS if necessary
    polygon_grid <- Acapulco_MasterGrid
    #polygon_grid <- polygon_grid_master
    polygon_grid <- st_transform(polygon_grid, crs = st_crs(wind_speed_U25))

    # ---- Windspeed ----
        # Windspeed data join
        #joined <- st_join(polygon_grid, wind_speed_U25, join = st_intersects)
        #glimpse(joined)

        # Calculate the mean wind speed for each polygon
        #wind_speed_summary <- joined %>%
        #group_by(GRID_ID) %>%
        #summarize(wind_U25 = mean(layer, na.rm = TRUE)) %>% as.data.frame() %>% drop_st_geomtry()
        #glimpse(wind_speed_summary)

        #polygon_grid <- left_join(polygon_grid, wind_speed_summary)

        #polygon_grid <- st_join(polygon_grid, wind_speed_U25, join = st_intersects) %>% rename(wind_U25 = layer)
        glimpse(polygon_grid)

        polygon_grid <- polygon_grid %>%
            st_join(wind_speed_U22, join = st_intersects) %>% rename(wind_U22 = layer)%>% 
            st_join(wind_speed_U23, join = st_intersects) %>% rename(wind_U23 = layer)%>%
            st_join(wind_speed_U24, join = st_intersects) %>% rename(wind_U24 = layer)%>%
            st_join(wind_speed_U25, join = st_intersects) %>% rename(wind_U25 = layer)%>%
            group_by(GRID_ID) %>%
            summarize(across(c(NEAR_FID:wind_U25), first)) %>% ungroup()

        polygon_grid <- polygon_grid %>%
            st_join(wind_speed_V22, join = st_intersects) %>% rename(wind_V22 = layer)%>% 
            st_join(wind_speed_V23, join = st_intersects) %>% rename(wind_V23 = layer)%>%
            st_join(wind_speed_V24, join = st_intersects) %>% rename(wind_V24 = layer)%>%
            st_join(wind_speed_V25, join = st_intersects) %>% rename(wind_V25 = layer)%>%
            group_by(GRID_ID) %>%
            summarize(across(c(NEAR_FID:wind_V25), first)) %>% ungroup()
        
           glimpse(polygon_grid)
        
        #   # plot 
        #   ggplot(data = polygon_grid) +
        #     geom_sf(aes(fill = wind_U25)) +
        #     scale_fill_viridis_c(option = "viridis") +
        #     labs(title = "Wind Speed Distribution",
        #          fill = "Wind U25",
        #          caption = "Source: Polygon Grid Data") +
        #     theme_minimal() +
        #     theme(
        #       plot.title = element_text(hjust = 0.5),
        #       plot.caption = element_text(hjust = 0.5)
        # )
  # ---- Damaged Buildings ----
      # ---- Damage in vegetation
      joined <- exact_extract(veg_raster,polygon_grid)
      averages <- numeric(length(joined))*NA
      
      # Loop through the list and calculate the average for each element
      for (i in seq_along(joined)) {
        df <- joined[[i]]  # Extract the data frame from the list
        averages[i] <- sum(df$value*df$coverage_fraction, na.rm = TRUE)/sum(df$coverage_fraction)  # Calculate the average of the 'values' column
        #print(i)
        }
      # Assign the averages to a new column in the polygon_grid data frame
      polygon_grid2 <- polygon_grid %>%
        mutate(veg_change = averages)
      
      glimpse(polygon_grid2)
      # Display the updated polygon_grid data frame
    #   ggplot(polygon_grid %>% filter(veg_change !=0))+
    #     geom_point(aes(color=NEAR_DIST, y=veg_change, x=NEAR_DIST_coastline),alpha=0.09)+
    #     scale_color_viridis_b()+
    #     theme_minimal()+
    #     geom_hline(aes(yintercept=0),linetype="dashed",color="red")+
    #     labs(x="Distance to Coastline",y="Change in vegetation",color="Distance to Mangrove",size="Mangrove Cover")
      
    #   ggplot(polygon_grid %>% filter(veg_change !=0))+
    #     geom_point(aes(x=NEAR_DIST, y=veg_change, color=Point_Count),alpha=0.09)+
    #     scale_color_viridis_b()+
    #     theme_minimal()+
    #     geom_hline(aes(yintercept=0),linetype="dashed",color="red")+
    #     labs(x="Distance to nearest mangrove",y="Change in vegetation",color="Mangrove Cover",size="Mangrove Cover")
    #     #scale_x_continuous(trans="log")
      
    #   ggplot(polygon_grid %>% filter(veg_change !=0, Point_Count>0), aes(x=Point_Count, y=veg_change))+
    #     geom_point(alpha=0.2)+
    #     theme_minimal()+
    #     geom_hline(aes(yintercept=0),linetype="dashed",color="red")+
    #     labs(x="Mangrove Cover",y="Change in vegetation",size="Mangrove Cover")+
    #     geom_smooth()
      
     
      # ---- Damaged Households ----
        glimpse(polygon_grid2)
        glimpse(damaged_households)
        
        damaged_households <- damaged_households %>% 
            rename(distance2mangroves=NEAR_DIST)
        
        points_in_polygons <- st_join(damaged_households, polygon_grid2, join = st_within)
        glimpse(points_in_polygons)
        counts <- points_in_polygons %>%
          group_by(GRID_ID) %>%
          summarize(damaged_households = n(),
                    distance = mean(distance2mangroves,na.rm=T)) %>% dplyr::select(-GRID_ID)
        polygon_grid3 <- st_join(polygon_grid2, counts)
        glimpse(polygon_grid3)        
        
        # plot_number_damaged <- ggplot(data = polygon_grid %>% filter(!is.na(damaged_households))) +
        #   annotation_map_tile(zoom = 13, type = "osm") +
        #   geom_sf(aes(fill = damaged_households),alpha=0.7) +
        #   scale_fill_viridis_c(option = "plasma") +
        #   labs(#title = "Casas Dañadas",
        #        fill = "Casas Dañadas",
        #        caption = "") +
        #   theme_void() +
        #   theme(
        #     plot.title = element_text(hjust = 0.5),
        #     plot.caption = element_text(hjust = 0.5)
        #   )
        # plot_number_damaged
        
        # plot_distance <- ggplot(data = polygon_grid %>% filter(!is.na(damaged_households))) +
        #   annotation_map_tile(zoom = 13, type = "osm") +
        #   geom_sf(aes(fill = distance),alpha=0.7) +
        #   scale_fill_viridis_c(option = "viridis") +
        #   labs(#title = "Distancia a Manglares (km)",
        #        fill = "Distancia a \nManglares (km)",
        #        caption = "") +
        #   theme_void() +
        #   theme(
        #     plot.title = element_text(hjust = 0.5),
        #     plot.caption = element_text(hjust = 0.5)
        #   )
      
        #   correlacion_casas_manglar <- ggplot(polygon_grid, aes(x=distance,y=damaged_households))+
        #     geom_point()+
        #     geom_smooth(formula = y ~0+ I(x^2), method = "lm")+
        #     theme_minimal()+ labs(x="Distancia a manglar (km)", y="Numero de \ncasas dañadas")
        
        #   ggarrange(
        #       ggarrange(plot_number_damaged,plot_distance,ncol=2),
        #       correlacion_casas_manglar,ncol=1)
          
      # ---- Percent damaged areas ----
          
     
          
          DamagedOnly_Final2 <- DamagedOnly_Final%>% 
            st_drop_geometry()  %>% 
            as.data.frame() %>% 
            #select(Acapulcopolygon_PairwiseClip_GRID_ID,Percent_area) %>% 
            rename(GRID_ID = Acapulcopolygon_PairwiseClip_GRID_ID, 
                   Percent_area_Damaged=Percent_area) %>% 
            mutate(repeated= paste0(Percent_area_Damaged,GRID_ID)) %>% 
            group_by(repeated) %>% 
            summarise(Percent_area_Damaged = first(Percent_area_Damaged), 
                      GRID_ID = first(GRID_ID)) %>% 
            select(-repeated)%>% 
            group_by(GRID_ID) %>% 
            summarise(Percent_area_Damaged=sum(Percent_area_Damaged,na.rm=TRUE))
          
          
          DestroyedOnly_Final2 <- DestroyedOnly_Final%>% 
            st_drop_geometry()  %>% 
            as.data.frame() %>% 
            #select(Acapulcopolygon_PairwiseClip_GRID_ID,Percent_area) %>% 
            rename(GRID_ID = Acapulcopolygon_PairwiseClip_GRID_ID, 
                   Percent_area_Destroyed=Percent_area) %>% 
            mutate(repeated= paste0(Percent_area_Destroyed,GRID_ID)) %>% 
            group_by(repeated) %>% 
            summarise(Percent_area_Destroyed = first(Percent_area_Destroyed), 
                      GRID_ID = first(GRID_ID)) %>% 
            select(-repeated)%>% 
            group_by(GRID_ID) %>% 
            summarise(Percent_area_Destroyed=sum(Percent_area_Destroyed,na.rm=TRUE))
          
          
          PossiblyDamagedOnly_Final2 <- PossiblyDamagedOnly_Final%>% 
            st_drop_geometry()  %>% 
            as.data.frame() %>% 
            #select(Acapulcopolygon_PairwiseClip_GRID_ID,Percent_area) %>% 
            rename(GRID_ID = Acapulcopolygon_PairwiseClip_GRID_ID, 
                   Percent_area_Possibly=Percent_area) %>% 
            mutate(repeated= paste0(Percent_area_Possibly,GRID_ID)) %>% 
            group_by(repeated) %>% 
            summarise(Percent_area_Possibly = first(Percent_area_Possibly), 
                      GRID_ID = first(GRID_ID)) %>% 
            select(-repeated) %>% 
            group_by(GRID_ID) %>% 
            summarise(Percent_area_Possibly=sum(Percent_area_Possibly,na.rm=TRUE))
            
            
            
          
                                     
        polygon_grid4 <- polygon_grid3 %>% 
            left_join(PossiblyDamagedOnly_Final2,by="GRID_ID") %>% 
          left_join(DamagedOnly_Final2,by="GRID_ID") %>% 
          left_join(DestroyedOnly_Final2,by="GRID_ID") 
        glimpse(polygon_grid4)
        
        
        polygon_grid_final <- polygon_grid3 %>% 
        rename(Dist2Mang = NEAR_DIST, 
            Mangrove_Pixels  = Point_Count, 
            Dist2Coast = NEAR_DIST_coastline) %>% 
            dplyr::select(-NEAR_FID,-sum_grid_code,-Join_ID,-NEAR_FID_coastline,-distance)
        glimpse(polygon_grid_final)
        glimpse(Acapulco_MasterGrid)

        saveRDS(polygon_grid_final, file = "Guerrero_Gridcells_Mangroves.rds")
