install.packages(c("sf","prettymapr","terra", "raster", "sp", "exactextractr","ggpubr","rnaturalearth","ggmap","ggspatial"))
# ---- Load Data and Libraries ----
  # Load necessary libraries
  library(sf)
  library(raster)
  library(sp)
  library(exactextractr)
  library(tidyverse)
  library(raster)
library(ggpubr)
library(rnaturalearth)
library(ggmap)
library("ggspatial")
library(prettymapr)
library("terra")
  
  
  # Step 2: Load Data into R
  # Load polygon grid
  polygon_grid_master <- st_read("E:/AburtoLabProjects/Otis/PolygonLayer.shp")
  gdb_path <- "E:\\AburtoLabProjects\\Otis\\Otis.gdb"
  # List the layers in the Geodatabase
  layers <- st_layers(gdb_path)
  print(layers)
  Acapulco_MasterGrid <- st_read(gdb_path, layer = "AcapulcoPolygon_MangroveStats_Coast2")
  plot(Acapulco_MasterGrid)
  
  DestroyedOnly_Final <- st_read(gdb_path, layer = "AcapulcoPolygon_Intersect_DestroyedOnly_Final")
  PossiblyDamagedOnly_Final <- st_read(gdb_path, layer = "AcapulcoPolygon_Intersect_PossiblyDamagedOnly_Final")
  DamagedOnly_Final <- st_read(gdb_path, layer = "AcapulcoPolygon_Intersect_DamagedOnly_Final")
  
  
  # Load wind dataset
  wind_speed_U25 <- st_read("E:\\AburtoLabProjects\\Otis\\uwind_shapefile25.shp")
  wind_speed_U24 <- st_read("E:\\AburtoLabProjects\\Otis\\uwind_shapefile24.shp")
  wind_speed_U23 <- st_read("E:\\AburtoLabProjects\\Otis\\uwind_shapefile23.shp")
  wind_speed_U22 <- st_read("E:\\AburtoLabProjects\\Otis\\uwind_shapefile22.shp")
  
  wind_speed_V25 <- st_read("E:\\AburtoLabProjects\\Otis\\vwind_shapefile25.shp")
  wind_speed_V24 <- st_read("E:\\AburtoLabProjects\\Otis\\vwind_shapefile24.shp")
  wind_speed_V23 <- st_read("E:\\AburtoLabProjects\\Otis\\vwind_shapefile23.shp")
  wind_speed_V22 <- st_read("E:\\AburtoLabProjects\\Otis\\vwind_shapefile22.shp")

  # Buildings Damaged (Points)
  damaged_households <- st_read("E:\\AburtoLabProjects\\Otis\\vap-974-16-product\\Damaged_households.shp")
  damaged_bridges <- st_read("E:\\AburtoLabProjects\\Otis\\INfrastructure\\EMSR703_AOI01_GRA_MONIT01_transportationP_v2.shp")
  damaged_roads <- st_read("E:\\AburtoLabProjects\\Otis\\INfrastructure\\EMSR703_AOI02_GRA_MONIT01_transportationL_v2.shp")
  
  # Buildings Damaged (Polygons)
  damaged_buildings <- st_read("E:\\AburtoLabProjects\\Otis\\INfrastructure\\EMSR703_AOI01_GRA_MONIT01_builtUpA_v2.shp")
  
  # Flash Flood (Polygons)
  flash_flood <- st_read("E:\\AburtoLabProjects\\Otis\\INfrastructure\\EMSR703_AOI01_GRA_MONIT01_observedEventA_v2.shp")
  
  # Mangroves
  mangroves <- st_read("E:\\AburtoLabProjects\\Otis\\MangrovesGuerrero.shp")
  mangrove_acapulco3 <- raster("E:\\AburtoLabProjects\\Otis\\gmw_v3_2020\\GMW_N17W101_2020_v3.tif")
  mangrove_acapulco4 <- raster("E:\\AburtoLabProjects\\Otis\\gmw_v3_2020\\GMW_N17W100_2020_v3.tif")
  
  mangrove_acapulco <- merge(mangrove_acapulco3, mangrove_acapulco4)
  plot(mangrove_acapulco3)
  
  # vegetation
  crf_file_path <- "E:\\AburtoLabProjects\\Otis\\OTISimpact_3.tif"
  # Read the .crf file
  veg_raster <- rast(crf_file_path)
  
  
# ---- Merge Datasets ----

  # Prepare the Polygon Grid: Check and transform CRS if necessary
  polygon_grid <- Acapulco_MasterGrid
  #polygon_grid <- polygon_grid_master
  polygon_grid <- st_transform(polygon_grid, crs = st_crs(wind_speed_U25))

  # ---- Windspeed ----
        # Windspeed data join
      joined <- st_join(polygon_grid, wind_speed_U25, join = st_intersects)
      
      # Calculate the mean wind speed for each polygon
      wind_speed_summary <- joined %>%
        group_by(GRID_ID) %>%
        summarize(wind_U25 = mean(wind_U25, na.rm = TRUE)) %>% as.data.frame()
      glimpse(wind_speed_summary)
      
      polygon_grid <- left_join(polygon_grid, wind_speed_summary)
      
    polygon_grid <- st_join(polygon_grid, wind_speed_U25, join = st_intersects) %>% rename(wind_U25 = layer)
      polygon_grid <- st_join(polygon_grid, wind_speed_U24, join = st_intersects) %>% rename(wind_U24 = layer)
      polygon_grid <- st_join(polygon_grid, wind_speed_U23, join = st_intersects) %>% rename(wind_U23 = layer)
      polygon_grid <- st_join(polygon_grid, wind_speed_U22, join = st_intersects) %>% rename(wind_U22 = layer)
      
      
      polygon_grid <- st_join(polygon_grid, wind_speed_V25, join = st_intersects) %>% rename(wind_V25 = layer)
      polygon_grid <- st_join(polygon_grid, wind_speed_V24, join = st_intersects) %>% rename(wind_V24 = layer)
      polygon_grid <- st_join(polygon_grid, wind_speed_V23, join = st_intersects) %>% rename(wind_V23 = layer)
      polygon_grid <- st_join(polygon_grid, wind_speed_V22, join = st_intersects) %>% rename(wind_V22 = layer)
      
      
      
      glimpse(polygon_grid)
      
      # plot 
      ggplot(data = polygon_grid) +
        geom_sf(aes(fill = wind_U25)) +
        scale_fill_viridis_c(option = "viridis") +
        labs(title = "Wind Speed Distribution",
             fill = "Wind U25",
             caption = "Source: Polygon Grid Data") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5)
        )
  # ---- Damaged Buildings ----
      # ---- Damage in vegetation
      joined <- exact_extract(veg_raster,polygon_grid)
      averages <- numeric(length(joined))*NA
      
      # Loop through the list and calculate the average for each element
      for (i in seq_along(joined)) {
        df <- joined[[i]]  # Extract the data frame from the list
        averages[i] <- sum(df$value*df$coverage_fraction, na.rm = TRUE)/sum(df$coverage_fraction)  # Calculate the average of the 'values' column
        print(i)
        }
      # Assign the averages to a new column in the polygon_grid data frame
      polygon_grid <- polygon_grid %>%
        mutate(veg_change = averages)
      
      glimpse(polygon_grid)
      # Display the updated polygon_grid data frame
      ggplot(polygon_grid %>% filter(veg_change !=0))+
        geom_point(aes(color=NEAR_DIST, y=veg_change, x=NEAR_DIST_coastline),alpha=0.09)+
        scale_color_viridis_b()+
        theme_minimal()+
        geom_hline(aes(yintercept=0),linetype="dashed",color="red")+
        labs(x="Distance to Coastline",y="Change in vegetation",color="Distance to Mangrove",size="Mangrove Cover")
      
      ggplot(polygon_grid %>% filter(veg_change !=0))+
        geom_point(aes(x=NEAR_DIST, y=veg_change, color=Point_Count),alpha=0.09)+
        scale_color_viridis_b()+
        theme_minimal()+
        geom_hline(aes(yintercept=0),linetype="dashed",color="red")+
        labs(x="Distance to nearest mangrove",y="Change in vegetation",color="Mangrove Cover",size="Mangrove Cover")
        #scale_x_continuous(trans="log")
      
      ggplot(polygon_grid %>% filter(veg_change !=0, Point_Count>0), aes(x=Point_Count, y=veg_change))+
        geom_point(alpha=0.2)+
        theme_minimal()+
        geom_hline(aes(yintercept=0),linetype="dashed",color="red")+
        labs(x="Mangrove Cover",y="Change in vegetation",size="Mangrove Cover")+
        geom_smooth()
      
     
      # ---- Damaged Households ----
        glimpse(polygon_grid)
        glimpse(damaged_households)
        
        damaged_households <- damaged_households %>% 
            rename(distance2mangroves=NEAR_DIST)
        
        points_in_polygons <- st_join(damaged_households, polygon_grid, join = st_within)
        glimpse(points_in_polygons)
        counts <- points_in_polygons %>%
          group_by(GRID_ID) %>%
          summarize(damaged_households = n(),
                    distance = mean(distance2mangroves,na.rm=T)) %>% dplyr::select(-GRID_ID)
        polygon_grid <- st_join(polygon_grid, counts)
        
        
        plot_number_damaged <- ggplot(data = polygon_grid %>% filter(!is.na(damaged_households))) +
          annotation_map_tile(zoom = 13, type = "osm") +
          geom_sf(aes(fill = damaged_households),alpha=0.7) +
          scale_fill_viridis_c(option = "plasma") +
          labs(#title = "Casas Dañadas",
               fill = "Casas Dañadas",
               caption = "") +
          theme_void() +
          theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)
          )
        plot_number_damaged
        
        plot_distance <- ggplot(data = polygon_grid %>% filter(!is.na(damaged_households))) +
          annotation_map_tile(zoom = 13, type = "osm") +
          geom_sf(aes(fill = distance),alpha=0.7) +
          scale_fill_viridis_c(option = "viridis") +
          labs(#title = "Distancia a Manglares (km)",
               fill = "Distancia a \nManglares (km)",
               caption = "") +
          theme_void() +
          theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)
          )
      
          correlacion_casas_manglar <- ggplot(polygon_grid, aes(x=distance,y=damaged_households))+
            geom_point()+
            geom_smooth(formula = y ~0+ I(x^2), method = "lm")+
            theme_minimal()+ labs(x="Distancia a manglar (km)", y="Numero de \ncasas dañadas")
        
          ggarrange(
              ggarrange(plot_number_damaged,plot_distance,ncol=2),
              correlacion_casas_manglar,ncol=1)
          
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
            
            
            
          
                                     
        polygon_grid3 <- polygon_grid %>% 
            left_join(PossiblyDamagedOnly_Final2,by="GRID_ID") %>% 
          left_join(DamagedOnly_Final2,by="GRID_ID") %>% 
          left_join(DestroyedOnly_Final2,by="GRID_ID") 
          
        install.packages("lfe")
        library(lfe)
        summary(felm(Percent_area_Destroyed ~ NEAR_DIST|0|0,data=polygon_grid3))
        
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