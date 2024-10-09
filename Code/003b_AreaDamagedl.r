
    Guer_Mang <- readRDS(file = "Guerrero_Gridcells_Mangroves.rds")
    glimpse(Guer_Mang)
    Guer_Mang$dif_distance_meters <- Guer_Mang$Dist2Mang_hist_meters - Guer_Mang$Dist2Mang_meters

    Guer_Mangd <- Guer_Mang %>%
          filter(!is.na(Percent_area_Destroyed) | !is.na(Percent_area_Damaged)) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang_meters, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin10 = cut(Dist2Coast,, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Dist2Mang_bin10 = cut(Dist2Mang_meters, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Area_DAM = sum(Percent_area_Destroyed,Percent_area_Damaged,na.rm=TRUE)) %>% 
          filter(Area_DAM>2)

    model_des_lin_bins <- felm(Percent_area_Destroyed~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangd )
    summary(model_des_lin_bins)
    model_des_lin_bins <- felm(Percent_area_Damaged~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangd )
    summary(model_des_lin_bins)
    # model_des_lin_bins <- felm(I(Percent_area_Damaged+Percent_area_Destroyed)~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangd )
    # summary(model_des_lin_bins)
    # model_des_lin_bins <- felm(Area_DAM~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangd )
    # summary(model_des_lin_bins)
 

    lincoef2 <- summary(model_des_lin_bins)$coefficients[2]
    lincoef2_se <- summary(model_des_lin_bins)$coefficients[6]
    
    Guer_Mangd <- Guer_Mangd %>% 
        mutate(counterfactual_des_dif  = lincoef2 *dif_distance_meters, 
            lower_ci = counterfactual_des_dif - 1.96 * (lincoef2_se * dif_distance_meters),
            upper_ci = counterfactual_des_dif + 1.96 * (lincoef2_se * dif_distance_meters)
         ) #%>% 
         #mutate(counterfactual_des_dif = ifelse(counterfactual_des_dif < -damaged_households,-damaged_households,counterfactual_house_dif ))

    bbox2 <- st_bbox(Guer_Mangd)
 counterfactual_des <- ggplot(data = Guer_Mangd) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = -counterfactual_des_dif), col = "transparent", alpha = 0.85) +
        
        scale_fill_scico(palette="bamako", direction=-1,
                            #limits = c(0, 1), 
                            oob = scales::squish,
                            guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
        labs(title = "B. Hypothetical Effect of Having 1981 Mangrove Coverage",
            fill = "Avoided Destroyed \nHouses (No. of households)",
            caption = "") +
        theme_void() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            #legend.position = "bottom",                        # Position the legend at the bottom
            #legend.direction = "horizontal"#,                   # Make the legend horizontal
            #legend.key.width = unit(0.8, "cm"),                  # Adjust width of the legend key
            #legend.key.height = unit(0.5, "cm")                # Adjust height of the legend key
        )+
  geom_sf(data = otis_line,color = "steelblue", linewidth = 3, alpha = 0.5) +
    coord_sf(xlim = c(bbox2["xmin"], bbox2["xmax"]), ylim = c(bbox2["ymin"], bbox2["ymax"]))  

 #counterfactual_des 
 
Guer_Mangd <- Guer_Mangd %>%
  mutate(calculated_area = (st_area(Shape)),  # Calculate area in square meters
         area_des_dif = calculated_area * counterfactual_des_dif/100, 
         area_des_dif_lower_ci = calculated_area * lower_ci/100, 
         area_des_dif_upper_ci = calculated_area * upper_ci/100, 
         area_des = as.numeric(calculated_area) * Percent_area_Damaged/100 )
glimpse(Guer_Mangd)


    # # DO NOT ERASE - SAVE TO DO FIELDWORK
    # #
    # Guerrero_Mangroves_Field3 <- Guer_Mangd %>% 
    # #mutate(Percent_area_DAM = Percent_area_Damaged + Percent_area_Destroyed, 
    # #Veg_Pct_Ch = veg_change*100) %>% 
    # select(
    #   GRID_ID, Dist2Coast,dif_distance_meters,area_des_dif,area_des,Percent_area_Damaged
    # ) %>% 
    # rename(Pct_DAM=Percent_area_Damaged,dif_dist=dif_distance_meters,DamA_m2_b=area_des,DamA_m2_av=area_des_dif
    # ) 
    #  st_write(Guerrero_Mangroves_Field3 , "Data/Guerrero_Mangroves_Field3_DamagedInfra.shp", delete_layer = TRUE)
    # #
    # # DO NOT ERASE - SAVE TO DO FIELDWORK

   sum(Guer_Mangd$area_des_dif,na.rm=T)
   
   sum(Guer_Mangd$area_des,na.rm=T)
   
   852976/49923691332

   852976/21064099

   784294/28163462

counterfactual_des <- ggplot(data = Guer_Mangd) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = -as.numeric(area_des_dif)), col = "transparent", alpha = 0.85) +
        
        scale_fill_scico(palette="bamako", direction=-1,
                            #limits = c(0, 1), 
                            oob = scales::squish,
                            guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
        labs(title = "C. Effect on Damaged Area",
            fill = "Avoided Damaged \nArea (m^2)",
            caption = "") +
        theme_void() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            legend.position = "bottom",                        # Position the legend at the bottom
            legend.direction = "horizontal",                   # Make the legend horizontal
            legend.key.width = unit(0.8, "cm"),                  # Adjust width of the legend key
            legend.key.height = unit(0.5, "cm")                # Adjust height of the legend key
        )+
  geom_sf(data = otis_line, col= "steelblue", linewidth = 3, alpha = 0.5) +
    coord_sf(xlim = c(bbox2["xmin"], bbox2["xmax"]), ylim = c(bbox2["ymin"], bbox2["ymax"]))
#counterfactual_des

glimpse(Guer_Mangd)
class(Guer_Mangd)   
   
   dam_avoided <- sum(Guer_Mangd$area_des_dif,na.rm=T)
   dam_avoided_lci <-sum(Guer_Mangd$area_des_dif_lower_ci,na.rm=T)
   dam_avoided_uci <-sum(Guer_Mangd$area_des_dif_upper_ci,na.rm=T)
   dam_base <-sum(Guer_Mangd$area_des,na.rm=T)

100*dam_avoided/dam_base
100*veg_avoided_lci/veg_base
100*veg_avoided_uci/veg_base

# Create an interactive leaflet map
leaflet(Guer_Mangd) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(as.numeric(counterfactual_des_dif)),
    weight = 1,
    color = "transparent",
    fillOpacity = 0.99,
    popup = ~paste("GRID ID:", GRID_ID, "<br>",
                   #"Longitude:", lon, "<br>",
                   #"Latitude:", lat, "<br>",
                   "Dif in Area Damged (%):", as.numeric(counterfactual_des_dif))
  ) %>%
  addLegend(
    pal = pal,
    values = ~as.numeric(counterfactual_des_dif),
    title = "Dif in Area Damged:",
    opacity = 1
  )

pal <- colorNumeric(
  palette = scico(10, palette = "lajolla"),
  domain = as.numeric(Guer_Mangd$counterfactual_des_dif)
)

glimpse(Guer_Mangd_select)
Guer_Mangd_select <- Guer_Mangd %>% select(GRID_ID,Dist2Coast,Percent_area_Damaged,Percent_area_Destroyed,area_des_dif,counterfactual_des_dif)
st_write(Guer_Mangd_select, "Data/Guerrero_Mangroves_DamagedInfrastructure_select.shp")


# # Shorten field names to comply with the Shapefile format's restrictions
# short_names <- make.names(substr(names(Guer_Mangd), 1, 10))
# # Erase the last two characters and add a unique suffix to ensure unique names
# names(Guer_Mangd) <- make.unique(substr(names(Guer_Mangd), 1, nchar(names(Guer_Mangd)) - 2))


# # Now write the sf object to a shapefile
# st_write(Guer_Mangd, "Data/Guerrero_Mangroves_DamagedInfrastructure.shp", delete_layer = TRUE)
