## Models

    polygon_grid_master <- st_read(paste0(datadir,"/PolygonLayer.shp"))
    gdb_path <- paste0(datadir,"\\Otis.gdb")
    Guerrero_Gridcells_Mangroves_withwater <- readRDS("Guerrero_Gridcells_Mangroves_withwater.rds")
    
    Guer_Mang <- readRDS(file = "Guerrero_Gridcells_Mangroves.rds")
    glimpse(Guer_Mang)
    Guer_Mang$dif_distance_meters <- Guer_Mang$Dist2Mang_hist_meters - Guer_Mang$Dist2Mang_meters
    
    ## DO NOT ERASE - SAVE TO DO FIELDWORK
    ##
    # Guerrero_Mangroves_Field1 <- Guer_Mang %>% 
    # mutate(Percent_area_DAM = Percent_area_Damaged + Percent_area_Destroyed, 
    # Veg_Pct_Ch = veg_change*100) %>% 
    # select(
    #   GRID_ID, Mangrove_Pixels, Dist2Coast,Veg_Pct_Ch,damaged_households,Percent_area_DAM,Dist2Mang_hist_meters,Dist2Mang_meters,dif_distance_meters
    # ) %>% 
    # rename(Mang_Pixels = Mangrove_Pixels,dam_house=damaged_households,Pct_DAM=Percent_area_DAM,
    #   Dist2MangH=Dist2Mang_hist_meters,Dist2Mang=Dist2Mang_meters,dif_dist=dif_distance_meters
    # ) 
    ##
    ## DO NOT ERASE - SAVE TO DO FIELDWORK

    # glimpse(Guerrero_Mangroves_Field1)
    # st_write(Guerrero_Mangroves_Field1, "Data/Guerrero_Mangroves_Field1.shp", delete_layer = TRUE)


   
    Guer_Mangv <- Guer_Mang %>%
          filter(!is.na(veg_change)) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang_meters, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin10 = cut(Dist2Coast,, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Dist2Mang_bin10 = cut(Dist2Mang_meters, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")))

    bbox <- st_bbox(Guer_Mangv)
#     model_veg_sq_bins <- felm(veg_change~Dist2Mang_meters + I(Dist2Mang_meters^2)  |Dist2Coast_bin10|0|0,Guer_Mangv )
#     model_veg_sq_bins2 <- felm(veg_change~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangv )
#     summary(model_veg_sq_bins2)
    
#     summary(model_veg_sq_bins)
#         lincoef <- summary(model_veg_sq_bins)$coefficients[1]
#         sqcoef <- summary(model_veg_sq_bins)$coefficients[2]
#         lincoef_se <- summary(model_veg_sq_bins)$coefficients[3]
#         sqcoef_se <- summary(model_veg_sq_bins)$coefficients[4]
#         fixed_effects <- getfe(model_veg_sq_bins)
#     fixed_effects_df <- fixed_effects %>%
#     select(effect, idx) %>%
#     rename(Dist2Coast_bin10 = idx, fixed_effect = effect)
    
#     Guer_Mangv <- Guer_Mangv %>%
#     left_join(fixed_effects_df, by = "Dist2Coast_bin10")%>% 
#         mutate(marginal_effect = lincoef + 2 * sqcoef * Dist2Mang_meters ) %>%
#         mutate(marginal_effect_se = sqrt(lincoef_se^2 + (2 * Dist2Mang_meters * sqcoef_se)^2))%>% 
#         mutate(counterfactual_veg_change =  marginal_effect *dif_distance_meters  )%>%
#         mutate(
#             lower_ci = marginal_effect - 2.576 * marginal_effect_se,
#             upper_ci = marginal_effect + 2.576  * marginal_effect_se
#         )%>%
#     mutate(effect_significant = ifelse(lower_ci > 0 | upper_ci < 0, "Significant", "Not Significant"))

#     glimpse(Guer_Mangv)


# marg_effect <- ggplot(data = Guer_Mangv) +
#         annotation_map_tile(zoom = 10, type = "osm") +
#         geom_sf(aes(fill = 100*1000*marginal_effect), col = "transparent", alpha = 0.85) +
#         geom_sf(data = Guer_Mangv %>% filter(effect_significant == "Not Significant"), 
#                 aes(geometry = Shape), fill = NA, color = "grey50", linetype = "dashed", size = 0.5) +
        
#         scale_fill_scico(palette="berlin", direction=-1,
#                             oob = scales::squish,
#                             guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
#         labs(title = "Effect of one additional kilometer farther from Mangroves",
#             fill = "(% Change of Vegetation)",
#             caption = "") +
#         theme_void() +
#         theme(
#             plot.title = element_text(hjust = 0.5),
#             plot.caption = element_text(hjust = 0.5),
#             legend.position = "bottom",                        # Position the legend at the bottom
#             legend.direction = "horizontal"#,                   # Make the legend horizontal
#             #legend.key.width = unit(0.8, "cm"),                  # Adjust width of the legend key
#             #legend.key.height = unit(0.5, "cm")                # Adjust height of the legend key
#         )


# counterfactual_veg <- ggplot(data = Guer_Mangv) +
#         annotation_map_tile(zoom = 10, type = "osm") +
#         geom_sf(aes(fill = 100*counterfactual_veg_change), col = "transparent", alpha = 0.85) +
#         geom_sf(data = Guer_Mangv %>% filter(effect_significant == "Not Significant"), 
#                 aes(geometry = Shape), fill = NA, color = "grey50", linetype = "dashed", size = 0.5) +
        
#         scale_fill_scico(palette="berlin", direction=-1,
#                             oob = scales::squish,
#                             guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
#         labs(title = "How vegetation would look like",
#             fill = "(Difference in % Change of Vegetation)",
#             caption = "") +
#         theme_void() +
#         theme(
#             plot.title = element_text(hjust = 0.5),
#             plot.caption = element_text(hjust = 0.5),
#             legend.position = "bottom",                        # Position the legend at the bottom
#             legend.direction = "horizontal"#,                   # Make the legend horizontal
#             #legend.key.width = unit(0.8, "cm"),                  # Adjust width of the legend key
#             #legend.key.height = unit(0.5, "cm")                # Adjust height of the legend key
#         )

# ggarrange(marg_effect,counterfactual_veg)

    model_veg_lin_interaction <- felm(veg_change~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangv )
    summary(model_veg_lin_interaction)
    lincoef2 <- summary(model_veg_lin_interaction)$coefficients[2]
    lincoef2_se <- summary(model_veg_lin_interaction)$coefficients[6]
    Guer_Mangv <- Guer_Mangv %>% 
        mutate(counterfactual_veg_change_dif  = lincoef2 *dif_distance_meters )%>%
  mutate(
    lower_ci = counterfactual_veg_change_dif - 1.96 * (lincoef2_se * dif_distance_meters),
    upper_ci = counterfactual_veg_change_dif + 1.96 * (lincoef2_se * dif_distance_meters)
  )

 counterfactual_veg <- ggplot(data = Guer_Mangv) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = 100*counterfactual_veg_change_dif), col = "transparent", alpha = 0.85) +
        
        scale_fill_scico(palette="bamako", direction=-1,
                            limits = c(0, 1), 
                            oob = scales::squish,
                            guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
        labs(title = "B. Effect on Nearby Vegetation",
            fill = "Avoided Vegetation \nDamage (% Change)",
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
  geom_sf(data = otis_line, color = "steelblue", linewidth = 3, alpha = 0.5) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) 

glimpse(Guerrero_Gridcells_Mangroves_withwater)
historical_distance <- ggplot(Guerrero_Gridcells_Mangroves_withwater)+
    geom_point(aes(x=Dist2Mang_hist_meters/1000,y=Dist2Mang_meters/1000,col=Dist2Coast),alpha=0.5)+
    geom_abline(intercept = 0, slope = 1, color = "steelblue", linetype = "solid", size = 1) +
     labs(title = "A. Historical and Current Distance to Mangroves",
       x = "Historical Distance to Mangroves (km)",
       col = "Distance to \nCoastline (km)",
       y = "Current Distance to Mangroves (km)") +
    theme_minimal() +
    scale_color_scico()+
    theme(
        plot.title = element_text(hjust = 0.5)
    )
historical_distance 
Guer_Mangv %>% 
mutate(dif_dist_all =  Dist2Mang_meters - Dist2Mang_hist_meters) %>% 
#group_by(Dist2Coast_bin) %>% 
#filter(Dist2Coast<2) %>% 
filter(dif_dist_all !=0) %>% 
summarize(dist_hist = mean(Dist2Mang_hist_meters,na.rm=T), 
    dist_present = mean(Dist2Mang_meters,na.rm=T), 
    max_dist = max(dif_dist_all,na.rm=T)) %>% 
    mutate(dif= dist_present-dist_hist, 
    dif_perc= dist_present/dist_hist)

Guer_Mangv %>% 
mutate(dif_dist_all =  Dist2Mang_meters - Dist2Mang_hist_meters) %>% 
mutate(nochange = ifelse(dif_dist_all!=0,"change","nochange")) %>% 
group_by(nochange) %>% 
summarize(count=n(), 
mean_dist = mean(dif_dist_all), 
max_dist = max(dif_dist_all))

change_dist <- Guer_Mangv %>% 
mutate(dif_dist_all =  Dist2Mang_meters - Dist2Mang_hist_meters) %>% 
mutate(nochange = ifelse(dif_dist_all!=0,"change","nochange")) 


glimpse(change_dist)

ggplot(change_dist)+
geom_boxplot(aes(x=dif_dist_all/Dist2Mang_hist_meters,y=Dist2Coast_bin10))

5277/(6966+5277) #Percent of locations that have experienced mangrove alejamiento



locations_3km <- Guer_Mangv %>% 
mutate(dif_dist_all =  Dist2Mang_meters - Dist2Mang_hist_meters) %>% 
mutate(nochange = ifelse(dif_dist_all!=0,"change","nochange")) %>% 
filter(nochange =="change") %>%
arrange(-dif_dist_all) %>% 
filter(Dist2Coast<3)%>%
select(GRID_ID,Dist2Coast,dif_dist_all) %>%
  select(GRID_ID, Dist2Coast, dif_dist_all, Shape) %>%
  rowwise() %>%
  mutate(
    lon = ifelse(st_geometry_type(Shape) == "POLYGON",
                             st_coordinates(Shape)[1, 1], NA),
    lat = ifelse(st_geometry_type(Shape) == "POLYGON",
                             st_coordinates(Shape)[1, 2], NA)
  ) %>%
  ungroup()

  locations_3km_nogeom <- locations_3km %>% st_drop_geometry
#write.csv(locations_3km_nogeom,file="Data/locations_3km_nogeom.csv")

glimpse(locations_3km)

ggplot(locations_3km)+
geom_sf(aes(fill=dif_dist_all),col="transparent")+
scale_fill_scico(palette="lajolla")

library(leaflet)

# Create a color palette for your fill variable
pal <- colorNumeric(
  palette = scico(10, palette = "lajolla"),
  domain = locations_3km$dif_dist_all
)

# Create an interactive leaflet map
leaflet(locations_3km) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(dif_dist_all),
    weight = 1,
    color = "transparent",
    fillOpacity = 0.99,
    popup = ~paste("GRID ID:", GRID_ID, "<br>",
                   #"Longitude:", lon, "<br>",
                   #"Latitude:", lat, "<br>",
                   "Dif in Distance (meters):", dif_dist_all)
  ) %>%
  addLegend(
    pal = pal,
    values = ~dif_dist_all,
    title = "Dif in Distance (meters)",
    opacity = 1
  )


# ggarrange(historical_distance,counterfactual_veg,align="v",ncol=1)
# ggsave("Figures/Avoided_Veg_Damage_5km.png",dpi=300)

glimpse(Guer_Mangv)

Guer_Mangv <- Guer_Mangv %>%
  mutate(calculated_area = (st_area(Shape)),  # Calculate area in square meters
         area_weighted_veg_change = calculated_area * counterfactual_veg_change_dif, 
         area_weighted_veg_change_lower_ci = calculated_area * lower_ci, 
         area_weighted_veg_change_upper_ci = calculated_area * upper_ci) %>%
  mutate(calculated_area = (st_area(Shape)),  # Calculate area in square meters
         area_veg_change = calculated_area *veg_change)

glimpse(Guer_Mangv)
   veg_avoided <- sum(Guer_Mangv$area_weighted_veg_change,na.rm=T)
   veg_avoided_lci <-sum(Guer_Mangv$area_weighted_veg_change_lower_ci,na.rm=T)
   veg_avoided_uci <-sum(Guer_Mangv$area_weighted_veg_change_upper_ci,na.rm=T)
   veg_base <-sum(Guer_Mangv$area_veg_change,na.rm=T)

100*veg_avoided/veg_base
100*veg_avoided_lci/veg_base
100*veg_avoided_uci/veg_base

1642080/138788316


   # DO NOT ERASE - SAVE TO DO FIELDWORK
    #
    Guerrero_Mangroves_Field2<- Guer_Mangv %>% 
    #mutate(Percent_area_DAM = Percent_area_Damaged + Percent_area_Destroyed, 
    #Veg_Pct_Ch = veg_change*100) %>% 
    select(
      GRID_ID, Dist2Coast,dif_distance_meters,area_veg_change,area_weighted_veg_change
    ) %>% 
    rename(dif_dist=dif_distance_meters,VegA_m2_b=area_veg_change,VegA_m2_av=area_weighted_veg_change
    ) 
     st_write(Guerrero_Mangroves_Field2 , "Data/Guerrero_Mangroves_Field2_DamagedVeg.shp", delete_layer = TRUE)
    #
    # DO NOT ERASE - SAVE TO DO FIELDWORK




