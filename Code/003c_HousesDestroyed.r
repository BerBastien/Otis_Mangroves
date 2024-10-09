
    Guer_Mang <- readRDS(file = "Guerrero_Gridcells_Mangroves.rds")
    glimpse(Guer_Mang)
    Guer_Mang$dif_distance_meters <- Guer_Mang$Dist2Mang_hist_meters - Guer_Mang$Dist2Mang_meters

    Guer_Mangh <- Guer_Mang %>%
          filter(!is.na(damaged_households)) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang_meters, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin10 = cut(Dist2Coast,, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Dist2Mang_bin10 = cut(Dist2Mang_meters, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")))

    model_hou_lin_bins <- felm(damaged_households~Dist2Mang_meters*Dist2Coast  |0|0|0,Guer_Mangh )
    summary(model_hou_lin_bins)


    lincoef2 <- summary(model_hou_lin_bins)$coefficients[2]
    lincoef2_se <- summary(model_hou_lin_bins)$coefficients[6]
    Guer_Mangh <- Guer_Mangh %>% 
        mutate(counterfactual_house_dif  = lincoef2 *dif_distance_meters, 
            counterfactual_house_dif_lower_ci = counterfactual_house_dif  - 1.96 * (lincoef2_se * dif_distance_meters),
            counterfactual_house_dif_upper_ci = counterfactual_house_dif  + 1.96 * (lincoef2_se * dif_distance_meters)
         ) %>% 
         mutate(counterfactual_house_dif = ifelse(counterfactual_house_dif < -damaged_households,-damaged_households,counterfactual_house_dif ))

    bbox3 <- st_bbox(Guer_Mangh)
 counterfactual_house <- ggplot(data = Guer_Mangh) +
        annotation_map_tile(zoom = 10, type = "osm") +
        geom_sf(aes(fill = -counterfactual_house_dif), col = "transparent", alpha = 0.85) +
        
        scale_fill_scico(palette="bamako", direction=-1,
                            #limits = c(0, 1), 
                            oob = scales::squish,
                            guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
        labs(title = "D. Effect on Individual Houses",
            fill = "Avoided Destroyed \nHouses (No. of households)",
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
    coord_sf(xlim = c(bbox3["xmin"], bbox3["xmax"]), ylim = c(bbox3["ymin"], bbox3["ymax"]))  


glimpse(Guer_Mangh)
   # DO NOT ERASE - SAVE TO DO FIELDWORK
    #
    Guerrero_Mangroves_Field4 <- Guer_Mangh %>% 
    #mutate(Percent_area_DAM = Percent_area_Damaged + Percent_area_Destroyed, 
    #Veg_Pct_Ch = veg_change*100) %>% 
    select(
      GRID_ID, Dist2Coast,dif_distance_meters,damaged_households,counterfactual_house_dif
    ) %>% 
    rename(dif_dist=dif_distance_meters,DamHous_b=damaged_households,DamHous_av=counterfactual_house_dif
    ) 
     st_write(Guerrero_Mangroves_Field4 , "Data/Guerrero_Mangroves_Field4_DamagedHouseholds.shp", delete_layer = TRUE)
    #
    # DO NOT ERASE - SAVE TO DO FIELDWORK
    
 hou_avoided <- sum(Guer_Mangh$counterfactual_house_dif)
 hou_avoided_lci <- sum(Guer_Mangh$counterfactual_house_dif_lower_ci)
 hou_avoided_uci <- sum(Guer_Mangh$counterfactual_house_dif_upper_ci)
 hou_base <- sum(Guer_Mangh$damaged_households)

results_df <- data.frame(avoided=c(as.numeric(veg_avoided)*0.0001,-dam_avoided*0.0001,-hou_avoided),
    avoided_lci = c(as.numeric(veg_avoided_lci)*0.0001,-dam_avoided_lci*0.0001,-hou_avoided_lci), 
    avoided_uci = c(as.numeric(veg_avoided_uci)*0.0001,-dam_avoided_uci*0.0001,-hou_avoided_uci),
    base = c(as.numeric(veg_base)*0.0001,-dam_base*0.0001,-hou_base),
    class=c("Damaged\nVegetation","Damaged\nNeighborhoods","Destroyed\nHouseholds") , 
    units = c("ha","ha","houses") ) %>% 
    mutate(perc_avoided = 100*avoided/base,
    perc_avoided_lci  = 100*avoided_lci /base,
    perc_avoided_uci  = 100*avoided_uci /base)
results_df
results_df$class <- factor(results_df$class, levels = c("Damaged\nNeighborhoods", "Destroyed\nHouseholds",  "Damaged\nVegetation"))

# Create the plot
Avoided_Loss <- ggplot(results_df, aes(y = class, x = -100*avoided/base)) +
  # Add the vertical dashed line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  
  # Add the uncertainty lines with a more elegant style
  geom_errorbarh(aes(xmin = -100*avoided_lci/base, xmax = -100*avoided_uci/base), height = 0.2, color = "black", size = 1.2) +
  
  # Add the mean circles
  geom_point(size = 2, shape = 21, fill = "white", color = "black") +
  
  # Add text labels for the mean value with units
  geom_text(aes(label = paste0(round(avoided, 0), " ", units)), vjust = -2, hjust = 0, color = "black", size = 4) +
  
  # Customize the theme for a clean and elegant look
  theme_minimal() +
  labs(title = "B. Avoided Damage",
       x = "Avoided Losses (% of Total Loss)",
       y = "") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold")
    #axis.text.x = element_text(size = 12)
  ) +
  
  # Adjust the scale to provide some padding
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1)))
Avoided_Loss
ggarrange(historical_distance,Avoided_Loss,align="v",ncol=2)
ggsave("Figures/AvoidedDamages_Estimate.jpg",dpi=300)

## Plots
counterfactual_veg2 <- counterfactual_veg + theme(plot.margin = margin(10, 10, 10, 10))
    counterfactual_des2 <- counterfactual_des+ theme(plot.margin = margin(10, 10, 10, 10))
    counterfactual_house2 <- counterfactual_house + theme(plot.margin = margin(10, 10, 10, 10))

ggarrange(historical_distance,ggarrange(counterfactual_veg2,counterfactual_des2, counterfactual_house2,ncol=3,align="hv"),align="v",ncol=1)
ggsave("Figures/AvoidedDamages.jpg",dpi=300)


install.packages("stargazer")
library("stargazer")
stargazer( model_veg_lin_interaction,model_hou_lin_bins,model_des_lin_bins ,type="html",out="Tables/DamageModels2.html")



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

ggarrange(historical_distance,counterfactual_veg,align="v",ncol=1)




    Guer_Mangd <- Guer_Mang %>%
          filter(!is.na(Percent_area_Destroyed)) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang_meters, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin10 = cut(Dist2Coast,, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Dist2Mang_bin10 = cut(Dist2Mang_meters, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")))

    model_des_sq_bins <- felm(Percent_area_Destroyed~Dist2Mang_meters |Dist2Coast_bin10|0|0,Guer_Mangd )
    summary(model_des_sq_bins)

