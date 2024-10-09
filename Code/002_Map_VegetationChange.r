    Guer_Mang <- readRDS(file = "Guerrero_Gridcells_Mangroves.rds")
    glimpse(Guer_Mang)

   
    Guer_Mang <- Guer_Mang %>%
          #filter(NEAR_DIST<6) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin10 = cut(Dist2Coast,, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Dist2Mang_bin10 = cut(Dist2Mang, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")))
    
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
        scale_fill_scico(palette="berlin", direction=-1,
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
    
    veg_dist_scatter_bins10 <- ggplot(Guer_Mang %>% filter(veg_change!=0),aes(x=Dist2Mang,y=100*veg_change,col=Dist2Coast_bin10)) +
        #geom_point(alpha=0.1) + 
        scale_color_scico_d(begin=0.1,end=0.9) + 
        geom_smooth(method="lm") + 
        theme_minimal() + 
        coord_cartesian(ylim = c(-40, 5)) + 
        labs(color="Distance to Coast",y="Vegetation Change \nAfter Hurricane Otis (%)",x = "Distance to Closest Mangrove (km)") 
    
    veg_dist_scatter_bins_man <- ggplot(Guer_Mang %>% filter(veg_change!=0),aes(x=Dist2Coast,y=100*veg_change,col=Dist2Mang_bin10)) +
        #geom_point(alpha=0.1) + 
        scale_color_scico_d(begin=0.1,end=0.9) + 
        geom_smooth(method="lm") + 
        theme_minimal() #+ 
        #coord_cartesian(ylim = c(-40, 5)) + 
        #labs(color="Distance to Coast",y="Vegetation Change \nAfter Hurricane Otis (%)",x = "Distance to Closest Mangrove (km)") 
    

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
    #ggsave("Figures/VegChange_Dist2Coast_Dist2Mang.png",dpi=600)
    # Adjust the margin of each plot
    plot_d2c_bin2 <- plot_d2c_bin + theme(plot.margin = margin(10, 10, 10, 10))
    plot_d2m2 <- plot_d2m + theme(plot.margin = margin(10, 10, 10, 10))
    plot_veg2 <- plot_veg + theme(plot.margin = margin(10, 10, 10, 10))


    plot_Guerrero_sc <- ggarrange(
                            
                            ggarrange(plot_d2c_bin2,plot_d2m2,plot_veg2,ncol=3,align="hv"),veg_dist_scatter_bins,
                            ncol=1,widths=c(2,3)) 
    plot_Guerrero_sc
    #ggsave("Figures/VegChange_Dist2Coast_Dist2Mang_horizontal.png",dpi=600)



plot_data <- ggplot_build(veg_dist_scatter_bins)$data[[2]]# Calculate the first derivative (difference between consecutive y values)
first_derivative <- diff(plot_data$y) / diff(plot_data$x)

# Find the index where the first derivative changes from negative to positive
local_min_index <- which(diff(sign(first_derivative)) == 2) + 1
local_min_index <- local_min_index[-1]

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




    model_veg_lin_bins <- felm(veg_change~Dist2Mang*Dist2Coast_bin | 0|0|0,Guer_Mang )
    summary(model_veg_lin_bins)
    # Extract model coefficients
coefficients <- coef(model_veg_lin_bins)
# Create the prediction data frame
prediction_data <- prediction_data %>%
  mutate(
    veg_change_pred = coefficients["(Intercept)"] +
      coefficients["Dist2Mang"] * Dist2Mang +
      coefficients["Dist2Coast_binIntermediate"] * (Dist2Coast_bin == "Intermediate") +
      coefficients["Dist2Coast_binFar"] * (Dist2Coast_bin == "Far") +
      coefficients["Dist2Mang:Dist2Coast_binIntermediate"] * Dist2Mang * (Dist2Coast_bin == "Intermediate") +
      coefficients["Dist2Mang:Dist2Coast_binFar"] * Dist2Mang * (Dist2Coast_bin == "Far")
  )

# Plot the predicted vegetation change
ggplot(prediction_data, aes(x = Dist2Mang, y = veg_change_pred, color = Dist2Coast_bin)) +
  geom_line(size = 1) +
  labs(
    title = "Predicted Effect of Distance to Mangroves and Coastline on Vegetation Change",
    x = "Distance to Mangroves (km)",
    y = "Predicted Vegetation Change",
    color = "Distance to Coast"
  ) +
  theme_minimal() +
  scale_color_scico_d(begin = 0.1, end = 0.9)







    model_veg_sq_bins10 <- felm(veg_change~Dist2Mang + I(Dist2Mang^2)|Dist2Coast_bin10|0|0,Guer_Mang )
    summary(model_veg_sq_bins10)
    fixed_effects <- getfe(model_veg_sq_bins10)

    Guer_Mang <- Guer_Mang %>%
      left_join(fixed_effects, by = c("Dist2Coast_bin10" = "idx")) %>%
      mutate(demeaned_veg_change = veg_change - effect)

    ggplot(Guer_Mang, aes(x = Dist2Mang, y = demeaned_veg_change)) +
      geom_point(alpha = 0.5, aes(col=Dist2Coast_bin10)) +
      geom_smooth(method = "loess") +
      labs(
        title = "De-meaned Damaged Households vs Distance to Mangroves",
        x = "Distance to Mangroves (km)",
        y = "De-meaned Damaged Households"
      ) +
      theme_minimal()    

      ggplot(Guer_Mang, aes(x = Dist2Mang, y = demeaned_veg_change)) +
  geom_bin2d(bins = 30) +  # Adjust the number of bins as needed
  scale_fill_viridis_c(option = "plasma") +  # Choose a color scale, e.g., viridis
  labs(
    title = "Heatmap of De-meaned Damaged Households vs Distance to Mangroves",
    x = "Distance to Mangroves (km)",
    y = "De-meaned Damaged Households"
  ) +
  theme_minimal()+
      geom_smooth(method = "loess") 
    
    glimpse(Guer_Mang)
    Guer_Mang2 <- Guer_Mang %>%
          filter(!is.na(Percent_area_Destroyed)) %>%
          mutate(Dist2Mang_bin = cut(Dist2Mang, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin = cut(Dist2Coast, breaks = 3, labels = c("Close", "Intermediate", "Far")), 
          Dist2Coast_bin10 = cut(Dist2Coast,, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")), 
          Dist2Mang_bin10 = cut(Dist2Mang, breaks = 10, labels = c("Closest", "2","3","4","5","6","7","8","9", "Farthest")))
    

    model_dest_sq_bins <- felm(Percent_area_Destroyed~Dist2Mang |Dist2Coast_bin10|0|0,Guer_Mang2 )
    summary(model_dest_sq_bins)
    fixed_effects <- getfe(model_dest_sq_bins)

    Guer_Mang2<- Guer_Mang2 %>%
      left_join(fixed_effects, by = c("Dist2Coast_bin10" = "idx")) %>%
      mutate(demeaned_Percent_area_Destroyed = Percent_area_Destroyed - effect.y)

    ggplot(Guer_Mang2, aes(x = Dist2Mang, y = demeaned_Percent_area_Destroyed)) +
      geom_point(alpha = 0.5, aes(col=Dist2Coast_bin10)) +
      geom_smooth(method = "loess") +
      labs(
        title = "De-meaned Damaged Households vs Distance to Mangroves",
        x = "Distance to Mangroves (km)",
        y = "De-meaned Damaged Households"
      ) +
      theme_minimal()    
    

    
    model_house_sq_bins <- felm(damaged_households~Dist2Mang+ I(Dist2Mang^2) |Dist2Coast_bin10|0|0,Guer_Mang )
    summary(model_house_sq_bins)
    # fixed_effects <- getfe(model_house_sq_bins)

    # Guer_Mang <- Guer_Mang %>%
    #   left_join(fixed_effects, by = c("Dist2Coast_bin10" = "idx")) %>%
    #   mutate(demeaned_damaged_households = damaged_households - effect)

    # ggplot(Guer_Mang, aes(x = Dist2Mang, y = demeaned_damaged_households)) +
    #   geom_point(alpha = 0.5, aes(col=Dist2Coast_bin10)) +
    #   geom_smooth(method = "loess") +
    #   labs(
    #     title = "De-meaned Damaged Households vs Distance to Mangroves",
    #     x = "Distance to Mangroves (km)",
    #     y = "De-meaned Damaged Households"
    #   ) +
    #   theme_minimal()    
    
    model_veg_sq_bins <- felm(veg_change~Dist2Mang:Dist2Coast_bin + I(Dist2Mang^2):Dist2Coast_bin| 0|0|0,Guer_Mang )
    summary(model_veg_sq_bins)
    
    
    
    
    
    
    
    # Extract model coefficients
coefficients_sq <- coef(model_veg_sq_bins)

# Create the prediction data frame
prediction_data_sq <- expand.grid(
  Dist2Mang = seq(0, max(Guer_Mang$Dist2Mang, na.rm = TRUE), length.out = 100),
  Dist2Coast_bin = unique(Guer_Mang$Dist2Coast_bin)
)

# Calculate the predicted values manually
prediction_data_sq <- prediction_data_sq %>%
  mutate(
    veg_change_pred = coefficients_sq["(Intercept)"] +
      coefficients_sq["Dist2Mang:Dist2Coast_binClose"] * (Dist2Mang * (Dist2Coast_bin == "Close")) +
      coefficients_sq["Dist2Mang:Dist2Coast_binIntermediate"] * (Dist2Mang * (Dist2Coast_bin == "Intermediate")) +
      coefficients_sq["Dist2Mang:Dist2Coast_binFar"] * (Dist2Mang * (Dist2Coast_bin == "Far")) +
      coefficients_sq["Dist2Coast_binClose:I(Dist2Mang^2)"] * (I(Dist2Mang^2) * (Dist2Coast_bin == "Close")) +
      coefficients_sq["Dist2Coast_binIntermediate:I(Dist2Mang^2)"] * (I(Dist2Mang^2) * (Dist2Coast_bin == "Intermediate")) +
      coefficients_sq["Dist2Coast_binFar:I(Dist2Mang^2)"] * (I(Dist2Mang^2) * (Dist2Coast_bin == "Far"))
  ) %>% 
  mutate(veg_change_pred = as.numeric(unlist(veg_change_pred)))
    class(prediction_data_sq)
    glimpse(prediction_data_sq)
    
    
    # Plot the predicted vegetation change
ggplot(prediction_data_sq, aes(x = Dist2Mang, y = veg_change_pred, color = Dist2Coast_bin)) +
  geom_point(size = 1) +
  labs(
    title = "Predicted Effect of Distance to Mangroves and Coastline (Quadratic Interaction) on Vegetation Change",
    x = "Distance to Mangroves (km)",
    y = "Predicted Vegetation Change",
    color = "Distance to Coast"
  ) +
  theme_minimal() +
  scale_color_scico_d(begin = 0.1, end = 0.9) + 
  ylim(c(-0.2,0.01))

# Extract the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model_veg_sq_bins)

# Calculate the standard errors for each prediction
# Extract the variance-covariance matrix of the model coefficients
vcov_matrix <- vcov(model_veg_sq_bins)

# Create the prediction data frame with predictions and standard errors
prediction_data_sq <- prediction_data_sq %>%
  mutate(
    veg_change_pred = coefficients_sq["(Intercept)"] +
      coefficients_sq["Dist2Mang:Dist2Coast_binClose"] * (Dist2Mang * (Dist2Coast_bin == "Close")) +
      coefficients_sq["Dist2Mang:Dist2Coast_binIntermediate"] * (Dist2Mang * (Dist2Coast_bin == "Intermediate")) +
      coefficients_sq["Dist2Mang:Dist2Coast_binFar"] * (Dist2Mang * (Dist2Coast_bin == "Far")) +
      coefficients_sq["Dist2Coast_binClose:I(Dist2Mang^2)"] * (I(Dist2Mang^2) * (Dist2Coast_bin == "Close")) +
      coefficients_sq["Dist2Coast_binIntermediate:I(Dist2Mang^2)"] * (I(Dist2Mang^2) * (Dist2Coast_bin == "Intermediate")) +
      coefficients_sq["Dist2Coast_binFar:I(Dist2Mang^2)"] * (I(Dist2Mang^2) * (Dist2Coast_bin == "Far")),
    
    # Standard error calculation
    se = sqrt(
      vcov_matrix["(Intercept)", "(Intercept)"] +
      (Dist2Mang * (Dist2Coast_bin == "Close"))^2 * vcov_matrix["Dist2Mang:Dist2Coast_binClose", "Dist2Mang:Dist2Coast_binClose"] +
      (Dist2Mang * (Dist2Coast_bin == "Intermediate"))^2 * vcov_matrix["Dist2Mang:Dist2Coast_binIntermediate", "Dist2Mang:Dist2Coast_binIntermediate"] +
      (Dist2Mang * (Dist2Coast_bin == "Far"))^2 * vcov_matrix["Dist2Mang:Dist2Coast_binFar", "Dist2Mang:Dist2Coast_binFar"] +
      (I(Dist2Mang^2) * (Dist2Coast_bin == "Close"))^2 * vcov_matrix["Dist2Coast_binClose:I(Dist2Mang^2)", "Dist2Coast_binClose:I(Dist2Mang^2)"] +
      (I(Dist2Mang^2) * (Dist2Coast_bin == "Intermediate"))^2 * vcov_matrix["Dist2Coast_binIntermediate:I(Dist2Mang^2)", "Dist2Coast_binIntermediate:I(Dist2Mang^2)"] +
      (I(Dist2Mang^2) * (Dist2Coast_bin == "Far"))^2 * vcov_matrix["Dist2Coast_binFar:I(Dist2Mang^2)", "Dist2Coast_binFar:I(Dist2Mang^2)"]
    ),
    
    lower_ci = veg_change_pred - 1.96 * se,
    upper_ci = veg_change_pred + 1.96 * se
  ) %>% mutate( se = as.double(unlist(se)), 
   lower_ci = as.double(unlist(lower_ci)), 
   upper_ci = as.double(unlist(upper_ci)), 
   veg_change_pred = as.double(unlist(veg_change_pred)))

glimpse(prediction_data_sq)

  # Plot the predicted vegetation change with confidence intervals
ggplot(prediction_data_sq, aes(x = Dist2Mang, y = veg_change_pred, color = Dist2Coast_bin)) +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = Dist2Coast_bin), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  labs(
    title = "Predicted Effect of Distance to Mangroves and Coastline (Quadratic Interaction) on Vegetation Change",
    x = "Distance to Mangroves (km)",
    y = "Predicted Vegetation Change",
    color = "Distance to Coast",
    fill = "Distance to Coast"
  ) +
  theme_minimal() +
  scale_color_scico_d(begin = 0.1, end = 0.9) +
  scale_fill_scico_d(begin = 0.1, end = 0.9) +
  ylim(c(-0.25, 0.01))






prediction_data_sq <- prediction_data_sq %>%
  mutate(
    derivative = coefficients_sq["Dist2Mang:Dist2Coast_binClose"] * (Dist2Coast_bin == "Close") +
      coefficients_sq["Dist2Mang:Dist2Coast_binIntermediate"] * (Dist2Coast_bin == "Intermediate") +
      coefficients_sq["Dist2Mang:Dist2Coast_binFar"] * (Dist2Coast_bin == "Far") +
      2 * coefficients_sq["Dist2Coast_binClose:I(Dist2Mang^2)"] * Dist2Mang * (Dist2Coast_bin == "Close") +
      2 * coefficients_sq["Dist2Coast_binIntermediate:I(Dist2Mang^2)"] * Dist2Mang * (Dist2Coast_bin == "Intermediate") +
      2 * coefficients_sq["Dist2Coast_binFar:I(Dist2Mang^2)"] * Dist2Mang * (Dist2Coast_bin == "Far")
  )

prediction_data_sq <- prediction_data_sq %>%
  mutate(
    se_derivative = sqrt(
      (Dist2Coast_bin == "Close")^2 * vcov_matrix["Dist2Mang:Dist2Coast_binClose", "Dist2Mang:Dist2Coast_binClose"] +
      (Dist2Coast_bin == "Intermediate")^2 * vcov_matrix["Dist2Mang:Dist2Coast_binIntermediate", "Dist2Mang:Dist2Coast_binIntermediate"] +
      (Dist2Coast_bin == "Far")^2 * vcov_matrix["Dist2Mang:Dist2Coast_binFar", "Dist2Mang:Dist2Coast_binFar"] +
      (2 * Dist2Mang * (Dist2Coast_bin == "Close"))^2 * vcov_matrix["Dist2Coast_binClose:I(Dist2Mang^2)", "Dist2Coast_binClose:I(Dist2Mang^2)"] +
      (2 * Dist2Mang * (Dist2Coast_bin == "Intermediate"))^2 * vcov_matrix["Dist2Coast_binIntermediate:I(Dist2Mang^2)", "Dist2Coast_binIntermediate:I(Dist2Mang^2)"] +
      (2 * Dist2Mang * (Dist2Coast_bin == "Far"))^2 * vcov_matrix["Dist2Coast_binFar:I(Dist2Mang^2)", "Dist2Coast_binFar:I(Dist2Mang^2)"]
    ),
    lower_ci_derivative = derivative - 1.96 * se_derivative,
    upper_ci_derivative = derivative + 1.96 * se_derivative
  )

glimpse(prediction_data_sq)


# Plot the derivative of the predicted vegetation change with confidence intervals
derivative_effect <- ggplot(prediction_data_sq, aes(x = Dist2Mang, y = 100*derivative, color = Dist2Coast_bin)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = 100*lower_ci_derivative, ymax = 100*upper_ci_derivative, fill = Dist2Coast_bin), alpha = 0.2, color = NA) +
  labs(
    title = "Marginal Effect",
    x = "Distance to Closest Mangrove (km)",
    y = "Vegetation Change (%)",
    color = "Distance to Coast",
    fill = "Distance to Coast"
  ) +
  theme_minimal() +
  scale_color_scico_d(begin = 0.1, end = 0.9) +
  scale_fill_scico_d(begin = 0.1, end = 0.9) +
        coord_cartesian(ylim = c(-5, 0.1),xlim=c(0,5))



plot_Guerrero_sc <- ggarrange(
                            
                            ggarrange(plot_d2c_bin2,plot_d2m2,plot_veg2,ncol=3,align="hv"),ggarrange(veg_dist_scatter_bins_annotations+labs(title="Total Relationship"),derivative_effect,common.legend=TRUE,legend="bottom",ncol=2),
                            ncol=1,widths=c(2,3)) 
    plot_Guerrero_sc
    
    ggsave("Figures/VegChange_Dist2Coast_Dist2Mang_horizontal_marginal.ong",dpi=600)



