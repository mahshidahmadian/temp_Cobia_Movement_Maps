# Function to create an interactive map visualizing cobia movement trajectories

cobia_map <- function(Imputed_points, Receivers_df, detected_loc_vec, time_points_vec) {
  
  # Set up the locations where we actually detected the fish (observed data points)
  # These mark the start/end of segments with gaps that we had to impute the trajectory
  gap_circles <- data.frame(
    x0 = detected_loc_vec[, 1],  # longitude of detections
    y0 = detected_loc_vec[, 2]   # latitude of detections
  )

  N_Imps <- dim(Imputed_points)[3]  # number of imputed trajectory samples
  N_receivers <- nrow(Receivers_df)  # number of acoustic receivers
  
  #_______________ Build the map _______________________
  # fflatten the 3D array of imputed points into simple x,y vectors
  # This combines all trajectory samples into one dataset for the heatmap
  Point_vector1 <- as.vector(Imputed_points[, 1, ])  # all longitude coordinates
  Point_vector2 <- as.vector(Imputed_points[, 2, ])  # all latitude coordinates
  dff1 <- data.frame(x = Point_vector1, y = Point_vector2)
  
  # building the interactive map
  map_imp <- leaflet() %>%
    # Add the base map layer (OpenStreetMap gives nice detail for coastal areas)
    addProviderTiles(providers$OpenStreetMap) %>% 
    
    # Plot the acoustic receiver locations as red circles
    addCircleMarkers(
      lng = Receivers_df[, 1],     # receiver longitudes
      lat = Receivers_df[, 2],     # receiver latitudes
      radius = 4,                  # marker size
      color = "black",             # border color
      fillColor = "red",           # interior color (makes them stand out)
      fillOpacity = 0.8            # --- > they're clearly visible
    ) %>%
    
    # Add a heatmap showing the density of imputed trajectory points
    # Meaning: hot spots show where the fish likely spent more time or where multiple trajectories overlap
    addHeatmap(
      lng = ~x,                    # longitude from our flattened data
      lat = ~y,                    # latitude from our flattened data
      intensity = ~0.75,           # intensity of each point
      data = dff1,                 # the trajectory samples
      radius = 15,                 
      blur = 20,                   # smoothing factor
      max = 0.4                    # cap on intensity (prevents over-saturation)
    ) %>%
    
    # Add time labels at each detection location so we can see zat what time-point the fish was there
    addLabelOnlyMarkers(
      lng = gap_circles[, 1],      # longitude of each detection
      lat = gap_circles[, 2],      # latitude of each detection
      label = paste0("t=", time_points_vec),  # label showing time index
      labelOptions = labelOptions(
        noHide = TRUE,             # keep labels always visible
        textOnly = TRUE,           # no marker icon
        direction = "top",         # position label above point
        offset = c(0, 5),         
        style = list(
          "font-size" = "18px",    # size of text
          "font-weight" = "bold",  
          "color" = "black",
          "text-shadow" = "2px 2px 4px white, -2px -2px 4px white, 2px -2px 4px white, -2px 2px 4px white",
          "background-color" = "rgba(255, 255, 255, 0.8)",
          "padding" = "3px 6px", 
          "border-radius" = "3px"
        )
      )
    ) 

  return(map_imp)
}