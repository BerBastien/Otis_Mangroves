dir <- "C:\\Users\\basti\\Documents\\GitHub\\Otis_Mangroves"
datadir <- "C:\\Users\\basti\\Box\\Data\\Oceans\\Otis"

setwd(dir)
    libraries <- c("sf", "raster", "sp", "exactextractr", "tidyverse", "ggpubr", 
                "rnaturalearth", "ggmap", "ggspatial", "prettymapr", "terra","tidyverse","lfe","scico","stargazer","geosphere")

    # Load all libraries in one command
    lapply(libraries, library, character.only = TRUE)
    install.packages("geosphere")

# Function to create the triangle for each point, ensuring it is closed
create_triangle <- function(point, bearing, distance) {
  angle1 <- bearing - 22.5  # Adjust angle for the triangle's left edge
  angle2 <- bearing + 22.5  # Adjust angle for the triangle's right edge
  
  # Get the coordinates of the point
  point_coords <- st_coordinates(point)[1, 1:2]
  
  # Calculate the points for the triangle
  point1 <- destPoint(point_coords, angle1, distance)[1,]
  point2 <- destPoint(point_coords, angle2, distance)[1,]
  
  # Create the matrix of coordinates and close the polygon
  coords <- rbind(
    point_coords,
    point1,
    point2,
    point_coords  # Close the polygon by repeating the first point
  )
  
  st_polygon(list(coords))
}

    gdb_path <- paste0(datadir,"\\Otis.gdb")
Otis_route <- st_read(gdb_path, layer = "hurricaneotisadvisories")
    # Convert to data frame, arrange by latitude, remove NAs, and convert back to sf
    otis_sf_clean <- Otis_route %>%
    st_coordinates() %>%
    as.data.frame() %>%
    cbind(Otis_route, .) %>%
    arrange(Y) %>%
    filter(!is.na(X) & !is.na(Y)) %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(Otis_route))

    # Combine points into MULTIPOINT and cast to LINESTRING
    otis_line <- otis_sf_clean %>%
    st_combine() %>%
    st_cast("LINESTRING")
    
