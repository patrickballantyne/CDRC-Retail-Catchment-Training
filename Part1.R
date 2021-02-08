## CDRC Retail Catchment Training - Part 1

# Setup -------------------------------------------------------------------

## Install and Load all the libraries that you need:
library(sf)
library(dplyr)
library(tmap)
# install.packages("hereR")
library(hereR)

## Make sure you set your working directory to the folder we have provided
#setwd("CDRC-Retail-Catchment-Training")

# 1. Data (& Preprocessing) -----------------------------------------------

## Read in the Retail Centre Boundaries 
rc <- st_read("Data/LCR_polygons.gpkg")

## Extract centroids from the Retail Centre Boundaries
rc_cent <- st_centroid(rc)

## Map the centroids - note: tm_dots() is used as the object rc_cent contains point data (retail centre centroids)
tmap_mode("view")
tm_shape(rc_cent) +
  tm_dots()


# 2. Creating a Retail Hierarchy ------------------------------------------

# Hypothetical point count (remove)
rc_cent$n_pts <- sample(300, size = nrow(rc), replace = TRUE)

## Use mutate and case_when to create the new hierarchy column
## notice how ~ assigns the new values based on the condition
rc_cent_nopipes <-  mutate(rc_cent, 
                           hierarchy = dplyr::case_when(n_pts < 100 ~ "tertiary",
                                                        n_pts >= 100 & n_pts < 250 ~ "secondary",
                                                        n_pts >= 250 ~ "primary"))

## Use select to extract the id, n_pts and hierarchy columns
rc_cent_nopipes <- select(rc_cent_nopipes, id, n_pts, hierarchy)

## Use pipes to create the new hierarchy column and then select the other columns we are interested in
rc_cent <- rc_cent %>% 
  dplyr::mutate(hierarchy = dplyr::case_when(n_pts < 100 ~ "tertiary",
                                             n_pts >= 100 & n_pts < 250 ~ "secondary",
                                             n_pts >= 250 ~ "primary")) %>%
  dplyr::select(id, n_pts, hierarchy, geom)


# 3. Catchments (1) - Fixed-Ring Buffers ----------------------------------

## Extract a 1000m buffer for each retail centre
buffer1km <- st_buffer(rc_cent, 1000)

## Map them
tm_shape(buffer1km) + ## Plot the buffers
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "black")

## This function delineates a 1000m catchment for retail centres classed as primary
## Run this chunk to save the function to your environment
get_primary_buffer <- function(centroids) {
  
  ## Extracts centroids of 'primary' retail centres
  rc_primary <- filter(centroids, hierarchy == "primary")
  
  ## Constructs a 1000m buffer
  primary_buffer <- st_buffer(rc_primary, dist = 1000)
  
  ## Return the 1000m buffer
  return(primary_buffer)
}

## Use the function to get primary buffers for LCR Retail Centres
buffer1000 <- get_primary_buffer(rc_cent)

## Plot the primary buffers and all the centroids, to check only primary centres have catchments
tm_shape(buffer1000) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) +
  tm_dots()

## This function delineates fixed-ring buffers that differ in size depending on hierarchy
## Run this chunk to save the function to your environment
get_buffer <- function(centroids, primary_dist = 1000, secondary_dist = 500, tertiary_dist = 250) {
  
  ## Split up the retail centres based on hierarchy
  rc_primary <- filter(centroids, hierarchy == "primary")
  rc_secondary <- filter(centroids, hierarchy == "secondary")
  rc_tertiary <- filter(centroids, hierarchy == "tertiary")
  
  ## Run the buffer for the different retail centre hierarchies separately
  primary_buffer <- st_buffer(rc_primary, dist = primary_dist)
  secondary_buffer <- st_buffer(rc_secondary, dist = secondary_dist)
  tertiary_buffer <- st_buffer(rc_tertiary, dist = tertiary_dist)
  
  ## Join together
  buffer <- rbind(primary_buffer, secondary_buffer, tertiary_buffer)
  return(buffer) ## Return
}

## Run the function to get fixed-ring buffers for the 3 different hierarchies
hbuffer <- get_buffer(rc_cent, primary_dist = 3000, secondary_dist = 2000, tertiary_dist = 1000)

## Map them to see what they look like
tm_shape(hbuffer)+ ## Plot the varying fixed-ring buffers
  tm_fill(col = "hierarchy", alpha = 0.5) + # Setting col to 'hierarchy' tells tmap to generate a different colour buffer for each value in the hierarchy column
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "black", alpha = 0.75)


# 4. Catchments (2) - Drive-Time Catchments -------------------------------

## Set API key
#set_key("insert-key-here")

## Extract first retail centre
rc_a <- rc_cent[1, ]

## Extract the 10-minute driving catchment
iso_a <- isoline(rc_a, range = (10 * 60), range_type = "time", transport_mode = "car")

## Map the drive-time catchment for the first retail centre
tm_shape(iso_a) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(rc_a) +
  tm_dots()

## Extract the 10-minute catchment for every retail centre in LCR
iso <- isoline(rc_cent, range = (10 * 60), range_type = "time", transport_mode = "car", aggregate = FALSE)

## Map the 10-minute drive-time catchments for LCR retail centres
tm_shape(iso) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) +
  tm_dots()

## This next function extracts drive-time catchments for primary centres
## So run the next chunk of code to save the get_primary_drive_time() function to your environment:
## Function to get drive-time catchments for the primary retail centres
get_primary_drive_time <- function(centroids, dist = 5, range_type = "time", transport_mode = "car") {
  
  ## Filter the centroids to extract the primary centres
  rc_primary <- filter(centroids, hierarchy == "primary")
  
  ## Build the drive-time catchment
  primary_drive_time <- isoline(rc_primary, range = (dist * 60), 
                                range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  
  ## Clean up the isoline - join on the retail centre information
  rc_primary <- rc_primary %>%
    as.data.frame() %>% 
    select(id, n_pts, hierarchy) %>%
    bind_cols(primary_drive_time) %>% ## Equivalent of cbind(), but for piping
    st_as_sf() ## Ensures final object is SF not dataframe
  return(rc_primary)
}

## Use the function to return drive-time catchments for primary centres
primary_iso <- get_primary_drive_time(rc_cent, dist = 10, range_type = "time", transport_mode =  "car")

## Map the primary drive-timecatchments
tm_shape(primary_iso) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(rc_cent) +
  tm_dots()

## This final function extracts drive-time catchments that differ in size for each of the retail centre hierarchies
## Run this chunk to save this function to your environment
get_drive_time <- function(centroids, primary_dist = 20, secondary_dist = 15, tertiary_dist = 10, 
                           range_type = "time", transport_mode = "car") {
  
  ## Split up the retail centres based on hierarchy
  rc_primary <- filter(centroids, hierarchy == "primary")
  rc_secondary <- filter(centroids, hierarchy == "secondary")
  rc_tertiary <- filter(centroids, hierarchy == "tertiary")
  
  ## Delineate the isolines for the different retail centre hierarchies separately
  primary_drive_time <- isoline(rc_primary, range = (primary_dist * 60),
                                range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  secondary_drive_time <- isoline(rc_secondary, range = (secondary_dist * 60),
                                  range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  tertiary_drive_time <- isoline(rc_tertiary, range = (tertiary_dist * 60),
                                 range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  
  ## Join the retail centre info onto each set of catchments
  primary <- rc_primary %>%
    as.data.frame() %>%
    select(id, n_pts, hierarchy) %>%
    bind_cols(primary_drive_time)
  secondary <- rc_secondary %>%
    as.data.frame() %>%
    select(id, n_pts, hierarchy) %>%
    bind_cols(secondary_drive_time)
  tertiary <- rc_tertiary %>%
    as.data.frame() %>%
    select(id, n_pts, hierarchy) %>%
    bind_cols(tertiary_drive_time)
  
  ## Join catchments together
  isolines <- st_as_sf(rbind(primary, secondary, tertiary))
  return(isolines)
}

## Use the function to extract the hierarchical drive time catchments for LCR retail centres
iso_hierarchy <- get_drive_time(rc_cent, primary_dist = 20, secondary_dist = 15, tertiary_dist = 10, 
                                range_type = "time", transport_mode = "car")

## Map them
tm_shape(iso_hierarchy) + ## Plot the hierarchical drive-time catchments 
  tm_fill(col = "hierarchy", alpha = 0.5) + ## Setting col = 'hierarchy' tells tmap to plot a different colour for each value of hierarchy
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots()

## Filter and plot catchments of interest
iso_hierarchy %>%
  filter(id == "TC0225") %>%
  tm_shape() + ## Notice how tm_shape can be left empty as you have piped directly from the iso_hierarchy object
  tm_fill(col = "orange", alpha = 0.5)
