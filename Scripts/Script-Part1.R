## CDRC Retail Catchment Training - Part 1

# Setup -------------------------------------------------------------------

## First we want to set up the libraries that we are going to be using for this practical - 
## most of them should be familiar (e.g. dplyr, sf, tmap). However, the hereR library will likely be new - 
## we are going to use this later, so install it for now and we will come back to it later.
library(sf)
library(dplyr)
library(tmap)
# install.packages("hereR")
library(hereR)

## Make sure you have set your working directory to wherever you have stored the CDRC-Retail-Catchment-Training folder we have provided:
#setwd("CDRC-Retail-Catchment-Training")


# 1. Data (& Preprocessing) -----------------------------------------------

## Read in the Retail Centre Boundaries 
rc <- st_read("Data/LCR_Retail_Centres_2018.gpkg")

## Setup plotting
tmap_mode("view")

## Map Retail Centre Polygons for LCR
tm_shape(rc) +
  tm_fill(col = "orange") +
  tm_text("rcName", size = 0.75)

## Extract centroids
rc_cent <- st_centroid(rc)

## Map the centroids - note: tm_dots() is used as the object rc_cent contains point data (retail centre centroids)
tm_shape(rc_cent) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)


# 2. Creating a Retail Hierarchy ------------------------------------------

## Use mutate and case_when to create the new column - notice how ~ assigns the new values based on the condition
rc_cent_nopipes <-  mutate(rc_cent, 
                           hierarchy = dplyr::case_when(n.comp.units < 50 & RetailPark == "N" ~ "tertiary",
                                                        (n.comp.units >= 50 & n.comp.units < 100) | RetailPark == "Y" ~ "secondary",
                                                        n.comp.units >= 100 ~ "primary"))

## Use select to extract the id, n.units, n.comp.units and hierarchy columns
rc_cent_nopipes <- select(rc_cent_nopipes, rcID, rcName, n.units, n.comp.units, RetailPark, hierarchy)

## Use pipes to create the new hierarchy column and then select the other columns we are interested in
rc_cent <- rc_cent %>% 
  mutate(hierarchy = dplyr::case_when(n.comp.units < 50 & RetailPark == "N" ~ "tertiary",
                                      (n.comp.units >= 50 & n.comp.units < 100) | RetailPark == "Y" ~ "secondary",
                                      n.comp.units >= 100 ~ "primary")) %>%
  select(rcID, rcName, n.units, n.comp.units, RetailPark, hierarchy)


# 3. Catchments (1) - Fixed-Ring Buffers ----------------------------------

## Extract a 1000m buffer for each retail centre
buffer1km <- st_buffer(rc_cent, 1000)

## Map the buffers
tm_shape(buffer1km) + ## Plot the buffers
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "black")

## Run this chunk to save the function to your environment
get_primary_buffer <- function(centroids) {
  
  ## Extracts centroids of 'primary' retail centres
  rc_primary <- filter(centroids, hierarchy == "primary")
  
  ## Constructs a 1000m buffer
  primary_buffer <- st_buffer(rc_primary, dist = 1000)
  
  ## Return the 1000m buffer
  return(primary_buffer)
}

## Get primary buffers for LCR Retail Centres
buffer1000 <- get_primary_buffer(rc_cent)

## Plot the primary buffers and all the centroids, to check only primary centres have catchments
tm_shape(buffer1000) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) +
  tm_dots()

## Run this chunk to save the function to your environment
get_buffer <- function(centroids, primary_dist = 5000, secondary_dist = 3000, tertiary_dist = 1500) {
  
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

## Run the function
hbuffer <- get_buffer(rc_cent, primary_dist = 5000, secondary_dist = 3000, tertiary_dist = 1500)

## Map the output
tm_shape(hbuffer)+ ## Plot the varying fixed-ring buffers
  tm_fill(col = "hierarchy", alpha = 0.5) + # Setting col to 'hierarchy' tells tmap to generate a different colour buffer for each value in the hierarchy column
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "black", alpha = 0.75)


# 4. Catchments (2) - Drive-Time Catchments -------------------------------

## Set API key
#set_key("insert-key-here")


# 4a. Building Simple Drive-Time Catchments -------------------------------

## Extract first retail centre
rc_a <- rc_cent[1, ]

## Extract the 10-minute driving catchment
iso_a <- isoline(rc_a, range = (10 * 60), range_type = "time", transport_mode = "car")

## Map the drive-time catchment for the first retail centre
tm_shape(iso_a) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(rc_a) +
  tm_dots() +
  tm_text("rcName", size = 0.5)

## Extract the 10-minute catchment for every retail centre in LCR
iso <- isoline(rc_cent, range = (10 * 60), range_type = "time", transport_mode = "car", aggregate = FALSE)

## Map the 10-minute drive-time catchments for LCR retail centres
tm_shape(iso) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) +
  tm_dots()

# 4b. Building Drive-Time Catchments for different dates/times ------------

## First set up the date & time you are interested in - e.g. Friday 5th Feb - 5pm 
friday5th <- as.POSIXct("2020-02-05 18:00:00 CET", tz = "Europe/Zurich")
## Build the catchment for Friday (rush hour)
friday_iso <- isoline(rc_a, range = (10 * 60), range_type = "time", transport_mode = "car",
                      datetime = friday5th)

## First set up the date & time you are interested in - e.g. Sunday 7th - 6am
sunday7th <- as.POSIXct("2020-02-07 06:00:00 CET", tz = "Europe/Zurich")
## Build the catchment for Friday (rush hour)
sunday_iso <- isoline(rc_a, range = (10 * 60), range_type = "time", transport_mode = "car",
                      datetime = sunday7th)

## Make the Maps
p1 <- tm_shape(friday_iso) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_shape(rc_a) +
  tm_dots()
p2 <- tm_shape(sunday_iso) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_shape(rc_a) +
  tm_dots()

## Plot them side-by-side
tmap_arrange(p1, p2, ncol = 2)


# 4c. Building Drive-Time Catchments that account for Hierarchy -----------

## Function to get drive-time catchments for the primary retail centres
get_primary_drive_time <- function(centroids, dist = 15, range_type = "time", transport_mode = "car") {
  
  ## Filter the centroids to extract the primary centres
  rc_primary <- filter(centroids, hierarchy == "primary")
  
  ## Build the drive-time catchment
  primary_drive_time <- isoline(rc_primary, range = (dist * 60), 
                                range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  
  ## Clean up the isoline - join on the retail centre information
  rc_primary <- rc_primary %>%
    as.data.frame() %>% 
    select(rcID, n.units, n.comp.units, hierarchy) %>%
    bind_cols(primary_drive_time) %>% ## Equivalent of cbind(), but for piping
    st_as_sf() ## Ensures final object is SF not dataframe
  return(rc_primary)
}

## Get catchments for primary centres
primary_iso <- get_primary_drive_time(rc_cent, dist = 15, range_type = "time", transport_mode =  "car")

## Map the primary drive-time catchments
tm_shape(primary_iso) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(rc_cent) +
  tm_dots()

## Run this chunk to save this function to your environment
get_drive_time <- function(centroids, primary_dist = 15, secondary_dist = 10, tertiary_dist = 5, 
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
    select(rcID, rcName, n.units, n.comp.units, hierarchy) %>%
    bind_cols(primary_drive_time)
  secondary <- rc_secondary %>%
    as.data.frame() %>%
    select(rcID, rcName, n.units, n.comp.units, hierarchy) %>%
    bind_cols(secondary_drive_time)
  tertiary <- rc_tertiary %>%
    as.data.frame() %>%
    select(rcID, rcName, n.units, n.comp.units, hierarchy) %>%
    bind_cols(tertiary_drive_time)
  
  ## Join catchments together
  isolines <- st_as_sf(rbind(primary, secondary, tertiary))
  return(isolines)
}

## Extract the hierarchical drive time catchments for LCR retail centres
iso_hierarchy <- get_drive_time(rc_cent, primary_dist = 15, secondary_dist = 10, tertiary_dist = 5, 
                                range_type = "time", transport_mode = "car")

## Map the output
tm_shape(iso_hierarchy) + ## Plot the hierarchical drive-time catchments 
  tm_fill(col = "hierarchy", alpha = 0.5) + ## Setting col = 'hierarchy' tells tmap to plot a different colour for each value of hierarchy
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots()


# 4d. Optional Exercise - Using Pipes with tmap  --------------------------

## Filter to catchments of interest
iso_hierarchy %>%
  filter(rcName== "Liverpool City") %>%
  tm_shape() + ## Notice how tm_shape can be left empty as you have piped directly from the iso_hierarchy object
  tm_fill(col = "orange", alpha = 0.5)


#  Writing out your files -------------------------------------------------

#st_write(rc_cent, "Data/Part2_Retail_Centres.gpkg")
