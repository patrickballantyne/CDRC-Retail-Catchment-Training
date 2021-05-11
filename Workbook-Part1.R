knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(knitr)
purl("Workbook-Part1.Rmd", documentation = 0)

library(sf)
library(dplyr)
library(tmap)
#install.packages("data.table")
library(data.table)
library(hereR)

#setwd("CDRC-Retail-Catchment-Training")

## Read in the Retail Centre Boundaries 
rc <- st_read("Data/LCR_Retail_Centres_2018.gpkg")

## Print the first ten retail centres
rc

## Setup plotting
tmap_mode("view")

## Map Retail Centre Polygons for LCR
tm_shape(rc) +
  tm_fill(col = "orange") +
  tm_text("rcName", size = 0.75)

## Extract centroids
rc_cent <- st_centroid(rc)

## Map the centroids - note: tm_dots() is used as the object rc_cent contains point data (Retail Centre centroids)
tm_shape(rc_cent) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)

## Use mutate and case_when to create the new column - notice how ~ assigns the new values based on the condition
rc_cent_nopipes <-  mutate(rc_cent, 
   hierarchy = dplyr::case_when(n.comp.units < 50 & RetailPark == "N" ~ "tertiary",
                              (n.comp.units >= 50 & n.comp.units < 100) | RetailPark == "Y" ~ "secondary",
                                n.comp.units >= 100 ~ "primary")) 

## Use select to extract the id, n.units, n.comp.units and hierarchy columns
rc_cent_nopipes <- select(rc_cent_nopipes, rcID, rcName, n.units, n.comp.units, RetailPark, hierarchy)

## Sort by hierarchy
rc_cent_nopipes <- arrange(rc_cent_nopipes, hierarchy)

## Use pipes to create the new hierarchy column and then select the other columns we are interested in
rc_cent <- rc_cent %>% 
  mutate(hierarchy = dplyr::case_when(n.comp.units < 50 & RetailPark == "N" ~ "tertiary",
             (n.comp.units >= 50 & n.comp.units < 100) | RetailPark == "Y" ~ "secondary",
              n.comp.units >= 100 ~ "primary")) %>%
  select(rcID, rcName, n.units, n.comp.units, RetailPark, hierarchy) %>%
  arrange(hierarchy)

## Without pipes
rc_cent_nopipes

## With pipes
rc_cent

## View rc_cent
View(rc_cent)

## Extract a 1500m buffer for each Retail Centre
buffer1.5km <- st_buffer(rc_cent, 1500)

## Map the buffers
tm_shape(buffer1.5km) + ## Plot the buffers
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)

## Run this chunk to save the function to your environment
get_primary_buffer <- function(centroids) {
  
  ## Extracts centroids of 'primary' Retail Centres
  rc_primary <- filter(centroids, hierarchy == "primary")
  
  ## Constructs a 5000m buffer
  primary_buffer <- st_buffer(rc_primary, dist = 5000)
  
  ## Return the 5000m buffer
  return(primary_buffer)
}

## Get primary buffers for LCR Retail Centres
pbuffer <- get_primary_buffer(rc_cent)

## Plot the primary buffers and all the centroids, to check only primary centres have catchments
tm_shape(pbuffer) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)

## Run this chunk to save the function to your environment
get_buffer <- function(centroids, primary_dist = 5000, secondary_dist = 3000, tertiary_dist = 1500) {
  
  ## Split up the Retail Centres based on hierarchy
  rc_primary <- filter(centroids, hierarchy == "primary")
  rc_secondary <- filter(centroids, hierarchy == "secondary")
  rc_tertiary <- filter(centroids, hierarchy == "tertiary")
  
  ## Run the buffer for the different Retail Centre hierarchies separately
  primary_buffer <- st_buffer(rc_primary, dist = primary_dist)
  secondary_buffer <- st_buffer(rc_secondary, dist = secondary_dist)
  tertiary_buffer <- st_buffer(rc_tertiary, dist = tertiary_dist)
  
  ## Join together
  buffer <- rbind(primary_buffer, secondary_buffer, tertiary_buffer)
  return(buffer) ## Return
}

## Run the function
hbuffer <- get_buffer(rc_cent, primary_dist = 5000, secondary_dist = 3000, tertiary_dist = 1500)

tm_shape(hbuffer)+ ## Plot the varying fixed-ring buffers
  tm_fill(col = "hierarchy", palette = c("yellow", "orange", "red"), alpha = 0.5) + # Setting col to 'hierarchy' tells tmap to generate a different colour buffer for each value in the hierarchy column
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "orange", alpha = 0.75) +
  tm_text("rcName", size = 0.75)

## Set API key
#set_key("insert-key-here")

## Set API key
set_key("Hy2XaUNC0aeDW7bRxOTIyqPwQuD_P3GQsUL-YaqVuR8")

## Extract Liverpool City Retail Centre - the first in our dataset
rc_a <- rc_cent[1, ]

## Extract the 10-minute driving catchment
iso_a <- isoline(rc_a, range = (15 * 60), range_type = "time", transport_mode = "car")

## Map the drive-time catchment for the first Retail Centre
tm_shape(iso_a) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_shape(rc_a) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)

## Extract the 5-minute catchment for every Retail Centre in LCR
iso <- isoline(rc_cent, range = (5 * 60), range_type = "time", transport_mode = "car", aggregate = FALSE)

## Map the 5-minute drive-time catchments for LCR Retail Centres
tm_shape(iso) +
  tm_fill(col = "orange", alpha = 0.3) +
  tm_shape(rc_cent) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)

## First set up the date & time you are interested in - e.g. Friday 7th May - 5pm 
friday7th <- as.POSIXct("2021-05-07 17:30:00 BST", tz = "Europe/London")
## Build the catchment for Friday (rush hour)
friday_iso <- isoline(rc_a, range = (10 * 60), range_type = "time", transport_mode = "car",
                      datetime = friday7th)

## First set up the date & time you are interested in - e.g. Sunday 9th - 5am
sunday9th <- as.POSIXct("2021-05-09 05:00:00 BST", tz = "Europe/London")
## Build the catchment for Friday (rush hour)
sunday_iso <- isoline(rc_a, range = (10 * 60), range_type = "time", transport_mode = "car",
                      datetime = sunday9th)

## Make the Maps
p1 <- tm_shape(friday_iso) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_shape(rc_a) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)
p2 <- tm_shape(sunday_iso) +
  tm_fill(col = "orange", alpha = 0.75) +
  tm_shape(rc_a) +
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)


## Plot them side-by-side
tmap_arrange(p1, p2, ncol = 2)

## Function to get drive-time catchments for the primary Retail Centres
get_primary_drive_time <- function(centroids, dist = 15, range_type = "time", transport_mode = "car") {
  
  ## Filter the centroids to extract the primary centres
  rc_primary <- filter(centroids, hierarchy == "primary")
  
  ## Build the drive-time catchment
  primary_drive_time <- isoline(rc_primary, range = (dist * 60), 
                                range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  
  ## Clean up the isoline - join on the Retail Centre information
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
  tm_dots(col = "orange") +
  tm_text("rcName", size = 0.75)

## Run this chunk to save this function to your environment
get_drive_time <- function(centroids, primary_dist = 15, secondary_dist = 10, tertiary_dist = 5, 
                           range_type = "time", transport_mode = "car") {
  
  ## Split up the Retail Centres based on hierarchy
  rc_primary <- filter(centroids, hierarchy == "primary")
  rc_secondary <- filter(centroids, hierarchy == "secondary")
  rc_tertiary <- filter(centroids, hierarchy == "tertiary")
  
  ## Delineate the isolines for the different Retail Centre hierarchies separately
  primary_drive_time <- isoline(rc_primary, range = (primary_dist * 60),
                                range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  secondary_drive_time <- isoline(rc_secondary, range = (secondary_dist * 60),
                                  range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  tertiary_drive_time <- isoline(rc_tertiary, range = (tertiary_dist * 60),
                                 range_type = range_type, transport_mode = transport_mode, aggregate = FALSE)
  
  ## Join the Retail Centre info onto each set of catchments
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

## Extract the hierarchical drive time catchments for LCR Retail Centres
iso_hierarchy <- get_drive_time(rc_cent, primary_dist = 15, secondary_dist = 10, tertiary_dist = 5, 
                                range_type = "time", transport_mode = "car")

tm_shape(iso_hierarchy) + ## Plot the hierarchical drive-time catchments 
  tm_fill(col = "hierarchy", palette = c("yellow", "orange", "red"), alpha = 0.5) + ## Setting col = 'hierarchy' tells tmap to plot a different colour for each value of hierarchy
  tm_shape(rc_cent) + ## Overlay the centroids
  tm_dots(col = "orange" ) +
  tm_text("rcName", size = 0.75)

## Filter to catchments of interest
iso_hierarchy %>%
  filter(rcName== "Liverpool City") %>%
  tm_shape() + ## Notice how tm_shape can be left empty as you have piped directly from the iso_hierarchy object
  tm_fill(col = "orange", alpha = 0.5) +
  tm_text("rcName", size = 0.75)

## Write out to Data folder
# st_write(rc_cent, "Data/Part2_Retail_Centres.gpkg")
