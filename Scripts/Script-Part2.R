## CDRC Retail Catchment Training - Part 2


# Setup -------------------------------------------------------------------

## Libraries - you should have these already
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view") ## Interactive Mapping

## New Libraries - you may not have these installed
# install.packages("rgdal")
library(rgdal)
library(rgeos)
library(igraph)
library(FNN)


## We need to get functions from the Huff Tools.R file
source("Data/huff-tools.R")

# 1. Data (& Preprocessing) -----------------------------------------------

## Read in the dataset from the end of practical 1
rc <- st_read("Data/Part2_Retail_Centres.gpkg")

## Read in the Liverpool City Region LSOAs
lsoa <- st_read("Data/LCR_LSOA.gpkg")

## Read in the Distance.csv file, containing road distances from each retail centre to each LCR LSOA
distances <- read.csv("Data/Distances.csv")

## Joing rc and distances together, and then tidy up the object (data.frame, selecting columns)
huff_input <- rc %>%
  inner_join(distances) %>%
  as.data.frame() %>%
  select(rcID, n.comp.units, hierarchy, lsoa11cd, distance)

## Joing rc and distances together, and then tidy up the object (data.frame, selecting columns) NO PIPES
# huff_input <- inner_join(rc, distances)
# huff_input <- as.data.frame(huff_input)
# huff_input <- select(huff_input, rcID, n.comp.units, hierarchy, lsoa11cd, distance)

# 2. Setting up the Huff Model ------------------------------------------------

## Create beta parameters, using mutate() and case_when()
huff_input <- huff_input %>%
  dplyr::mutate(beta = dplyr::case_when(hierarchy == "primary" ~ 1.8,
                                        hierarchy == "secondary" ~ 1.9,
                                        hierarchy == "tertiary" ~ 2.0))

## Create beta parameters, using mutate() and case_when() NO PIPES
# huff_input <- mutate(huff_input, beta = case_when(hierarchy == "primary" ~ 1.4,
#                                           hierarchy == "secondary" ~ 1.6,
#                                             hierarchy == "tertiary" ~ 1.8) )

## Create column called alpha, where the value = 1 for each retail centre
huff_input$alpha <- 1


# 3. Running the Huff Model - Calculating Huff Probabilities --------------


# 3a. Huff Model (Default Parameters) -------------------------------------

## Fit the huff model, setting alpha to 1 and beta to 2 - these are the default parameters
huff_probs <- huff_basic(destinations_name = huff_input$rcName,
                         destinations_attractiveness = huff_input$n.comp.units,
                         origins_name = huff_input$lsoa11cd,
                         distance = huff_input$distance,
                         alpha = 1,
                         beta = 2)

## Extract the highest huff probability in each LSOA
top_probs <- select_by_probs(huff_probs, 1)


# 3b. Huff Model (specification of *alpha* and *beta*) --------------------

## Fit the huff model, setting alpha to 1 and beta the the beta column, containing the values we set earlier
hierarchy_huff_probs <- huff_basic(destinations_name = huff_input$rcName,
                                   destinations_attractiveness = huff_input$n.comp.units,
                                   origins_name = huff_input$lsoa11cd,
                                   distance = huff_input$distance,
                                   alpha = 1,
                                   beta = huff_input$beta)

## Extract the highest huff probability in each LSOA
hierarchy_top_probs <- select_by_probs(hierarchy_huff_probs, 1)


# 4. Mapping the Output ---------------------------------------------------

## Tidy up top_probs PIPED
top_probs <-top_probs %>%
  rename(lsoa11cd = origins_name, rcName = destinations_name) %>%
  select(lsoa11cd, rcName, huff_probability)

## Tidy up top_probs NO PIPES
# top_probs <- rename(top_probs, lsoa11cd = origins_name, rcName = destinations_name)
# top_probs <- select(top_probs, lsoa11cd, rcName, huff_probability)

## Tidy up the output of the huff model using different beta values for hierarchy PIPED
hierarchy_top_probs <-hierarchy_top_probs %>%
  rename(lsoa11cd = origins_name, rcName = destinations_name) %>%
  select(lsoa11cd, rcName, huff_probability)

## Tidy up hierarchy_top_probs NO PIPES
# top_probs <- rename(top_probs, lsoa11cd = origins_name, rcName = destinations_name)
# top_probs <- select(top_probs, lsoa11cd, rcName, huff_probability)

## Merge the results of the huff model with default parameters onto the LSOAs
lcr_huff <- merge(lsoa, top_probs, by = "lsoa11cd")

## Merge on the results of thehuff model accounting for hierarchy with beta values onto the LSOAs
lcr_hierarchy_huff <- merge(lsoa, hierarchy_top_probs, by = "lsoa11cd")

## Map the allocation of LSOAs to retail centres - notice the additional arguments in tm_layout() to move the legend outside the map frame, and tm_borders() to show LSOA boundaries clearly
tm_shape(lcr_huff) +
  tm_fill(col = "rcName", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")

## Map the allocation of LSOAs to retail centres - notice the additional arguments in tm_layout() to move the legend outside the map frame, and tm_borders() to show LSOA boundaries clearly
tm_shape(lcr_hierarchy_huff) +
  tm_fill(col = "rcName", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")


# 5. Extracting Huff Catchments -------------------------------------------

## Extract LSOAs and catchment for the St Helens Retail Centre
sthelens <- filter(lcr_hierarchy_huff, rcName == "St Helens")

## Extract the St Helen's Retail Centre Centroid
sthelens_rc <- filter(rc, rcName == "St Helens")

## Map the St Helens Retail Centre and Huff Catchment
tm_shape(sthelens) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_shape(sthelens_rc) +
  tm_dots(size = 0.1, col = "red")

## Extract Huff catchments for each Retail Centre
catchments <- lcr_hierarchy_huff %>%
  group_by(rcName) %>%
  summarise(n.lsoa = n())

## Map the Huff Catchments for the Retail Centres
tm_shape(catchments) +
  tm_fill(col = "rcName", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2)

## Write out your catchments
# st_write(catchments, "Data/LCR_RC_Huff_Catchments.gpkg")


