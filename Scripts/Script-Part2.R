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
  select(rcID, n.units, hierarchy, lsoa11cd, distance)

## Joing rc and distances together, and then tidy up the object (data.frame, selecting columns) NO PIPES
# huff_input <- inner_join(rc, distances)
# huff_input <- as.data.frame(huff_input)
# huff_input <- select(huff_input, rcID, n.units, hierarchy, lsoa11cd, distance)

# 2. Setting up the Huff Model ------------------------------------------------

## First we need to assign the beta values, which are based on the attractiveness rank - in this case we are going to use
## the hierarchies created in the last practical to assign three different beta values, one for each of the retail centre
## hierarchy categories (primary, secondary, tertiary)


# 2b. Beta Exponent -------------------------------------------------------

## Again, we are going to use case_when statements to assign these values, the beta values can be modified
huff_input <- huff_input %>%
  dplyr::mutate(beta = dplyr::case_when(hierarchy == "primary" ~ 1.2,
                                          hierarchy == "secondary" ~ 1.4,
                                            hierarchy == "tertiary" ~ 1.6))

## Create beta parameters, using mutate() and case_when() NO PIPES
# huff_input <- mutate(huff_input, beta = case_when(hierarchy == "primary" ~ 1.4,
#                                           hierarchy == "secondary" ~ 1.6,
#                                             hierarchy == "tertiary" ~ 1.8) )


# 2c. Alpha Exponent ------------------------------------------------------

## Create column called alpha, where the value = 1 for each retail centre
huff_input$alpha <- 1


# 3. Running the Huff Model -----------------------------------------------

## Run the huff model
huff_probs <- huff_basic(destinations_name = huff_input$rcID,
                         destinations_attractiveness = huff_input$n.units,
                         origins_name = huff_input$lsoa11cd,
                         distance = huff_input$distance,
                         alpha = huff_input$alpha,
                         beta = huff_input$beta)

## Extract the highest Huff probabilities for each LSOA
top_probs <- select_by_probs(huff_probs, 1)


# 4. Mapping the Output ---------------------------------------------------

## Tidy up PIPED
top_probs <- top_probs %>%
  rename(lsoa11cd = origins_name, rcID = destinations_name) %>%
  select(lsoa11cd, rcID, huff_probability)

## Tidy up top_probs NO PIPES
# top_probs <- rename(top_probs, lsoa11cd = origins_name, rcID = destinations_name)
# top_probs <- select(top_probs, lsoa11cd, rcID, huff_probability)

## Merge
lcr_huff <- merge(lsoa, top_probs, by = "lsoa11cd", all.y = TRUE)

## Now we have an sf object of LSOAs for Liverpool City Region, with each LSOA assigned to the retail centre with the highest
## huff probability

## Map the allocation of LSOAs to retail centres - notice the additional arguments in tm_layout() to move the legend outside the map frame, and tm_borders() to show LSOA boundaries clearly
tm_shape(lcr_huff) +
  tm_fill(col = "rcID", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")


# 5. Extracting Huff Catchments -------------------------------------------

## Extract LSOAs and catchment for the St Helens Retail Centre
sthelens <- filter(lcr_huff, rcID == "RC_EW_3102")

## Extract the St Helen's Retail Centre Centroid
sthelens_rc <- filter(rc, rcID == "RC_EW_3102")

## Map the St Helens Retail Centre and Huff Catchment
tm_shape(sthelens) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_shape(sthelens_rc) +
  tm_dots(size = 0.25, col = "red")

## Extract Huff catchments for each Retail Centre
catchments <- lcr_huff %>%
  group_by(rcID) %>%
  summarise(n.lsoa = n())

## Map the Huff Catchments for the Retail Centres
tm_shape(catchments) +
  tm_fill(col = "rcID", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2)

## Write out your catchments
# st_write(catchments, "Data/LCR_RC_Huff_Catchments.gpkg")

