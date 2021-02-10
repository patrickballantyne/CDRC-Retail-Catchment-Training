## CDRC Retail Catchment Training - Part 2


# Setup -------------------------------------------------------------------

## Libraries - you should have these already
library(sf)
library(dplyr)
library(tmap)

## New Libraries - you may not have these installed
# install.packages("rgdal")
library(rgdal)
library(rgeos)
library(igraph)
library(FNN)
tmap_mode("view")

## We need to get functions from the Huff Tools.R file
source("Data/huff-tools.R")

# 1. Data (& Preprocessing) -----------------------------------------------

## Read in the dataset from the end of practical 1
rc <- st_read("Data/Part2_Retail_Centres.gpkg")

## Read in the Distance.csv file, containing road distances from each retail centre to each LCR LSOA
distances <- read.csv("Data/Distances.csv")

## Join the distances onto the retail centre data - each retail centre has a distance to each LSOA in the dataset
db <- inner_join(rc, distances)

## We are only interested in a select few columns now, drop the ones we don't want:
huff_input <- db %>%
  as.data.frame() %>%
  select(rcID, n.comp.units, hierarchy, lsoa11cd, distance)


# 2. Building a Huff Model ------------------------------------------------

## First we need to assign the beta values, which are based on the attractiveness rank - in this case we are going to use
## the hierarchies created in the last practical to assign three different beta values, one for each of the retail centre
## hierarchy categories (primary, secondary, tertiary)

## Again, we are going to use case_when statements to assign these values, the beta values can be modified
huff_input <- huff_input %>%
  dplyr::mutate(beta = dplyr::case_when(hierarchy == "primary" ~ 1.2,
                                          hierarchy == "secondary" ~ 1.4,
                                            hierarchy == "tertiary" ~ 1.6))

## Run the huff model
probs <- huff_basic(huff_input$rcID,
                    huff_input$n.comp.units,
                    huff_input$lsoa11cd, 
                    huff_input$distance,
                    huff_input$beta,
                    alpha = 1)

## Extract the highest Huff probabilities for each LSOA
sele_probs <- select_by_probs(probs, 1)

## Tidy up
sele_probs <- sele_probs %>%
  rename(lsoa11cd = origins_name, rcID = destinations_name)


# 3. Mapping the Output ---------------------------------------------------

## First we need to read in an LSOA shapefile for LCR to join the data onto
lcr <- st_read("Data/LCR_lsoa.gpkg")

## Merge
lcr_huff <- merge(lcr, sele_probs, by = "lsoa11cd", all.y = TRUE)

## Now we have an sf object of LSOAs for Liverpool City Region, with each LSOA assigned to the retail centre with the highest
## huff probability

## We can plot the retail centre names to see which LSOAs have been assigned to which Retail Centres
tm_shape(lcr_huff) +
  tm_fill(col = "rcID")

## We can also filter to a certain retail centre to get it's catchment
lcr_huff %>%
  filter(rcID == "RC_EW_3077") %>%
  tm_shape() +
  tm_fill(col = "orange", alpha = 0.5)

# 4. Extracting Huff Catchments -------------------------------------------

## To create the huff catchments all we need to do is use the group_by and summarise functions
catchments <- lcr_huff %>%
  group_by(rcID) %>%
  summarise()

## Now we can map the huff catchments for each retail centre
tm_shape(catchments) +
  tm_fill(col = "rcID", alpha = 0.4)


