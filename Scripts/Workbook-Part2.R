knitr::opts_chunk$set(warning = FALSE, message = FALSE)

library(sf)
library(dplyr)
library(tmap)
tmap_mode("view") ## Interactive Mapping

install.packages("FNN")
library(rgdal)
library(rgeos)
library(igraph)
library(FNN)

#setwd("CDRC-Retail-Catchment-Training")

## Save huff-tools.R functions to memory
source("Scripts/huff-tools.R")

## Read in the LCR Retail Centres (with Hierarchy from Part 1)
rc <- st_read("Data/Part2_Retail_Centres.gpkg")

## Read in the Liverpool City Region LSOAs
lsoa <- st_read("Data/LCR_LSOA.gpkg")

## Plot the LSOAs for Liverpool City Region 
tm_shape(lsoa) +
  tm_fill(alpha = 0.75) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.5)

## Read in Distances.csv
distances <- read.csv("Data/Distances.csv")

## Use head() to print the first few rows of any data frame or tabular dataset
head(distances)

## Joing rc and distances together, and then tidy up the object (data.frame, selecting columns)
huff_input <- rc %>%
  inner_join(distances) %>%
  as.data.frame() %>%
  select(rcID, rcName,n.comp.units, RetailPark, hierarchy, lsoa11cd, distance)

## Joing rc and distances together, and then tidy up the object (data.frame, selecting columns) NO PIPES
# huff_input <- inner_join(rc, distances)
# huff_input <- as.data.frame(huff_input)
# huff_input <- select(huff_input, rcID, rcName, n.comp.units, hierarchy, lsoa11cd, distance)

## use head() to print the first few rows of any data frame or tabular dataset
head(huff_input)

## Create attractiveness score column and modify based on whether Retail Centre is in a Retail Park or not
huff_input <- huff_input %>%
  mutate(attr_score = n.comp.units) %>%
  mutate(attr_score, ifelse(RetailPark == "N", attr_score,
                            ifelse(RetailPark == "Y", attr_score * 2, 0))) %>%
  select(-c(attr_score)) %>%
  rename(attr_score = 8)

## Create column called beta, where the value = 2 for each Retail Centre
huff_input$beta <- 2

## Create column called alpha, where the value = 1 for each Retail Centre
huff_input$alpha <- 1

## head() displays the first few rows of any data frame or tabular object
head(huff_input)

## Select the columns we need for the Huff Model
huff_default <- huff_input %>%
  select(rcName, attr_score, lsoa11cd, distance, alpha, beta)

## Fit the huff model, setting alpha to 1 and beta to 2 - these are the default parameters
huff_probs <- huff_basic(destinations_name = huff_default$rcName,
                         destinations_attractiveness = huff_default$attr_score,
                         origins_name = huff_default$lsoa11cd,
                         distance = huff_default$distance,
                         alpha = huff_default$alpha,
                         beta = huff_default$beta)

## head() displays the first few rows of any data frame or tabular object
head(huff_probs)

## Extract the highest huff probability in each LSOA
top_probs <- select_by_probs(huff_probs, 1)

## head displays the top few rows of a dataframe or any tabular object
head(top_probs)

## Remove LSOAs with huff probs < 60%
top_probs_60 <- filter(top_probs, huff_probability >= 0.6)

top_probs_40 <- filter(top_probs, huff_probability >= 0.4)

## Create beta parameters, using mutate() and case_when()
huff_input <- huff_input %>%
  dplyr::mutate(beta = dplyr::case_when(hierarchy == "primary" ~ 1.9,
                                          hierarchy == "secondary" ~ 2.0,
                                            hierarchy == "tertiary" ~ 2.1))

## Create beta parameters, using mutate() and case_when() NO PIPES
# huff_input <- mutate(huff_input, beta = case_when(hierarchy == "primary" ~ 1.9,
#                                           hierarchy == "secondary" ~ 2.0,
#                                             hierarchy == "tertiary" ~ 2.1) )

## use head() to print the first few rows of any data frame or tabular dataset
head(huff_input)

## Fit the huff model, setting alpha to 1 and beta the the beta column, containing the values we set earlier
hierarchy_huff_probs <- huff_basic(destinations_name = huff_input$rcName,
                         destinations_attractiveness = huff_input$attr_score,
                         origins_name = huff_input$lsoa11cd,
                         distance = huff_input$distance,
                         alpha = huff_input$alpha,
                         beta = huff_input$beta)

## Extract the highest huff probability in each LSOA
hierarchy_top_probs <- select_by_probs(hierarchy_huff_probs, 1)

## Remove those with < 60% huff probability
hierarchy_top_probs_60 <- filter(hierarchy_top_probs, huff_probability >= 0.6)
## Remove those with < 40% huff probability
hierarchy_top_probs_40 <- filter(hierarchy_top_probs, huff_probability >= 0.4)

# head() displays the first few rows of a data frame or tabular object
head(hierarchy_top_probs_60)

## Tidy up top_probs 60 & 40
top_probs_60 <- top_probs_60 %>%
  rename(lsoa11cd = origins_name, rcName = destinations_name) %>%
  select(lsoa11cd, rcName, huff_probability)
top_probs_40 <- top_probs_40 %>%
  rename(lsoa11cd = origins_name, rcName = destinations_name) %>%
  select(lsoa11cd, rcName, huff_probability)

## Tidy up the output of the huff model using different beta values for hierarchy PIPED
hierarchy_top_probs_60 <-hierarchy_top_probs_60%>%
  rename(lsoa11cd = origins_name, rcName = destinations_name) %>%
  select(lsoa11cd, rcName, huff_probability)
hierarchy_top_probs_40 <-hierarchy_top_probs_40%>%
  rename(lsoa11cd = origins_name, rcName = destinations_name) %>%
  select(lsoa11cd, rcName, huff_probability)

## Merge the results of the huff model with default parameters onto the LSOAs - 40 & 60%
lcr_huff_60 <- merge(lsoa, top_probs_60, by = "lsoa11cd")
lcr_huff_40 <- merge(lsoa, top_probs_40, by = "lsoa11cd")

## Merge on the results of thehuff model accounting for hierarchy with beta values onto the LSOAs - 40 & 60%
lcr_hierarchy_huff_60 <- merge(lsoa, hierarchy_top_probs_60, by = "lsoa11cd")
lcr_hierarchy_huff_40 <- merge(lsoa, hierarchy_top_probs_40, by = "lsoa11cd")

## Huff probabilities (default parameters)
head(lcr_huff_60)

## Huff probabilities (beta values set to reflect hierarchy)
head(lcr_hierarchy_huff_60)

## Map the allocation of LSOAs to Retail Centres - notice the additional arguments in tm_layout() to move the legend outside the map frame, and tm_borders() to show LSOA boundaries clearly
## Plot the centroids over these too
tm_shape(lcr_huff_60) +
  tm_fill(col = "rcName", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right") +
  tm_shape(rc) +
  tm_dots(col = "orange", size = 0.05) +
  tm_text("rcName")

## Map the allocation of LSOAs to Retail Centres - notice the additional arguments in tm_layout() to move the legend outside the map frame, and tm_borders() to show LSOA boundaries clearly
tm_shape(lcr_hierarchy_huff_60) +
  tm_fill(col = "rcName", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right") +
  tm_shape(rc) +
  tm_dots(col = "orange", size = 0.05) +
  tm_text("rcName")

## Extract LSOAs and catchment for the Huyton Retail Centre
huyton <- filter(lcr_huff_60, rcName == "Huyton")

## Extract the Huyton Retail Centre Centroid
huyton_rc <- filter(rc, rcName == "Huyton")

## Map the Huyton Retail Centre and Huff Catchment
tm_shape(huyton) +
  tm_fill(col = "orange", alpha = 0.5) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_shape(huyton_rc) +
  tm_dots(col = "orange", size = 0.25) +
  tm_text("rcName", size = 0.75)

## Extract Huff catchments for each Retail Centre
catchments <- lcr_huff_60 %>%
  group_by(rcName) %>%
  summarise(n.lsoa = n())

## head() displays the first few rows of any dataframe or tabular object
head(catchments)

## Map the Huff Catchments for the Retail Centres and overlay Retail Centres
tm_shape(catchments) +
  tm_fill(col = "rcName", alpha = 0.4) +
  tm_borders(col = "black", lwd = 0.25, alpha = 0.2) +
  tm_shape(rc) +
  tm_dots(size = 0.015, alpha = 0.1, col = "orange") +
  tm_text("rcName", size = 0.75)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")

## Write out your catchments
# st_write(catchments, "Data/LCR_RC_Huff_Catchments.gpkg")
