

## Load libraries
  
options(repos = c(CRAN = "https://cran.rstudio.com"))


install.packages("pacman")
library("pacman")

install.packages("markdown")
library("markdown")


## loading necessary libraries

library("ggh4x")
library("dplyr")
library("ggplot2")
library("tidyr")
library("sf")
library("sp")
library("terra")
library("tmap")
library("zoo")
library("tidyverse")
library("leaflet")
library("XML")
library("lubridate")
library("forcats")
library("httr")
library("hereR")
library("png")
library("gridExtra")
library("RColorBrewer")
library("purrr")
library("ggridges")
library("agricolae")



## Import Spatial Data
# Import spatial data. Geopackage was preprocessed using Quantum GIS Version 3.34.5.

## All Spatial Data is stored in a geopackage called basic_data. Basic_data consists of several layers such as the street network, surface type or housing footprint. The layers were preprocessed and cliped to the extent of the city outline of zurich using Quantum GIS Version 3.34.5


## Show layers in basic_data.gpkg
sf::st_layers("gis_files/basic_data.gpkg")


## Import street network from zurich, based on the swisstlm3d
streets <- sf::read_sf("gis_files/basic_data.gpkg", "street_network_z") |> 
  select(objektart, geom) |> 
  mutate(
    objektart = as.factor(objektart),
    width = as.numeric(substr(objektart, start = 1, stop = 1)),
  ) |> 
  na.omit()


## Import city border of zurich
outline <- sf::read_sf("gis_files/basic_data.gpkg", "city_outline")


## Import housing footprint of zurich
housing <- sf::read_sf("gis_files/basic_data.gpkg", "housing_footprint") |> 
  select(objektart, geom) |>
  mutate(
    objektart = as.factor(objektart)
  )


## Import surface type of zurich
surface <- sf::read_sf("gis_files/basic_data.gpkg", "surface_type") |> 
  select(art, geom) |> 
  mutate(
    art = as.factor(art)) |> 
  filter(art == "fliessendes Gewässer" | art == "stehendes Gewässer" | art == "Strasse, Weg" | art == "Verkehrsinsel") |> 
  na.omit()


## Import digital height model DHM25, a set of data representing the 3D form of the earth’s surface without vegetation and buildings
height <- terra::rast("gis_files/dhm25_zh.tif")



## 1. Generating alternative routes using hereR

# Creating alternative routes
## load all_routes_seg_tunnel_cor from global environment
load("rda_files/all_routes_seg_tunnel_cor.rda")


## Extract starting and endpoints from real world trajecotries
a_to_b <- all_routes_seg_tunnel_cor |> 
  group_by(segment_id_cor) |> 
  slice(c(1, n())) |> 
  ungroup()


## quick and dirty overview
tmap_mode("plot")
tm_shape(a_to_b) + 
  tm_dots(col = "gold", size = .25) +
  tm_layout(frame = FALSE)


## Get accsess to API from here, this API will later generate our alternative routes based on the start and endpoints of the deliveries
library("hereR")
set_key("qCKNngXWAlCW3GjOMYSldRSI_LwQ-O0PxOjPUuQPXgc")


## Create a dataframe with even rows -> ENDPOINTS
df_even <- a_to_b[seq(2, nrow(a_to_b), by = 2), ]
df_even <- df_even |> 
  filter(shift == "F1")

# df_even_raeubertochter <- filter(df_even, messenger == "raeubertochter")
# df_even_donner <- filter(df_even, messenger == "donner")

# if a single smaple is needed
#df_even <- filter(df_even, segment_id_cor == 57)


## Create a dataframe with odd rows -> STARTING POINTS
df_odd <- a_to_b[seq(1, nrow(a_to_b), by = 2), ]
df_odd <- df_odd |> 
  filter(shift == "F1")

# df_odd_raeubertochter <- filter(df_odd, messenger == "raeubertochter")
# df_odd_donner <- filter(df_odd, messenger == "donner")

# if a single smaple is needed
#df_odd <- filter(df_odd, segment_id_cor == 57)
testruns <- nrow(df_even)


alternates_final <- map(seq_len(testruns), function(x){
  alternative_lines = route(origin = df_odd[x,], destination = df_even[x,], transport_mode = "bicycle", traffic = FALSE, results = 7 , routing_mode = "short")
  Sys.sleep(1.2)
  segment_id = df_even$segment_id_cor[x]
  alternative_lines$segment_id = segment_id
  alternative_lines
})

save(alternates_final, file = "rda_files/alternates_final.rda")



df <- enframe(alternates_final)
df <-  st_as_sf(do.call(rbind, alternates_final))



## Transform alternative in spatial point dataframe
merged_dataset <- df |> 
  group_by(segment_id, rank) |> 
  summarise(geometry = sf::st_union(geometry))

alternative <- merged_dataset |> 
  st_transform(crs = 2056) |> 
  st_cast("POINT") |> 
  st_zm(drop = TRUE) |> 
  rename(segment_id = segment_id) |> 
  mutate(
    segment_id = as.factor(segment_id),
    rank = as.factor(rank)
  )


## drop height information, well add id back later via dhm25
alternative_final <- st_zm(alternative, drop = TRUE)

# save robject
save(alternative_final, file = "rda_files/alternative_final.rda")

