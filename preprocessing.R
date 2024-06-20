# Creating alternative routes

load("C:/Users/patri/Desktop/Semesterproject/semesterproject/all_routes_seg_tunnel_cor.rda")




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
set_key("PARpVmlSSQAeuGjMhIdg94LMi9s1ha4UShazq0j8sAo")


############### ATTENTION: We get a lot of warning due to our limited access to this API. We're barely able to get 1 alternative route per delivery for 1 shift per messenger. We have access to almost 60 shifts per messenger, but can not generate an alternative for one shift per messenger, major bummer! We try to run the function (route) twice, ones for raeubertochter, ones for donner so at least for day one, all routes get an alternative

## Create a dataframe with even rows -> ENDPOINTS
df_even <- a_to_b[seq(2, nrow(a_to_b), by = 2), ]
# df_even_raeubertochter <- filter(df_even, messenger == "raeubertochter")
# df_even_donner <- filter(df_even, messenger == "donner")

# if a single smaple is needed
#df_even <- filter(df_even, segment_id_cor == 57)


## Create a dataframe with odd rows -> STARTING POINTS
df_odd <- a_to_b[seq(1, nrow(a_to_b), by = 2), ]
# df_odd_raeubertochter <- filter(df_odd, messenger == "raeubertochter")
# df_odd_donner <- filter(df_odd, messenger == "donner")

# if a single smaple is needed
#df_odd <- filter(df_odd, segment_id_cor == 57)
testruns <- nrow(df_even)


alternates <- map(seq_len(testruns), function(x){
  alternative_lines = route(origin = df_odd[x,], destination = df_even[x,], transport_mode = "bicycle", traffic = FALSE, results = 7 , routing_mode = "short")
  Sys.sleep(1.2)
  segment_id = df_even$segment_id_cor[x]
  alternative_lines$segment_id = segment_id
  alternative_lines
})

?route
 
# alternative_lines <- route(origin = df_odd[1,], destination = df_even[1,], transport_mode = "bicycle", traffic = FALSE, results = 7 , routing_mode = "short")



## create an alternative route for every single delivery from raeubertochter
# alternative_lines_donner <- route(origin = df_odd_donner, destination = df_even_donner, transport_mode = "bicycle", traffic = FALSE, results = 1 , routing_mode = "short")
# 
# ## create an alternative route for every single delivery from raeubertochter
# alternative_lines_raeubertochter <- route(origin = df_odd_raeubertochter, destination = df_even_raeubertochter, transport_mode = "bicycle", traffic = FALSE, results = 1 , routing_mode = "short")


############### We still get warning because we sent too many request in a given time frame. We will compute the alternatives with a little pause in between so we can get alternatives for all routes on day one.


## Combine datasets back together
#alternative <- rbind(alternative_lines_donner, alternative_lines_raeubertochter)


## Transform alternative in spatial point dataframe
alternates2  <- tidyr::unnest(alternates, lines)
df <- enframe(alternates)
df <-  st_as_sf(do.call(rbind, alternates))


merged_dataset <- df %>%
  group_by(segment_id, rank) %>%
  summarise(geometry = sf::st_union(geometry))

alternative <- merged_dataset |> 
  st_transform(crs = 2056) |> 
  st_cast("POINT") |> 
  st_zm(drop = TRUE) |> 
  rename(segment_id = segment_id)


## drop height information, well add id back later via dhm25
alternative <- st_zm(alternative, drop = TRUE)



save(alternative, file = "alternative.rda")

