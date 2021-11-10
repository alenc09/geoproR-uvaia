#library----
library(raster)
library(dplyr)
library(here)
library(geobr)
library(sf)

# data----
read.csv(file = here("dados/ATLANTIC_AMPHIBIANS_sites.csv")) -> amphi_sites
read.csv(file = here("dados/ATLANTIC_AMPHIBIANS_species.csv"))->amphi_spp
raster(x = here("dados/pop_MA_1000_ployBR.tif"))-> pop_MA_1000_polyBR
read_biomes()%>%
  filter(name_biome == "Mata Atlântica") -> map_MA

## data transformation ----
sf::st_transform(map_MA, crs = 4326) ->map_MA_wgs84
sf::as_Spatial(map_MA_wgs84) ->map_MA_wgs84_sp

sf::st_transform(map_MA, crs = 5880) ->map_MA_polyBR #aqui é só para o sf da mata atlântica ficar na mesma projeção

amphi_sites %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 5880)-> anf_sites_points

anf_sites_points[map_MA_polyBR, ]-> anf_sites_MA
anf_sites_points[map_MA_polyBR,  , op = st_disjoint]-> anf_sites_outMA

plot(anf_sites_MA$geometry)


### buffer
buff_1km <- sf::st_buffer(x = anf_sites_MA, dist = 1000)
buff_2km <- sf::st_buffer(x = anf_sites_MA, dist = 2000)
buff_5km <- sf::st_buffer(x = anf_sites_MA, dist = 5000)

## data visualization ----
plot(pop_MA_1000_polyBR)

plot(map_MA_polyBR$geom, add = T) #para visualizar se o processamento acima funcinou

plot(anf_sites_points$geometry)

plot(buff_1km$geometry)

plot(buff_2km$geometry)

plot(buff_5km$geometry)

#####################################################################

#geoprocessing----
pop_MA_1000_polyBR[is.na(pop_MA_1000_polyBR)] <- 0

buff_1km %>% 
  dplyr::mutate(pop_1km = raster::extract(x = pop_MA_1000_polyBR,
                                          y = buff_1km,
                                          fun = sum,
                                          na.rm = FALSE)) -> buff_1km
glimpse(buff_1km)
as.factor(buff_1km$id) -> buff_1km$id
as.vector(buff_1km$pop_1km) ->buff_1km$pop_1km


buff_2km %>% 
  dplyr::mutate(pop_2km = raster::extract(x = pop_MA_1000_polyBR,
                                          y = buff_2km,
                                          fun = sum,
                                          na.rm = FALSE)) -> buff_2km
glimpse(buff_2km)
as.factor(buff_2km$id) -> buff_2km$id
as.vector(buff_2km$pop_2km) ->buff_2km$pop_2km

buff_5km %>% 
  dplyr::mutate(pop_5km = raster::extract(x = pop_MA_1000_polyBR,
                                          y = buff_5km,
                                          fun = sum,
                                          na.rm = FALSE)) -> buff_5km
glimpse(buff_5km)
as.factor(buff_5km$id) -> buff_5km$id
as.vector(buff_5km$pop_5km) ->buff_5km$pop_5km


#######################################################################
# plot raster de população resolução ~1km

df_pop_MA_1000 <- as.data.frame(pop_MA_1000_polyBR, xy=T, centroids=T)
library(ggplot2)
library(ggspatial)

ggplot() +
  geom_raster(data = df_pop_MA_1000, aes(x = x, y = y, fill = pop_MA_1000_ployBR)) +
  scale_fill_viridis_c(name="Tamanho populacional") + 
  ylim(7200000, 7600000) + xlim(5600000, 6000000) +
  labs(y="Latitude\n", x="\nLongitude", 
       title="Raster de tamanho populacional (resolução ~1 km)") +
  coord_sf(default_crs = sf::st_crs(5880))+
  theme_dark()+
  theme(panel.grid.major = element_line(color = "transparent"))

####################################################################

install.packages("viridis")
library(viridis)

ggplot() +
  geom_raster(data = df_pop_MA_1000, aes(x = x, y = y, fill = pop_MA_1000_ployBR)) +
  scale_fill_viridis(option = "D", name="Tamanho populacional") +
  ylim(7300000, 7500000) + xlim(5800000, 6000000) +
  geom_sf(buff_5km, mapping=aes()) +
  geom_sf(buff_2km, mapping=aes()) +
  geom_sf(buff_1km, mapping=aes()) +
  labs(y="Latitude\n", x="\nLongitude", 
       title="Buffers (~1 km; ~2 km; ~5 km)") +
  coord_sf(default_crs = sf::st_crs(5880))+
  theme(panel.grid.major = element_line(color = "transparent")) +
  annotation_scale(location = "br", width_hint = 0.5,
                 style= "bar",
                 pad_x = unit(0.8, "in"), pad_y = unit(0.4, "in")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.8, "in"),
                         style = north_arrow_fancy_orienteering 
                         (text_size = 8, line_col = "black",
                           fill = c("white", "white"),
                           text_col = "white"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) 


??annotation_scale
