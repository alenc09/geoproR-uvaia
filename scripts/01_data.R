# Fri Oct 29 16:58:34 2021 ------------------------------

#library----
library(raster)
library(dplyr)
library(here)
library(geobr)
library(sf)

# data----
raster(x = here("dados/bra_ppp_2020_UNadj_constrained.tif"))-> pop_br
read.csv(file = here("dados/ATLANTIC_AMPHIBIANS_sites.csv"), fileEncoding = "latin1") -> amphi_sites
read.csv(file = here("dados/ATLANTIC_AMPHIBIANS_species.csv"))->amphi_spp
read_biomes()%>%
  filter(name_biome == "Mata Atlântica") -> map_MA
raster(x = here("dados/pop_MA_1000_sirgas2000.tif"))-> pop_MA_1000_polyBR

## data transformation ----
sf::st_transform(map_MA, crs = 4326) ->map_MA_wgs84
sf::as_Spatial(map_MA_wgs84) ->map_MA_wgs84_sp

pop_br%>%
raster::crop(map_MA_wgs84_sp)->pop_br_crop #demora bastante pra rodar. Aqui eu cortei a janela para ficar só a parte da mata atlantica
raster::mask(x = pop_br_crop, mask = map_MA_wgs84_sp) ->pop_MA #demora bastante pra rodar. Aqui eu fiz uma máscara, para selecionar só os pixel da mata atlantica
raster::aggregate(x = pop_MA, fact = 10, fun = sum) ->pop_MA_1000 #demora bastante pra rodar. Aqui eu agreguei os pixels (100x100m no original) para 1000x1000m (a medidade de número de pessoas por km² é bastante comum e alivia a pressão no pc pra rodar as análises)

"+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" -> proj_sigar2000_polyBR
raster::projectRaster(pop_MA_1000, crs = proj_sigar2000_polyBR, method = "bilinear")-> pop_MA_1000_polyBR #mudei o sistemas de coordenadas para uma projetada (SIRGAS 2000/Brazil Polyconic)

sf::st_transform(map_MA, crs = 5880) ->map_MA_polyBR #aqui é só para o sf da mata atlântica ficar na mesma projeção

amphi_sites %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 5880)-> anf_sites_points

anf_sites_points[map_MA_polyBR, ]-> anf_sites_MA
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

## data export----
raster::writeRaster(pop_MA, 
                    filename = here::here("dados/pop_MA"),
                    format = "GTiff",
                    datatype = "INT2S",
                    options = c("COMPRESS=DEFLATE", "TFW=YES"),
                    progress = "text",
                    overwrite = TRUE)

raster::writeRaster(pop_MA_1000_polyBR, 
                    filename = here::here("dados/pop_MA_1000_ployBR"),
                    format = "GTiff",
                    datatype = "INT2S",
                    options = c("COMPRESS=DEFLATE", "TFW=YES"),
                    progress = "text",
                    overwrite = TRUE)
