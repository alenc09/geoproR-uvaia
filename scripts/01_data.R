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

## data transformation ----
sf::st_transform(map_MA, crs = 4326) ->map_MA_wgs84
sf::as_Spatial(map_MA_wgs84) ->map_MA_wgs84_sp

pop_br%>%
raster::crop(map_MA_wgs84_sp)->pop_br_crop

raster::mask(x = pop_br_crop, mask = map_MA_wgs84_sp) ->pop_MA
raster::aggregate(x = pop_MA, fact = 10, fun = sum) ->pop_MA_1000
"+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" -> proj_sigar2000_polyBR
raster::projectRaster(pop_MA_1000, crs = proj_sigar2000_polyBR, method = "bilinear")-> pop_MA_1000_sirgas2000

## data export----
raster::writeRaster(pop_MA, 
                    filename = here::here("dados/pop_MA"),
                    format = "GTiff",
                    datatype = "INT2S",
                    options = c("COMPRESS=DEFLATE", "TFW=YES"),
                    progress = "text",
                    overwrite = TRUE)
