# Wed Nov  3 10:04:08 2021 ------------------------------
# script para geoprocessamento e análise dos dados

#library----
library(sf)
library(raster)
library(here)
library(dplyr)
library(tidyr)
library(purrr)

#data----
raster(here("dados/pop_MA_1000_ployBR.tif"))-> pop_MA_1000_polyBR
glimpse(buff_1km)
glimpse(buff_2km)
glimpse(buff_5km)
read.csv(file = here("dados/ATLANTIC_AMPHIBIANS_species.csv") -> amphi_spp

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

#data transformation----
amphi_spp$species<- as.factor(amphi_spp$species)
amphi_spp$id <- as.factor(amphi_spp$id)

amphi_spp%>%
  anti_join(y = select(anf_sites_outMA, id))%>%
  drop_na(data =., any_of("individuals"))%>%
  group_by(species)%>%
  dplyr::summarise(count = n())%>%
  filter(count >= 10)%>%
  glimpse -> spp_site_count

amphi_spp%>%
  anti_join(y = select(anf_sites_outMA, id))%>%
  right_join(y = spp_site_count, by = "species")%>%
  drop_na(data =., any_of("individuals"))%>%
  pivot_wider(names_from = species, values_from = individuals, id_cols=id, values_fn = sum) %>% #, values_fn = length)%>%
  inner_join(y=select(buff_1km,id, pop_1km))%>%
  left_join(y=select(buff_2km, id, pop_2km), by = "id")%>%
  left_join(y = select(buff_5km, id, pop_5km), by = "id")%>%
  rename("geometry1" = "geometry.x",
         "geometry2" = "geometry.y",
         "geometry5" = "geometry")%>%
  glimpse->amphi_spp_wide

#data analysis----
amphi_spp_wide[,2:91]%>%
map(~glm(. ~ amphi_spp_wide$pop_1km, family="poisson"))->list_model1km

lapply(list_model1km, summary)


#summary(glm(data = amphi_spp_wide, `Physalaemus cuvieri` ~ log(pop_5km+0.0001), family = "poisson"))


#data viz----
plot(pop_MA_1000_polyBR)
plot(buff_5km$geometry, add=T)
plot(x = log(amphi_spp_wide$pop_5km+0.0001), y = amphi_spp_wide$`Physalaemus cuvieri`)
