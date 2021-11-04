# Wed Nov  3 10:04:08 2021 ------------------------------
# script para geoprocessamento e análise dos dados

#library----
library(sf)
library(raster)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(plyr)
library(ggplot2)

#data----
raster(here("dados/pop_MA_1000_ployBR.tif"))-> pop_MA_1000_polyBR
glimpse(buff_1km)
glimpse(buff_2km)
glimpse(buff_5km)
read.csv(file = here("dados/ATLANTIC_AMPHIBIANS_species.csv") -> amphi_spp

#geoprocessing----
#pop_MA_1000_polyBR[is.na(pop_MA_1000_polyBR)] <- 0

#buff_1km %>% 
#  dplyr::mutate(pop_1km = raster::extract(x = pop_MA_1000_polyBR,
#                                          y = buff_1km,
#                                          fun = sum,
#                                          na.rm = FALSE)) -> buff_1km
#glimpse(buff_1km)
#as.factor(buff_1km$id) -> buff_1km$id
#as.vector(buff_1km$pop_1km) ->buff_1km$pop_1km


#buff_2km %>% 
#  dplyr::mutate(pop_2km = raster::extract(x = pop_MA_1000_polyBR,
#                                          y = buff_2km,
#                                          fun = sum,
#                                          na.rm = FALSE)) -> buff_2km
#glimpse(buff_2km)
#as.factor(buff_2km$id) -> buff_2km$id
#as.vector(buff_2km$pop_2km) ->buff_2km$pop_2km

#buff_5km %>% 
#  dplyr::mutate(pop_5km = raster::extract(x = pop_MA_1000_polyBR,
#                                          y = buff_5km,
#                                          fun = sum,
#                                          na.rm = FALSE)) -> buff_5km
#glimpse(buff_5km)
#as.factor(buff_5km$id) -> buff_5km$id
#as.vector(buff_5km$pop_5km) ->buff_5km$pop_5km

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
  pivot_wider(names_from = species, values_from = individuals, id_cols=id, values_fn = sum) %>%
  inner_join(y=select(buff_1km,id, pop_1km))%>%
  left_join(y=select(buff_2km, id, pop_2km), by = "id")%>%
  left_join(y = select(buff_5km, id, pop_5km), by = "id")%>%
  dplyr::rename("geometry1" = "geometry.x",
         "geometry2" = "geometry.y",
         "geometry5" = "geometry")%>%
  glimpse->amphi_spp_wide

#data analysis----
amphi_spp_wide%>%
  select(-id, -geometry1, -geometry2, -geometry5)%>%
  map(~glm(. ~ log(amphi_spp_wide$pop_1km + 0.0001), family="poisson")) ->list_model1km
lapply(list_model1km, summary)-> summary_mods_1km

amphi_spp_wide%>%
  select(-id, -geometry1, -geometry2, -geometry5)%>%
  map(~glm(. ~ log(amphi_spp_wide$pop_2km + 0.0001), family="poisson")) ->list_model2km
lapply(list_model2km, summary)-> summary_mods_2km

amphi_spp_wide%>%
  select(-id, -geometry1, -geometry2, -geometry5)%>%
  map(~glm(. ~ log(amphi_spp_wide$pop_5km + 0.0001), family="poisson")) ->list_model5km
lapply(list_model5km, summary)-> summary_mods_5km

##summary tables----
###1km
lapply(summary_mods_1km, coef)->coefs_model1km
ldply(coefs_model1km, as.data.frame)-> df_coefs_1km

df_coefs_1km%>%  
  group_by(.id)%>%
  filter(n()>1)%>%
  glimpse -> df_coefs_1km_1

seq_len(nrow(df_coefs_1km_1)) %% 2 -> row_odd
df_coefs_1km_1[row_odd == 0, ] -> df_coefs_1km_2

###2km
lapply(summary_mods_2km, coef)->coefs_model2km
ldply(coefs_model2km, as.data.frame)-> df_coefs_2km

df_coefs_2km%>%  
  group_by(.id)%>%
  filter(n()>1)%>%
  glimpse -> df_coefs_2km_1

seq_len(nrow(df_coefs_2km_1)) %% 2 -> row_odd
df_coefs_2km_1[row_odd == 0, ] -> df_coefs_2km_2

###5km
lapply(summary_mods_5km, coef)->coefs_model5km
ldply(coefs_model5km, as.data.frame)-> df_coefs_5km

df_coefs_5km%>%  
  group_by(.id)%>%
  filter(n()>1)%>%
  glimpse -> df_coefs_5km_1

seq_len(nrow(df_coefs_5km_1)) %% 2 -> row_odd
df_coefs_5km_1[row_odd == 0, ] -> df_coefs_5km_2

##combined talbe----
df_coefs_1km_2%>%
  ungroup()%>%
  dplyr::rename(estimate_1km = Estimate,
                std.error_1km = `Std. Error`,
                z.value_1km = `z value`,
                p.value_1km = `Pr(>|z|)`)%>%
  left_join(y = df_coefs_2km_2)%>%
  dplyr::rename(estimate_2km = Estimate,
                std.error_2km = `Std. Error`,
                z.value_2km = `z value`,
                p.value_2km = `Pr(>|z|)`)%>%
  left_join(y = df_coefs_5km_2)%>%
  dplyr::rename(estimate_5km = Estimate,
                std.error_5km = `Std. Error`,
                z.value_5km = `z value`,
                p.value_5km = `Pr(>|z|)`)%>%
  glimpse -> tab_results

#data viz----
plot(pop_MA_1000_polyBR)
plot(buff_5km$geometry, add=T)
plot(x = amphi_spp_wide$pop_5km, y = amphi_spp_wide$`Physalaemus cuvieri`)

### all positive
amphi_spp_wide%>%
  pivot_longer(cols = 2:90)%>%
  right_join(y = select(tab_results, .id, estimate_1km, p.value_1km, estimate_2km, estimate_5km,p.value_2km,p.value_5km),
             by = c("name" = ".id"))%>%
  filter(estimate_1km > 0 &
           estimate_2km > 0 &
           estimate_5km > 0 &
           p.value_1km < 0.05 &
           p.value_2km < 0.05 &
           p.value_5km < 0.05)%>%
  glimpse -> amphi_spp_long_all_positive


ggplot(data = amphi_spp_long_all_positive) +
  #geom_point(aes(x = pop_1km, y = value))+
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_1km), method = "glm", se = F, col = "red", size = 0.5) +
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_2km), method = "glm", se = F, col = "green", size = 0.5) +
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_5km), method = "glm", se = F, col = "blue", size = 0.5) +
  facet_wrap(~name, scales = "free") +
  xlab("Tamanho populacional") + ylab("Abundância") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3)) ->spp_all_positive

### all negative
amphi_spp_wide%>%
  pivot_longer(cols = 2:90)%>%
  right_join(y = select(tab_results, .id, estimate_1km, p.value_1km, estimate_2km, estimate_5km,p.value_2km,p.value_5km),
             by = c("name" = ".id"))%>%
  filter(estimate_1km < 0 &
         estimate_2km < 0 &
         estimate_5km < 0 &
         p.value_1km < 0.05 &
         p.value_2km < 0.05 &
         p.value_5km < 0.05)%>%
  glimpse -> amphi_spp_long_all_negative


ggplot(data = amphi_spp_long_all_negative) +
  #geom_point(aes(x = pop_1km, y = value))+
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_1km), method = "glm", se = F, col = "red", size = 0.5) +
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_2km), method = "glm", se = F, col = "green", size = 0.5) +
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_5km), method = "glm", se = F, col = "blue", size = 0.5) +
  facet_wrap(~name, scales = "free") +
  xlab("Tamanho populacional") + ylab("Abundância") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3)) -> spp_all_negative

###mixed per buffer
amphi_spp_wide%>%
  pivot_longer(cols = 2:90)%>%
  right_join(y = select(tab_results, .id, estimate_1km, p.value_1km, estimate_2km, estimate_5km,p.value_2km,p.value_5km),
             by = c("name" = ".id"))%>%
  anti_join(y=amphi_spp_long_all_positive)%>%
  anti_join(y=amphi_spp_long_all_negative)%>%
  filter(p.value_1km < 0.05 &
           p.value_2km < 0.05 &
           p.value_5km < 0.05)%>%
  glimpse -> amphi_spp_long_mixed


ggplot(data = amphi_spp_long_mixed) +
  #geom_point(aes(x = pop_1km, y = value))+
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_1km), method = "glm", se = F, col = "red", size = 0.5) +
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_2km), method = "glm", se = F, col = "green", size = 0.5) +
  stat_smooth(formula = y ~ log(x + 0.0001), aes(y = value, x=pop_5km), method = "glm", se = F, col = "blue", size = 0.5) +
  facet_wrap(~name, scales = "free") +
  xlab("Tamanho populacional") + ylab("Abundância") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.3))-> spp_mixed

ggsave(plot = spp_all_negative, filename = "spp_all_negative.jpg", dpi = 300, width = 14, height = 8)
ggsave(plot = spp_all_positive, filename = "spp_all_positive.jpg", dpi = 300, width = 10, height = 8)
ggsave(plot = spp_mixed, filename = "spp_mixed.jpg", dpi = 300, width = 12, height = 8)
