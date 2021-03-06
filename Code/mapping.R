## Maps!
source("Code/simulate.R")

#### Read shapefiles ####
community_shapes <- readOGR(dsn = "Data/Shapefiles", layer = "ESP_adm1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  ms_simplify() %>%
  mutate(community_name = c("Andalusia", "Aragon", "Cantabria", "Castile–La Mancha", "Castile and León", "Catalonia", "Ceuta and Melilla",
                            "Madrid", "Navarre", "Valencia", "Extremadura", "Galicia", "Balearic Islands", "Canary Islands", "La Rioja",
                            "Basque Country", "Asturias", "Murcia")) %>%
  left_join(community_key %>%
              slice(1:18) %>%
              mutate(community_name = case_when(community_name == "Ceuta" ~ "Ceuta and Melilla",
                                                community_name != "Ceuta" ~ community_name)), 
            by = "community_name") 

province_shapes <- readOGR(dsn = "Data/Shapefiles", layer = "ESP_adm2", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  ms_simplify() %>%
  mutate(province_name = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén", "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza",
                           "Cantabria (Santander)", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo", "Ávila", "Burgos", "León", 
                           "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona (Gerona)", "Lleida (Lérida)",
                           "Tarragona", "Ceuta", "Melilla", "Madrid", "Navarra", "Alicante", "Castellón / Castelló", "Valencia / València",
                           "Badajoz", "Cáceres", "A Coruña", "Lugo", "Ourense (Orense)", "Pontevedra", "Illes Balears (Baleares)", "Las Palmas",
                           "Santa Cruz de Tenerife", "La Rioja (Logroño)", "Araba - Álava", "Gipuzkoa (Guipúzcoa)", "Bizkaia (Vizcaya)",
                           "Asturias (Oviedo)", "Murcia")) 

#### Communities ####
community_shapes2 <- community_shapes %>%
  left_join(community_key, by = "community_name") %>%
  left_join(community_means, by = "community_name") %>%
  left_join(community_pct5, by = "community_name") %>%
  left_join(community_pct95, by = "community_name") %>%
  
  ## Infoboxes
  mutate(community_info = paste0("<b><u>", community_name, "</b></u><br>", 
                                 "<font color = '#008CD7'><b>PP</b>: ", round(pp_mean), " (",  pp_pct5, "–", pp_pct95, ")</font><br>",
                                 "<font color = 'red'><b>PSOE</b>: ", round(psoe_mean), " (", psoe_pct5, "–", psoe_pct95, ")</font><br>",
                                 "<font color = '#683064'><b>Unidos Podemos</b>: ", round(up_mean), " (", up_pct5, "–", up_pct95, ")</font><br>",
                                 "<font color = '#FA5000'><b>Ciudadanos</b>: ", round(ciudadanos_mean), " (", ciudadanos_pct5, "–", ciudadanos_pct95, 
                                 ")</font><br>",
                                 "<font color = '#5AC035'><b>Vox</b>: ", round(vox_mean), " (", vox_pct5, "–", vox_pct95, ")</font><br>"),
         community_info = case_when(
           community_name == "Catalonia" ~ paste0(community_info, "<font color = 'orange'><b>ERC</b>: ", round(catalan_republican_mean), " (",
                                                  catalan_republican_pct5, "–", catalan_republican_pct95, ")</font><br>",
                                                  "<font color = '#FF1493'><b>JxCat</b>: ", round(junts_catalunya_mean), " (",
                                                  junts_catalunya_pct5, "–", junts_catalunya_pct95, ")</font>"),
           community_name == "Basque Country" ~ paste0(community_info, "<font color = '#228B22'><b>Basque Nationalist</b>: ", 
                                                       round(basque_nationalist_mean), " (", basque_nationalist_pct5, "–", basque_nationalist_pct95, 
                                                       ")</font><br>", "<font color = '#556B2F'><b>EH Bildu</b>: ", round(eh_bildu_mean), " (", 
                                                       eh_bildu_pct5, "–", eh_bildu_pct95, ")</font>"),
           community_name == "Navarre" ~ paste0(community_info, "<font color = #FF1493><b>EH Bildu</b>: ", round(eh_bildu_mean), " (", eh_bildu_pct5, "–", 
                                                eh_bildu_pct95, ")</font>"),
           community_name == "Canary Islands" ~ paste0(community_info, "<font color = 'orange'><b>Canarian Coalition</b>: ", round(canarian_coalition_mean), 
                                                       " (", canarian_coalition_pct5, "–", canarian_coalition_pct95, ")</font>"), 
           !(community_name %in% c("Catalonia", "Basque Country", "Navarre", "Canary Islands")) ~ community_info)) %>%
  
  ## Colors
  mutate(max_seats = pmax(pp_mean, psoe_mean, up_mean, ciudadanos_mean, vox_mean, catalan_republican_mean, junts_catalunya_mean,
                          basque_nationalist_mean, eh_bildu_mean, canarian_coalition_mean, na.rm = TRUE),
         total_seats = pp_mean + psoe_mean + up_mean + ciudadanos_mean + vox_mean + catalan_republican_mean + junts_catalunya_mean +
           basque_nationalist_mean + eh_bildu_mean + canarian_coalition_mean,
         color = case_when(pp_mean == max_seats ~ "#008CD7",
                           psoe_mean == max_seats ~ "red",
                           up_mean == max_seats ~ "#683064",
                           ciudadanos_mean == max_seats ~ "#FA5000",
                           vox_mean == max_seats ~ "#5AC035",
                           catalan_republican_mean == max_seats ~ "gold",
                           junts_catalunya_mean == max_seats ~ "darkturquoise",
                           basque_nationalist_mean == max_seats ~ "forestgreen",
                           eh_bildu_mean == max_seats ~ "deeppink",
                           canarian_coalition_mean == max_seats ~ "yellow"),
         opacity = ((4*(max_seats/total_seats - 0.2))^1.5)/1.1)

#### Provinces ####
province_shapes2 <- province_shapes %>%
  left_join(province_key, by = "province_name") %>%
  left_join(province_means, by = "province_name") %>%
  left_join(province_pct5, by = "province_name") %>%
  left_join(province_pct95, by = "province_name") %>%
  mutate(province_info = paste0(
    "<b><u>", province_name, "</b></u><br>", 
    "<font color = '#008CD7'><b>PP</b>: ", round(100*pp_vote_mean, 1), "% (",  round(100*pp_vote_pct5, 1), "% – ", round(100*pp_vote_pct95, 1), 
    "%)</font><br>",
    "<font color = 'red'><b>PSOE</b>: ", round(100*psoe_vote_mean, 1), "% (",  round(100*psoe_vote_pct5, 1), "% – ", round(100*psoe_vote_pct95, 1), 
    "%)</font><br>",
    "<font color = '#683064'><b>Unidos Podemos</b>: ", round(100*up_vote_mean, 1), "% (",  round(100*up_vote_pct5, 1), "% – ", round(100*up_vote_pct95, 1), 
    "%)</font><br>",
    "<font color = '#FA5000'><b>Ciudadanos</b>: ", round(100*ciudadanos_vote_mean, 1), "% (", round(100*ciudadanos_vote_pct5, 1), "% – ", 
    round(100*ciudadanos_vote_pct95, 1), "%)</font><br>",
    "<font color = '#5AC035'><b>Vox</b>: ", round(100*vox_vote_mean, 1), "% (",  round(100*vox_vote_pct5, 1), "% – ", round(100*vox_vote_pct95, 1), 
    "%)</font><br>"),
         province_info = case_when(
           province_name %in% c("Barcelona", "Girona (Gerona)", "Lleida (Lérida)", "Tarragona") ~ 
             paste0(province_info, 
                    "<font color = 'orange'><b>ERC</b>: ", round(100*catalan_republican_vote_mean, 1), "% (",
                    round(100*catalan_republican_vote_pct5, 1), "% – ", round(100*catalan_republican_vote_pct95, 1), "%)</font><br>",
                    "<font color = '#FF1493'><b>JxCat</b>: ", round(100*junts_catalunya_vote_mean, 1), "% (",
                    round(100*junts_catalunya_vote_pct5, 1), "% – ", round(100*junts_catalunya_vote_pct95, 1), "%)</font>"),
           province_name %in% c("Araba - Álava", "Bizkaia (Vizcaya)", "Gipuzkoa (Guipúzcoa)") ~ 
             paste0(province_info, 
                    "<font color = '#228B22'><b>Basque Nationalist</b>: ", round(100*basque_nationalist_vote_mean, 1), "% (", 
                    round(100*basque_nationalist_vote_pct5, 1), "% – ", round(100*basque_nationalist_vote_pct95, 1), "%)</font><br>", 
                    "<font color = '#556B2F'><b>EH Bildu</b>: ", round(100*eh_bildu_vote_mean, 1), "% (", round(100*eh_bildu_vote_pct5, 1), "% – ", 
                    round(100*eh_bildu_vote_pct95, 1), "%)</font>"),
           province_name == "Navarre" ~ 
             paste0(province_info, "<font color = '#556B2F'><b>EH Bildu</b>: ", round(100*eh_bildu_vote_mean, 1), "% (", round(100*eh_bildu_vote_pct5, 1), 
                    "% – ", round(100*eh_bildu_vote_pct95, 1), "%)</font>"),
           province_name %in% c("Santa Cruz de Tenerife", "Las Palmas") ~ 
             paste0(province_info, "<font color = 'orange'><b>Canarian Coalition</b>: ", round(100*canarian_coalition_vote_mean, 1), "% (", 
                    round(100*canarian_coalition_vote_pct5, 1), "% – ", round(100*canarian_coalition_vote_pct95, 1), "%)</font>"), 
           !(province_name %in% c("Barcelona", "Girona (Gerona)", "Lleida (Lérida)", "Tarragona", "Araba - Álava", "Bizkaia (Vizcaya)", 
                                  "Gipuzkoa (Guipúzcoa)", "Navarre", "Santa Cruz de Tenerife", "Las Palmas")) ~ province_info)) %>%
  ## Colors
  mutate(max_vote = pmax(pp_vote_mean, psoe_vote_mean, up_vote_mean, ciudadanos_vote_mean, vox_vote_mean, catalan_republican_vote_mean, 
                         junts_catalunya_vote_mean, basque_nationalist_vote_mean, eh_bildu_vote_mean, canarian_coalition_vote_mean, na.rm = TRUE),
         color = case_when(pp_vote_mean == max_vote ~ "#008CD7",
                           psoe_vote_mean == max_vote ~ "red",
                           up_vote_mean == max_vote ~ "#683064",
                           ciudadanos_vote_mean == max_vote ~ "#FA5000",
                           vox_vote_mean == max_vote ~ "#5AC035",
                           catalan_republican_vote_mean == max_vote ~ "gold",
                           junts_catalunya_vote_mean == max_vote ~ "deeppink",
                           basque_nationalist_vote_mean == max_vote ~ "forestgreen",
                           eh_bildu_vote_mean == max_vote ~ "darkolivegreen",
                           canarian_coalition_vote_mean == max_vote ~ "yellow"),
         opacity = ((4*(max_vote - 0.25) + 0.3)^1.5)/1.1)

#### Mapping ####
leaflet() %>%
  addPolygons(data = province_shapes2, weight = 1, opacity = 1, color = "#666666", fillOpacity = ~opacity, fillColor = ~color, label = ~province_name,
              popup = ~province_info, group = "Vote (province)") %>%
  addTiles(options = tileOptions(opacity = 0.5, fillOpacity = 0.5)) %>%
  addLayersControl(
    baseGroups = c("Seats (community)", "Vote (province)"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE)
  )
