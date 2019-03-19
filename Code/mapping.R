## Maps!
source("Code/simulate.R")

#### Communities ####
community_shapes <- readOGR(dsn = "Data/Shapefiles", layer = "ESP_adm1", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  ms_simplify() %>%
  mutate(community_name = c("Andalusia", "Aragon", "Cantabria", "Castile–La Mancha", "Castile and León", "Catalonia", "Ceuta and Melilla",
                            "Madrid", "Navarre", "Valencia", "Extremadura", "Galicia", "Balearic Islands", "Canary Islands", "La Rioja",
                            "Basque Country", "Asturias", "Murcia")) %>%
  left_join(community_key %>%
              slice(1:18) %>%
              mutate(case_when(community_name == "Ceuta" ~ "Ceuta and Melilla",
                               community_name != "Ceuta" ~ community_name)), 
            by = "community_name") 

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
                                                  "<font color = '#00CED1'><b>PDeCAT</b>: ", round(catalan_european_democrat_mean), " (",
                                                  catalan_european_democrat_pct5, "–", catalan_european_democrat_pct95, ")</font>"),
           community_name == "Basque Country" ~ paste0(community_info, "<font color = '#228B22'><b>Basque Nationalist</b>: ", 
                                                       round(basque_nationalist_mean), " (", basque_nationalist_pct5, "–", basque_nationalist_pct95, 
                                                       ")</font><br>", "<font color = #FF1493><b>EH Bildu</b>: ", round(eh_bildu_mean), " (", eh_bildu_pct5, 
                                                       "–", eh_bildu_pct95, ")</font>"),
           community_name == "Navarre" ~ paste0(community_info, "<font color = #FF1493><b>EH Bildu</b>: ", round(eh_bildu_mean), " (", eh_bildu_pct5, "–", 
                                                eh_bildu_pct95, ")</font>"),
           community_name == "Canary Islands" ~ paste0(community_info, "<font color = 'orange'><b>Canarian Coalition</b>: ", round(canarian_coalition_mean), 
                                                       " (", canarian_coalition_pct5, "–", canarian_coalition_pct95, ")</font>"), 
           !(community_name %in% c("Catalonia", "Basque Country", "Navarre", "Canary Islands")) ~ community_info)) %>%
  
  ## Colors
  mutate(max_seats = pmax(pp_mean, psoe_mean, up_mean, ciudadanos_mean, vox_mean, catalan_republican_mean, catalan_european_democrat_mean,
                          basque_nationalist_mean, eh_bildu_mean, canarian_coalition_mean, na.rm = TRUE),
         color = case_when(pp_mean == max_seats ~ "#008CD7",
                           psoe_mean == max_seats ~ "red",
                           up_mean == max_seats ~ "#683064",
                           ciudadanos_mean == max_seats ~ "#FA5000",
                           vox_mean == max_seats ~ "#5AC035",
                           catalan_republican_mean == max_seats ~ "gold",
                           catalan_european_democrat_mean == max_seats ~ "darkturquoise",
                           basque_nationalist_mean == max_seats ~ "forestgreen",
                           eh_bildu_mean == max_seats ~ "deeppink",
                           canarian_coalition_mean == max_seats ~ "yellow"))

#### Provinces ####
province_shapes <- readOGR(dsn = "Data/Shapefiles", layer = "ESP_adm2", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  ms_simplify() %>%
  mutate(province_name = c("Almería", "Cádiz", "Córdoba", "Granada", "Huleva", "Jaén", "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza",
                           "Cantabria (Santander)", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo", "Ávila", "Burgos", "León", 
                           "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona (Gerona)", "Lleida (Lérida)",
                           "Tarragona", "Ceuta", "Melilla", "Madrid", "Navarra", "Alicante", "Castellón / Castelló", "Valencia / València",
                           "Badajoz", "Cáceres", "A Coruña", "Lugo", "Ourense", "Pontevedra", "Illes Balears (Baleares)", "Las Palmas",
                           "Santa Cruz de Tenerife", "La Rioja (Logroño)", "Araba - Álava", "Gipuzkoa (Guipúzcoa)", "Bizkaia (Vizcaya)",
                           "Asturias (Oviedo)", "Murcia")) %>%
  left_join(province_key, by = "province_name") %>%
  left_join(provincial_medians, by = "province_name") %>%
  left_join(provincial_pct5, by = "province_name") %>%
  left_join(provincial_pct95, by = "province_name")

#### Mapping ####
leaflet(community_shapes2) %>%
  addPolygons(weight = 1, opacity = 1, color = "#666666", fillOpacity = 1, fillColor = ~color, label = ~community_name, popup = ~community_info, 
              group = "Seats")
