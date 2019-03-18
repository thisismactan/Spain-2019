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
  left_join(community_medians, by = "community_name") %>%
  left_join(community_pct5, by = "community_name") %>%
  left_join(community_pct95, by = "community_name") %>%
  mutate(community_info = paste0("<b><u>", community_name, "</b></u><br>", 
                                 "<font color = '#008CD7'><b>PP</b>: ", pp_median, " (",  pp_pct5, "–", pp_pct95, ")</font><br>",
                                 "<font color = 'red'><b>PSOE</b>: ", psoe_median, " (", psoe_pct5, "–", psoe_pct95, ")</font><br>",
                                 "<font color = '#683064'><b>Unidos Podemos</b>: ", up_median, " (", up_pct5, "–", up_pct95, ")</font><br>",
                                 "<font color = '#FA5000'><b>Ciudadanos</b>: ", ciudadanos_median, " (", ciudadanos_pct5, "–", ciudadanos_pct95, 
                                 ")</font><br>",
                                 "<font color = '#5AC035'><b>Vox</b>: ", vox_median, " (", vox_pct5, "–", vox_pct95, ")</font><br>"),
         community_info = case_when(
           community_name == "Catalonia" ~ paste0(community_info, "<font color = 'orange'><b>ERC</b>: ", catalan_republican_median, " (",
                                                  catalan_republican_pct5, "–", catalan_republican_pct95, ")</font><br>",
                                                  "<font color = '#00CED1'><b>PDeCAT</b>: ", catalan_european_democrat_median, " (",
                                                  catalan_european_democrat_pct5, "–", catalan_european_democrat_pct95, ")</font>"),
           community_name == "Basque Country" ~ paste0(community_info, "<font color = '#228B22'><b>Basque Nationalist</b>: ",
                                                       basque_nationalist_median, " (", basque_nationalist_pct5, "–",
                                                       basque_nationalist_pct95, ")</font><br>",
                                                       "<font color = #FF1493><b>EH Bildu</b>: ", eh_bildu_median, " (", eh_bildu_pct5, "–", 
                                                       eh_bildu_pct95, ")</font>"),
           community_name == "Navarre" ~ paste0(community_info, "<font color = #FF1493><b>EH Bildu</b>: ", eh_bildu_median, " (", eh_bildu_pct5, "–", 
                                                eh_bildu_pct95, ")</font>"),
           community_name == "Canary Islands" ~ paste0(community_info, "<font color = 'orange'><b>Canarian Coalition</b>: ",
                                                       canarian_coalition_median, " (", canarian_coalition_pct5, "–",
                                                       canarian_coalition_pct95, ")</font>"), 
           !(community_name %in% c("Catalonia", "Basque Country", "Navarre", "Canary Islands")) ~ community_info))


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
  
## Map
leaflet(community_shapes2) %>%
  addPolygons(weight = 1, opacity = 1, color = "#666666", label = ~community_name, popup = ~community_info, group = "Seats")
