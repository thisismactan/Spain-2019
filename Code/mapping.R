## Maps!
source("Code/simulate.R")

#### 
province_shapes <- readOGR(dsn = "Data/Shapefiles", layer = "ESP_adm2", stringsAsFactors = FALSE) %>%
  st_as_sf() %>%
  mutate(province_name = c("Almería", "Cádiz", "Córdoba", "Granada", "Huleva", "Jaén", "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza",
                           "Cantabria (Santander)", "Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo", "Ávila", "Burgos", "León", 
                           "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora", "Barcelona", "Girona (Gerona)", "Lleida (Lérida)",
                           "Tarragona", "Ceuta", "Melilla", "Madrid", "Navarra", "Alicante", "Castellón / Castelló", "Valencia / València",
                           "Badajoz", "Cáceres", "A Coruña", "Lugo", "Ourense", "Pontevedra", "Illes Balears (Baleares)", "Las Palmas",
                           "Santa Cruz de Tenerife", "La Rioja (Logroño)", "Araba - Álava", "Gipuzkoa (Guipúzcoa)", "Bizkaia (Vizcaya)",
                           "Asturias (Oviedo)", "Murcia")) %>%
  left_join(province_key, by = "province_name")
