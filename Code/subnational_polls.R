source("code/library.R")

subnational_polls <- read_csv("Data/subnational_polling.csv")

region_colors <- c("Asturias and Cantabria" = "#0066FF",
                   "Basque Country and Navarre" = "#D62718",
                   "Castile and Aragon" = "#612958",
                   "Catalonia" = "#E76F1C",
                   "Galicia" = "#009ACD",
                   "Outlying islands" = "#000000",
                   "Southern Spain" = "#006633",
                   "Valencia" = "#F9DE27")

subnational_polls %>%
  melt(id.vars = c("province", "province_code", "pollster", "median_date", "date_spread", "n"),
       variable.name = "party", value.name = "pct") %>%
  mutate(community = case_when(province %in% c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén", "Málaga", "Sevilla") ~ "Andalusia",
                               province %in% c("Huesca", "Teruel", "Zaragoza") ~ "Aragon",
                               province %in% c("Asturias (Oviedo)") ~ "Asturias",
                               province %in% c("Illes Balears (Baleares)") ~ "Balearic Islands",
                               province %in% c("Araba - Álava", "Bizkaia (Vizcaya)", "Gipuzkoa (Guipúzcoa)") ~ "Basque Country",
                               province %in% c("Las Palmas", "Santa Cruz de Tenerife") ~ "Canary Islands",
                               province %in% c("Cantabria (Santander)") ~ "Cantabria",
                               province %in% c("Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo") ~ "Castile–La Mancha",
                               province %in% c("Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora") ~ 
                                 "Castile and León",
                               province %in% c("Barcelona", "Girona (Gerona)", "Lleida (Lérida)", "Tarragona") ~ "Catalonia",
                               province %in% c("Badajoz", "Cáceres") ~ "Extremadura",
                               province %in% c("A Coruña", "Lugo", "Ourense (Orense)", "Pontevedra") ~ "Galicia",
                               province %in% c("La Rioja (Logroño)") ~ "La Rioja",
                               province %in% c("Madrid") ~ "Madrid",
                               province %in% c("Murcia") ~ "Murcia",
                               province %in% c("Navarra") ~ "Navarre",
                               province %in% c("Valencia / València") ~ "Valencian Community")
  ) %>%
  mutate(region = case_when(community %in% c("Andalusia", "Extremadura", "Murcia") ~ "Southern Spain",
                            community %in% c("Balearic Islands", "Canary Islands", "Ceuta", "Melilla") ~ "Outlying islands",
                            community %in% c("Aragon", "Castile–La Mancha", "Castile and León", "La Rioja", "Madrid") ~ "Castile and Aragon",
                            community %in% c("Catalonia") ~ "Catalonia",
                            community %in% c("Asturias", "Cantabria") ~ "Asturias and Cantabria",
                            community %in% c("Galicia") ~ "Galicia",
                            community %in% c("Basque Country", "Navarre") ~ "Basque Country and Navarre",
                            community %in% c("Valencian Community") ~ "Valencia")) %>%
  merge(province_means %>%
          melt(id.vars = "province_name", variable.name = "party", value.name = "model_pct") %>%
          mutate(party = gsub("_vote_mean", "", party)), 
        by.x = c("province", "party"), by.y = c("province_name", "party"), all.x = TRUE) %>%
  mutate(model_pct = model_pct*100,
         median_date = as.Date(median_date, format = "%d-%b-%y"),
         age = as.numeric(today() - median_date),
         Weight = 25/exp(age^0.3)/sqrt(abs(date_spread - 5) + 1)/sqrt(sqrt(n)),
         party_name = case_when(party == "pp" ~ "People's Party",
                                party == "psoe" ~ "Socialist Workers' Party",
                                party == "ciudadanos" ~ "Ciudadanos",
                                party == "up" ~ "Unidos Podemos",
                                party == "vox" ~ "Vox")) %>%
  as.tbl() %>%
  filter(!is.na(party_name), !is.na(region)) %>%
  ggplot(aes(x = model_pct, y = pct, col = region, size = Weight)) +
  facet_wrap(~party_name, scales = "free_x") +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(name = "Region", values = region_colors, labels = names(region_colors)) +
  lims(x = c(0, 45)) +
  labs(title = "Model predictions vs. subnational polling", subtitle = "By province",
       x = "% predicted by model", y = "% in polls")
