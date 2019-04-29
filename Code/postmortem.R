## Polls vs. actual
results_2019 <- tibble(party = c("pp", "psoe", "up", "ciudadanos", "catalan_republican", "junts_catalunya", "basque_nationalist", "eh_bildu",
                                 "canarian_coalition", "vox"),
                       pct = 100*c(0.1669, 0.2869, 0.1431, 0.1585, 0.039, 0.0191, 0.0152, 0.01, 0.0052, 0.1026),
                       type = "Actual result")

polls_vs_actual <- poll_avg %>%
  mutate(type = "Poll average",
         pct = avg) %>%
  bind_rows(results_2019) %>%
  dplyr::select(-avg)

polls_vs_actual %>%
  mutate(party = ordered(party, levels = c("pp", "psoe", "up", "ciudadanos", "catalan_republican", "junts_catalunya", "basque_nationalist", "eh_bildu",
                                           "canarian_coalition", "vox"))) %>%
  ggplot(aes(x = party, y = pct, fill = party, alpha = type)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "darkgray") +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_labels) +
  scale_alpha_manual(name = "", values = c(1, 2/3), labels = c("Actual result", "Poll average")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "2019 Spanish general election results", subtitle = "Polls (light) vs. actual results (dark)",
       x = "Party", y = "%", caption = "Error bars indicate 90% CI")

## Distribution of errors across polls (boxplots)
polls_2019_error <- polls_2019.long %>%
  left_join(results_2019, by = "party") %>%
  mutate(error = pct.x - pct.y)

polls_2019_error %>%
  filter(age <= 21) %>%
  mutate(party = ordered(party, levels = c("pp", "psoe", "up", "ciudadanos", "catalan_republican", "junts_catalunya", "basque_nationalist", "eh_bildu",
                                           "canarian_coalition", "vox"))) %>%
  ggplot(aes(x = party, y = error, fill = party, weight = weight)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_labels) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "2019 Spanish general election polling error", subtitle = "Last three weeks of polling",
       x = "Party", y = "Polling error (pp)")

## Distributions vs. final results
simulation_results.natl %>%
  ungroup() %>%
  filter(party %in% names(major_party_labels)) %>%
  ggplot(aes(x = seats, y = ..density.., fill = party)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  scale_fill_manual(name = "Party", labels = major_party_labels, values = major_party_colors) +
  geom_vline(data = tibble(party = c("pp", "psoe", "up", "ciudadanos", "vox"),
                           seats = c(66, 123, 42, 57, 24)),
             aes(xintercept = seats, col = party), size = 1, show.legend = FALSE) +
  scale_colour_manual(name = "Party", labels = major_party_labels, values = major_party_colors) +
  labs(title = "2019 Spanish general election", x = "Seats", y = "Probability",
       subtitle = "Forecast vs. results")

## Maps
province_results_2019 <- read_csv("Data/results_2019.csv") %>%
  melt(id.vars = c("community_name", "province_name"), variable.name = "party", value.name = "votes") %>%
  group_by(community_name, province_name) %>%
  mutate(pct = votes / sum(votes, na.rm = TRUE)) %>%
  dplyr::select(-votes) %>%
  spread(party, pct)

province_shapes3 <- province_shapes2 %>%
  left_join(province_results_2019, by = "province_name") %>%
  mutate(max_vote = pmax(pp, psoe, up, ciudadanos, vox, catalan_republican, 
                         junts_catalunya, basque_nationalist, eh_bildu, canarian_coalition, na.rm = TRUE),
         color = case_when(pp == max_vote ~ "#008CD7",
                           psoe == max_vote ~ "red",
                           up == max_vote ~ "#683064",
                           ciudadanos == max_vote ~ "#FA5000",
                           vox == max_vote ~ "#5AC035",
                           catalan_republican == max_vote ~ "gold",
                           junts_catalunya == max_vote ~ "deeppink",
                           basque_nationalist == max_vote ~ "forestgreen",
                           eh_bildu == max_vote ~ "darkolivegreen",
                           canarian_coalition == max_vote ~ "yellow"),
         opacity = ((4*(max_vote - 0.25) + 0.3)^1.5)/1.1) %>%
  dplyr::select(-(pp_vote_mean:vox_vote_pct95))

leaflet() %>%
  addPolygons(data = province_shapes3, weight = 1, opacity = 1, color = "#666666", fillOpacity = ~opacity, fillColor = ~color, label = ~province_name,
              group = "Vote (province)") %>%
  addTiles(options = tileOptions(opacity = 0.5, fillOpacity = 0.5))

## Subnational polling
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
  merge(province_results_2019 %>% 
          melt(id.vars = c("community_name", "province_name"), variable.name = "party", value.name = "actual_pct"), 
        by.x = c("province", "party"), by.y = c("province_name", "party"), all.x = TRUE) %>%
  mutate(actual_pct = actual_pct*100,
         median_date = as.Date(median_date, format = "%d-%b-%y"),
         age = as.numeric(today() - median_date),
         Weight = 25/exp(age^0.3)/sqrt(abs(date_spread - 5) + 1)/sqrt(sqrt(n)),
         party_name = case_when(party == "pp" ~ "People's Party",
                                party == "psoe" ~ "Socialist Workers' Party",
                                party == "ciudadanos" ~ "Ciudadanos",
                                party == "up" ~ "Unidos Podemos",
                                party == "vox" ~ "Vox")) %>%
  melt(id.vars = c("province", "province_code", "community", "community_name", "region", "pollster", "median_date", "date_spread", "n", "age", 
                   "Weight", "party", "party_name", "actual_pct"), value.name = "pct") %>%
  as.tbl() %>%
  filter(!is.na(party_name), !is.na(region)) %>%
  ggplot(aes(x = pct, y = actual_pct, col = region)) +
  facet_wrap(~party_name, scales = "free_x") +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(name = "Region", values = region_colors, labels = names(region_colors)) +
  lims(x = c(0, 50), y = c(0, 50)) +
  labs(title = "Actual results vs. subnational polling", subtitle = "By party",
       y = "% of vote", x = "% in polls")

## Provincial predictions vs. provincial results
province_results_long <- province_results_2019 %>%
  ungroup() %>%
  melt(id.vars = c("community_name", "province_name"), variable.name = "party", value.name = "pct") %>%
  as.tbl()

region_colors <- c("Asturias and Cantabria" = "#0066FF",
                   "Basque Country and Navarre" = "#D62718",
                   "Castile and Aragon" = "#612958",
                   "Catalonia" = "#E76F1C",
                   "Galicia" = "#009ACD",
                   "Outlying islands" = "#000000",
                   "Southern Spain" = "#006633",
                   "Valencia" = "#F9DE27")

province_results_long %>%
  left_join(province_means %>%
              melt(id.vars = "province_name", variable.name = "party", value.name = "model_pct") %>%
              mutate(party = gsub("_vote_mean", "", party)), 
            by = c("province_name", "party")) %>%
  mutate(region = case_when(community_name %in% c("Andalusia", "Extremadura", "Murcia") ~ "Southern Spain",
                            community_name %in% c("Balearic Islands", "Canary Islands", "Ceuta", "Melilla") ~ "Outlying islands",
                            community_name %in% c("Aragon", "Castile–La Mancha", "Castile and León", "La Rioja", "Madrid") ~ "Castile and Aragon",
                            community_name %in% c("Catalonia") ~ "Catalonia",
                            community_name %in% c("Asturias", "Cantabria") ~ "Asturias and Cantabria",
                            community_name %in% c("Galicia") ~ "Galicia",
                            community_name %in% c("Basque Country", "Navarre") ~ "Basque Country and Navarre",
                            community_name %in% c("Valencia") ~ "Valencia")) %>%
  mutate(region = case_when(!is.na(region) ~ region,
                            is.na(region) ~ "Castile and Aragon")) %>%
  filter(party %in% c("pp", "psoe", "vox", "up", "ciudadanos")) %>%
  mutate(party = case_when(party == "pp" ~ "People's Party",
                           party == "psoe" ~ "Socialist Workers' Party",
                           party == "up" ~ "Unidas Podemos",
                           party == "ciudadanos" ~ "Ciudadanos",
                           party == "vox" ~ "Vox")) %>%
  ggplot(aes(x = 100*model_pct, y = 100*pct, col = region)) +
  facet_wrap(~party, scale = "free_x") +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(name = "Region", values = region_colors) +
  lims(x = c(0, 50), y = c(0, 50)) +
  labs(title = "Actual results vs. model predictions", subtitle = "By party",
       x = "% predicted by model", y = "% of vote")
