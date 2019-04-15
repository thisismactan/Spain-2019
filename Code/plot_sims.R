## Plotting simulation results
source("Code/simulate.R")

#### National distribution of seats ####
simulation_results.natl %>%
  ungroup() %>%
  filter(party %in% names(major_party_labels)) %>%
  ggplot(aes(x = seats, y = ..density.., fill = party)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  scale_fill_manual(name = "Party", labels = major_party_labels, values = major_party_colors) +
  labs(title = "2019 Spanish general election forecast", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))
  
#### By region ####
province_simulations_tbl %>%
  ungroup() %>%
  mutate(region = case_when(community_name %in% c("Andalusia", "Extremadura", "Murcia") ~ "Southern Spain",
                            community_name %in% c("Balearic Islands", "Canary Islands", "Ceuta", "Melilla") ~ "Outlying islands",
                            community_name %in% c("Aragon", "Castile–La Mancha", "Castile and León", "La Rioja", "Madrid") ~ "Castile and Aragon",
                            community_name %in% c("Catalonia") ~ "Catalonia",
                            community_name %in% c("Asturias", "Cantabria") ~ "Asturias and Cantabria",
                            community_name %in% c("Galicia") ~ "Galicia",
                            community_name %in% c("Basque Country", "Navarre") ~ "Basque Country / Navarre",
                            community_name %in% c("Valencia") ~ "Valencia")) %>%
  mutate(region = case_when(region %in% c("Asturias and Cantabria", "Basque Country / Navarre", "Galicia", "Outlying islands", "Valencia") ~ "The rest",
                            !(region %in% c("Asturias and Cantabria", "Basque Country / Navarre", "Galicia", "Outlying islands", "Valencia")) ~ region)) %>%
  group_by(region, sim_number, party) %>%
  summarise(seats = sum(seats)) %>%
  ungroup() %>%
  ggplot(aes(x = seats, y = ..density.., fill = party)) +
  facet_wrap(~region, scales = "free_x") +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Party", labels = party_labels, values = party_colors) +
  labs(title = "2019 Spanish general election forecast by region", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))
