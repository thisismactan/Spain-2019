## Exploratory scatterplots ##

#### Colors! ####
community_colors <- c("Andalusia" = "#006633",
                      "Aragon" = "#FCDD09",
                      "Asturias" = "#0066FF",
                      "Balearic Islands" = "#40104C",
                      "Basque Country" = "#D62718",
                      "Canary Islands" = "#0168AA",
                      "Cantabria" = "#F5898E",
                      "Castile–La Mancha" = "#F5C400",
                      "Castile and León" = "#612958",
                      "Catalonia" = "#EB740A",
                      "Ceuta" = "black",
                      "Extremadura" = "#00AB39",
                      "Galicia" = "#009ACD",
                      "La Rioja" = "#66BC29",
                      "Madrid" = "#C70318",
                      "Melilla" = "#366BAE",
                      "Murcia" = "#ED1C24",
                      "Navarre" = "#EAC102",
                      "Valencia" = "#DB0A13")

region_colors <- c("Asturias and Cantabria" = "#0066FF",
                   "Basque Country and Navarre" = "#D62718",
                   "Castile and Aragon" = "#612958",
                   "Catalonia" = "#E76F1C",
                   "Galicia" = "#009ACD",
                   "Outlying islands" = "#000000",
                   "Southern Spain" = "#006633",
                   "Valencia" = "#F9DE27")

#### Plot ####
source("Code/process_results.R")

large_parties <- c("psoe", "pp", "up", "ciudadanos")

results_2015 %>%
  filter(list %in% large_parties) %>%
  left_join(results_2016 %>% filter(list %in% large_parties), by = c("community", "province", "community_name", "province_name", "list")) %>%
  mutate(region = case_when(community_name %in% c("Andalusia", "Extremadura", "Murcia") ~ "Southern Spain",
                            community_name %in% c("Balearic Islands", "Canary Islands", "Ceuta", "Melilla") ~ "Outlying islands",
                            community_name %in% c("Aragon", "Castile–La Mancha", "Castile and León", "La Rioja", "Madrid") ~ "Castile and Aragon",
                            community_name %in% c("Catalonia") ~ "Catalonia",
                            community_name %in% c("Asturias", "Cantabria") ~ "Asturias and Cantabria",
                            community_name %in% c("Galicia") ~ "Galicia",
                            community_name %in% c("Basque Country", "Navarre") ~ "Basque Country and Navarre",
                            community_name %in% c("Valencia") ~ "Valencia")) %>%
  ggplot(aes(x = pct.x, y = pct.y)) +
  facet_wrap(~list, labeller = as_labeller(party_labels)) +
  geom_point(aes(col = region, size = (province_votes.x + province_votes.y)/2000), alpha = 2/3) +
  scale_colour_manual(name = "Region", values = region_colors, labels = names(region_colors)) +
  scale_size(name = "Average votes in 2015\nand 2016 (thousands)") +
  labs(title = "Spanish general election results by community", subtitle = "2015 vs. 2016", 
       x = "Share of 2015 vote", y = "Share of 2016 vote")

