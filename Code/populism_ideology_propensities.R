## Estimate provincial political behavior on three axes: economics (left/right), populist leanings (high/low), regionalism (regionalist/pro-Spain)


#### Placing parties on these axes ####
## 2011
left_2011 <- grep("PSOE|AMAIUR|Izquierda|EQUO|PACMA|Andaluc|Esquerra|BNG|Equo|Verd", unique(results_2011$party), value = TRUE)
right_2011 <- grep("PP|PNV|CiU|Canaria|Foro|Plataforma", unique(results_2011$party), value = TRUE)
populist_2011 <- grep("Plataforma|Izquierda|EQUO", unique(results_2011$party), value = TRUE)
establishment_2011 <- grep("PSOE|PP", unique(results_2011$party), value = TRUE)
regional_2011 <- grep("PNV|Geroa|CiU|BNG|Andaluc|Amaiur|Esquerra|Canaria|Cantabria|Foro|Plataforma", unique(results_2011$party), value = TRUE)
national_2011 <- grep("PP|PSOE|Izquierda Plural|UPyD", unique(results_2011$party), value = TRUE)

## 2015
left_2015 <- grep("Podem|PSOE|Bildu|IU|Animalista|Verde|Comunista|per Mallorca|Socialist|Esquerra|BNG", unique(results_2015$party), value = TRUE)
right_2015 <- grep("PP|PNV|Ciudadanos|Vox", unique(results_2015$party), value = TRUE)
populist_2015 <- grep("Podemos|Ciudadanos|IU|Vox", unique(results_2015$party), value = TRUE)
establishment_2015 <- grep("PP|PSOE", unique(results_2015$party), value = TRUE)
regional_2015 <- grep("PNV|Bildu|Mallorca|Esquerra Republicana|Canario|BNG|tica de Catalunya", unique(results_2015$party), value = TRUE)
national_2015 <- grep("Podemos|PP|PSOE|IU|UPyD|Vox", unique(results_2015$party), value = TRUE)

## 2016
left_2016 <- grep("Podem|PSOE|Bildu|PACMA|Verde|Socialist|Esquerra|PODEMOS|BNG", unique(results_2016$party), value = TRUE)
right_2016 <- grep("PP|PNV|Ciudadanos|Vox|Ciutadan", unique(results_2016$party), value = TRUE)
populist_2016 <- grep("Podem|Ciudadanos|Vox|Ciutadan", unique(results_2016$party), value = TRUE)
establishment_2016 <- grep("PP|PSOE", unique(results_2016$party), value = TRUE)
regional_2016 <- grep("PNV|Bildu|Esquerra Republicana|Galega|Canario", unique(results_2016$party), value = TRUE)
national_2016 <- grep("Podemos|PP|PSOE|Ciudadanos|Vox|UPyD|Ciutadan", unique(results_2016$party), value = TRUE)

#### How much the nation leaned to one side or the other on each of these axes ####
source("Code/process_results.R")

## 2011
results_2011.nation <- results_2011 %>%
  mutate(left = case_when(grepl("PSOE|AMAIUR|Izquierda|EQUO|PACMA|Andaluc|Esquerra|BNG|Equo", party) ~ "Left",
                          grepl("PP|PNV|CiU|Canaria", party) ~ "Right"),
         populist = case_when(grepl("Plataforma|Izquierda|EQUO", party) ~ "Populist",
                              grepl("PSOE|PP", party) ~ "Establishment"),
         regional = case_when(grepl("Geroa|CiU|BNG|Andaluc|Amaiur|Esquerra|Canaria|Cantabria", party) ~ "Regional",
                              grepl("PP|PSOE|Izquierda Plural|UPyD", party) ~ "National")) %>%
  mutate(left_votes = sum((left == "Left")*votes, na.rm = TRUE),
         right_votes = sum((left == "Right")*votes, na.rm = TRUE),
         populist_votes = sum((populist == "Populist")*votes, na.rm = TRUE),
         establishment_votes = sum((populist == "Establishment")*votes, na.rm = TRUE),
         regional_votes = sum((regional == "Regional")*votes, na.rm = TRUE),
         national_votes = sum((regional == "National")*votes, na.rm = TRUE),
         total_votes = sum(votes, na.rm = TRUE)) %>%
  summarise(left_pct = mean(left_votes/total_votes),
            right_pct = mean(right_votes/total_votes),
            populist_pct = mean(populist_votes/total_votes),
            establishment_pct = mean(establishment_votes/total_votes),
            regional_pct = mean(regional_votes/total_votes),
            national_pct = mean(national_votes/total_votes))

## 2015
results_2015.nation <- results_2015 %>%
  mutate(left = case_when(grepl("Podem|PSOE|Bildu|IU|Animalista|Verde|Comunista|per Mallorca|Socialist|Esquerra|BNG", party) ~ "Left",
                          grepl("PP|PNV|Ciudadanos|Vox", party) ~ "Right"),
         populist = case_when(grepl("Podemos|Ciudadanos|IU|Vox", party) ~ "Populist",
                              grepl("PSOE|PP", party) ~ "Establishment"),
         regional = case_when(grepl("PNV|Bildu|Mallorca|Esquerra Republicana|Canario|BNG|tica de Catalunya", party) ~ "Regional",
                              grepl("Podemos|PP|PSOE|IU|UPyD|Vox", party) ~ "National")) %>%
  mutate(left_votes = sum((left == "Left")*votes, na.rm = TRUE),
         right_votes = sum((left == "Right")*votes, na.rm = TRUE),
         populist_votes = sum((populist == "Populist")*votes, na.rm = TRUE),
         establishment_votes = sum((populist == "Establishment")*votes, na.rm = TRUE),
         regional_votes = sum((regional == "Regional")*votes, na.rm = TRUE),
         national_votes = sum((regional == "National")*votes, na.rm = TRUE),
         total_votes = sum(votes, na.rm = TRUE)) %>%
  summarise(left_pct = mean(left_votes/total_votes),
            right_pct = mean(right_votes/total_votes),
            populist_pct = mean(populist_votes/total_votes),
            establishment_pct = mean(establishment_votes/total_votes),
            regional_pct = mean(regional_votes/total_votes),
            national_pct = mean(national_votes/total_votes))

## 2016
results_2016.nation <- results_2016 %>%
  mutate(left = case_when(grepl("Podem|PSOE|Bildu|PACMA|Verde|Socialist|Esquerra|PODEMOS|BNG", party) ~ "Left",
                          grepl("PP|PNV|Ciudadanos|Vox|Ciutadan", party) ~ "Right"),
         populist = case_when(grepl("Podem|Ciudadanos|Vox|Ciutadan", party) ~ "Populist",
                              grepl("PSOE|PP", party) ~ "Establishment"),
         regional = case_when(grepl("PNV|Bildu|Esquerra Republicana|Galega|Canario", party) ~ "Regional",
                              grepl("Podemos|PP|PSOE|Ciudadanos|Vox|UPyD|Ciutadan", party) ~ "National")) %>%
  mutate(left_votes = sum((left == "Left")*votes, na.rm = TRUE),
         right_votes = sum((left == "Right")*votes, na.rm = TRUE),
         populist_votes = sum((populist == "Populist")*votes, na.rm = TRUE),
         establishment_votes = sum((populist == "Establishment")*votes, na.rm = TRUE),
         regional_votes = sum((regional == "Regional")*votes, na.rm = TRUE),
         national_votes = sum((regional == "National")*votes, na.rm = TRUE),
         total_votes = sum(votes, na.rm = TRUE)) %>%
  summarise(left_pct = mean(left_votes/total_votes),
            right_pct = mean(right_votes/total_votes),
            populist_pct = mean(populist_votes/total_votes),
            establishment_pct = mean(establishment_votes/total_votes),
            regional_pct = mean(regional_votes/total_votes),
            national_pct = mean(national_votes/total_votes))

#### How much each province leaned to one side or the other relative to the nation

## 2011
results_2011.province <- results_2011 %>%
  mutate(left = case_when(grepl("Podem|PSOE|Bildu|PACMA|Verde|Socialist|Esquerra|PODEMOS|BNG", party) ~ "Left",
                          grepl("PP|PNV|Ciudadanos|Vox|Ciutadan", party) ~ "Right"),
         populist = case_when(grepl("Podem|Ciudadanos|Vox|Ciutadan", party) ~ "Populist",
                              grepl("PSOE|PP", party) ~ "Establishment"),
         regional = case_when(grepl("PNV|Bildu|Esquerra Republicana|Galega|Canario", party) ~ "Regional",
                              grepl("Podemos|PP|PSOE|Ciudadanos|Vox|UPyD|Ciutadan", party) ~ "National")) %>%
  group_by(community, province) %>%
  mutate(left_votes = sum((left == "Left")*votes, na.rm = TRUE),
         right_votes = sum((left == "Right")*votes, na.rm = TRUE),
         populist_votes = sum((populist == "Populist")*votes, na.rm = TRUE),
         establishment_votes = sum((populist == "Establishment")*votes, na.rm = TRUE),
         regional_votes = sum((regional == "Regional")*votes, na.rm = TRUE),
         national_votes = sum((regional == "National")*votes, na.rm = TRUE),
         total_votes = sum(votes, na.rm = TRUE)) %>%
  summarise(seats = sum(seats),
            left_pct = mean(left_votes/total_votes),
            right_pct = mean(right_votes/total_votes),
            populist_pct = mean(populist_votes/total_votes),
            establishment_pct = mean(establishment_votes/total_votes),
            regional_pct = mean(regional_votes/total_votes),
            national_pct = mean(national_votes/total_votes)) %>%
  left_join(community_key, by = "community") %>%
  left_join(province_key, by = "province") %>%
  ungroup() %>%
  dplyr::select(community = community_name, province = province_name, seats, left_pct, right_pct, populist_pct, establishment_pct, regional_pct, 
                national_pct)

## 2015
results_2015.province <- results_2015 %>%
  mutate(left = case_when(grepl("Podem|PSOE|Bildu|IU|Animalista|Verde|Comunista|per Mallorca|Socialist|Esquerra|BNG", party) ~ "Left",
                          grepl("PP|PNV|Ciudadanos|Vox", party) ~ "Right"),
         populist = case_when(grepl("Podemos|Ciudadanos|IU|Vox", party) ~ "Populist",
                              grepl("PSOE|PP", party) ~ "Establishment"),
         regional = case_when(grepl("PNV|Bildu|Mallorca|Esquerra Republicana|Canario|BNG|tica de Catalunya", party) ~ "Regional",
                              grepl("Podemos|PP|PSOE|IU|UPyD|Vox", party) ~ "National")) %>%
  group_by(community, province) %>%
  mutate(left_votes = sum((left == "Left")*votes, na.rm = TRUE),
         right_votes = sum((left == "Right")*votes, na.rm = TRUE),
         populist_votes = sum((populist == "Populist")*votes, na.rm = TRUE),
         establishment_votes = sum((populist == "Establishment")*votes, na.rm = TRUE),
         regional_votes = sum((regional == "Regional")*votes, na.rm = TRUE),
         national_votes = sum((regional == "National")*votes, na.rm = TRUE),
         total_votes = sum(votes, na.rm = TRUE)) %>%
  summarise(seats = sum(seats),
            left_pct = mean(left_votes/total_votes),
            right_pct = mean(right_votes/total_votes),
            populist_pct = mean(populist_votes/total_votes),
            establishment_pct = mean(establishment_votes/total_votes),
            regional_pct = mean(regional_votes/total_votes),
            national_pct = mean(national_votes/total_votes)) %>%
  left_join(community_key, by = "community") %>%
  left_join(province_key, by = "province") %>%
  ungroup() %>%
  dplyr::select(community = community_name, province = province_name, seats, left_pct, right_pct, populist_pct, establishment_pct, regional_pct, 
                national_pct)

## 2016
results_2016.province <- results_2016 %>%
  mutate(left = case_when(grepl("Podem|PSOE|Bildu|PACMA|Verde|Socialist|Esquerra|PODEMOS|BNG", party) ~ "Left",
                          grepl("PP|PNV|Ciudadanos|Vox|Ciutadan", party) ~ "Right"),
         populist = case_when(grepl("Podem|Ciudadanos|Vox|Ciutadan", party) ~ "Populist",
                              grepl("PSOE|PP", party) ~ "Establishment"),
         regional = case_when(grepl("PNV|Bildu|Esquerra Republicana|Galega|Canario", party) ~ "Regional",
                              grepl("Podemos|PP|PSOE|Ciudadanos|Vox|UPyD|Ciutadan", party) ~ "National")) %>%
  group_by(community, province) %>%
  mutate(left_votes = sum((left == "Left")*votes, na.rm = TRUE),
         right_votes = sum((left == "Right")*votes, na.rm = TRUE),
         populist_votes = sum((populist == "Populist")*votes, na.rm = TRUE),
         establishment_votes = sum((populist == "Establishment")*votes, na.rm = TRUE),
         regional_votes = sum((regional == "Regional")*votes, na.rm = TRUE),
         national_votes = sum((regional == "National")*votes, na.rm = TRUE),
         total_votes = sum(votes, na.rm = TRUE)) %>%
  summarise(seats = sum(seats),
            left_pct = mean(left_votes/total_votes),
            right_pct = mean(right_votes/total_votes),
            populist_pct = mean(populist_votes/total_votes),
            establishment_pct = mean(establishment_votes/total_votes),
            regional_pct = mean(regional_votes/total_votes),
            national_pct = mean(national_votes/total_votes)) %>%
  left_join(community_key, by = "community") %>%
  left_join(province_key, by = "province") %>%
  ungroup() %>%
  dplyr::select(community = community_name, province = province_name, seats, left_pct, right_pct, populist_pct, establishment_pct, regional_pct, 
                national_pct)
## 2011 lean
province_lean.2011 <- results_2011.province %>%
  mutate(year = 2011,
         left = left_pct - results_2011.nation$left_pct,
         right = right_pct - results_2011.nation$right_pct,
         populist = populist_pct - results_2011.nation$populist_pct,
         establishment = establishment_pct - results_2011.nation$establishment_pct,
         regional = regional_pct - results_2011.nation$regional_pct,
         national = national_pct - results_2011.nation$national_pct) %>%
  dplyr::select(community, province, seats, year, left, right, populist, establishment, regional, national)

## 2015 lean
province_lean.2015 <- results_2015.province %>%
  mutate(year = 2015,
         left = left_pct - results_2015.nation$left_pct,
         right = right_pct - results_2015.nation$right_pct,
         populist = populist_pct - results_2015.nation$populist_pct,
         establishment = establishment_pct - results_2015.nation$establishment_pct,
         regional = regional_pct - results_2015.nation$regional_pct,
         national = national_pct - results_2015.nation$national_pct) %>%
  dplyr::select(community, province, seats, year, left, right, populist, establishment, regional, national)

## 2016 lean
province_lean.2016 <- results_2016.province %>%
  mutate(year = 2016,
         left = left_pct - results_2016.nation$left_pct,
         right = right_pct - results_2016.nation$right_pct,
         populist = populist_pct - results_2016.nation$populist_pct,
         establishment = establishment_pct - results_2016.nation$establishment_pct,
         regional = regional_pct - results_2016.nation$regional_pct,
         national = national_pct - results_2016.nation$national_pct) %>%
  dplyr::select(community, province, seats, year, left, right, populist, establishment, regional, national)

## Overall lean
province_lean <- bind_rows(province_lean.2011, province_lean.2015, province_lean.2016) %>%
  mutate(weight = case_when(year == 2011 ~ 8,
                            year == 2015 ~ 4,
                            year == 2016 ~ 3)) %>%
  group_by(community, province) %>%
  summarise(left = wtd.mean(left, weights = weight),
            right = wtd.mean(right, weights = weight),
            populist = wtd.mean(populist, weights = weight),
            establishment = wtd.mean(establishment, weights = weight),
            regional = wtd.mean(regional, weights = weight),
            national = wtd.mean(national, weights = weight)) %>%
  ungroup()
