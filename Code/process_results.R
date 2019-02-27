source("Code/library.R")

## Read in results, community and province keys
community_key <- read_csv("Data/community_key.csv")
province_key <- read_csv("Data/province_key.csv")

results_2011 <- process_spain_data(2011) %>%
  left_join(community_key) %>%
  left_join(province_key) %>%
  mutate(list = party,
         list = case_when(grepl("psoe", party, ignore.case = TRUE) ~ "psoe",
                          grepl("partido popular", party, ignore.case = TRUE) ~ "pp",
                          grepl("izquierda plural", party, ignore.case = TRUE) ~ "up",
                          grepl("pnv", party, ignore.case = TRUE) ~ "basque_nationalist",
                          grepl("upyd", party, ignore.case = TRUE) ~ "upyd",
                          grepl("esquerra republicana de catalunya", party, ignore.case = TRUE) ~ "catalan_republican",
                          grepl("bng", party, ignore.case = TRUE) ~ "galician_nationalist",
                          grepl("canaria", party, ignore.case = TRUE) ~ "canarian_coalition",
                          !grepl("psoe|partido popular|izquierda plural|pnv|esquerra republicana de catalunya|bng|canaria",
                                 party, ignore.case = TRUE) ~ list)) %>%
  group_by(community, province, community_name, province_name, year, party, list) %>%
  summarise(votes = sum(votes),
            seats = sum(seats)) %>%
  group_by(community, province, community_name, province_name, year, list) %>%
  summarise(votes = sum(votes),
            seats = sum(seats)) %>%
  group_by(community, province, community_name, province_name, year) %>%
  mutate(province_votes = sum(votes),
         pct = votes/province_votes)

results_2015 <- process_spain_data(2015) %>%
  left_join(community_key) %>%
  left_join(province_key) %>%
  mutate(list = party, 
         list = case_when(grepl("psoe", party, ignore.case = TRUE) ~ "psoe",
                          grepl("partido popular", party, ignore.case = TRUE) ~ "pp",
                          grepl("unidad popular", party, ignore.case = TRUE) ~ "up",
                          grepl("eh bildu", party, ignore.case = TRUE) ~ "eh_bildu",
                          grepl("pnv", party, ignore.case = TRUE) ~ "basque_nationalist",
                          grepl("esquerra republicana de catalunya", party, ignore.case = TRUE) ~ "catalan_republican",
                          grepl("bng", party, ignore.case = TRUE) ~ "galician_nationalist",
                          grepl("canaria", party, ignore.case = TRUE) ~ "canarian_coalition", 
                          grepl("ciudadanos", party, ignore.case = TRUE) ~ "ciudadanos",
                          grepl("podem|comunista", party, ignore.case = TRUE) ~ "up",
                          grepl("vox", party, ignore.case = TRUE) ~ "vox",
                          !grepl("psoe|partido popular|unidad popular|comunista|eh bildu|pnv|esquerra republicana de catalunya|bng|canaria|ciudadan|
                                 podem|vox", party, ignore.case = TRUE) ~ list)) %>%
  group_by(community, province, community_name, province_name, year, list, party) %>%
  summarise(votes = sum(votes),
            seats = sum(seats)) %>%
  group_by(community, province, community_name, province_name, year, list) %>%
  summarise(votes = sum(votes),
            seats = sum(seats)) %>%
  group_by(community, province, community_name, province_name, year) %>%
  mutate(province_votes = sum(votes),
         pct = votes/province_votes)

results_2016 <- process_spain_data(2016) %>%
  left_join(community_key) %>%
  left_join(province_key) %>%
  mutate(list = party,
         list = case_when(grepl("psoe", party, ignore.case = TRUE) ~ "psoe",
                          grepl("partido popular", party, ignore.case = TRUE) ~ "pp",
                          grepl("eh bildu", party, ignore.case = TRUE) ~ "eh_bildu",
                          grepl("pnv", party, ignore.case = TRUE) ~ "basque_nationalist",
                          grepl("esquerra republicana de catalunya", party, ignore.case = TRUE) ~ "catalan_republican",
                          grepl("bng", party, ignore.case = TRUE) ~ "galician_nationalist",
                          grepl("canaria", party, ignore.case = TRUE) ~ "canarian_coalition", 
                          grepl("ciudadan", party, ignore.case = TRUE) ~ "ciudadanos",
                          grepl("podem|comunista", party, ignore.case = TRUE) ~ "up",
                          grepl("vox", party, ignore.case = TRUE) ~ "vox",
                          !grepl("psoe|partido popular|eh bildu|pnv|esquerra republicana de catalunya|bng|canaria|ciudadan|podem|vox", party,
                                 ignore.case = TRUE) ~ list)) %>%
  group_by(community, province, community_name, province_name, year, list, party) %>%
  summarise(votes = sum(votes),
            seats = sum(seats)) %>%
  group_by(community, province, community_name, province_name, year, list) %>%
  summarise(votes = sum(votes),
            seats = sum(seats)) %>%
  group_by(community, province, community_name, province_name, year) %>%
  mutate(province_votes = sum(votes),
         pct = votes/province_votes)

