## Estimating historical polling error
source("Code/poll_scrape_clean.R") # get polls

#### 2011 ####
results_2011.natl <- results_2011 %>%
  mutate(total_votes = sum(votes, na.rm = TRUE)) %>%
  group_by(party, total_votes) %>%
  summarise(party_votes = sum(votes, na.rm = TRUE)) %>%
  mutate(list = case_when(grepl("psoe", party, ignore.case = TRUE) ~ "psoe",
                          grepl("partido popular", party, ignore.case = TRUE) ~ "pp",
                          grepl("izquierda plural", party, ignore.case = TRUE) ~ "iu",
                          grepl("converg", party, ignore.case = TRUE) ~ "ciu",
                          grepl("pnv", party, ignore.case = TRUE) ~ "basque_nationalist",
                          grepl("upyd", party, ignore.case = TRUE) ~ "upyd",
                          grepl("esquerra republicana de catalunya", party, ignore.case = TRUE) ~ "catalan_republican",
                          grepl("bng", party, ignore.case = TRUE) ~ "galician_nationalist",
                          grepl("canaria", party, ignore.case = TRUE) ~ "canarian_coalition", 
                          grepl("amaiur", party, ignore.case = TRUE) ~ "amaiur")) %>%
  group_by(list, total_votes) %>%
  summarise(votes = sum(party_votes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = votes/total_votes) %>%
  na.omit()

results_2011.logit <- results_2011.natl %>%
  mutate(pct_logit = logit(pct))

#### 2015 ####
results_2015.natl <- results_2015 %>%
  mutate(total_votes = sum(votes, na.rm = TRUE)) %>%
  group_by(party, total_votes) %>%
  summarise(party_votes = sum(votes, na.rm = TRUE)) %>%
  mutate(list = case_when(grepl("psoe", party, ignore.case = TRUE) ~ "psoe",
                          grepl("partido popular", party, ignore.case = TRUE) ~ "pp",
                          grepl("unidad popular", party, ignore.case = TRUE) ~ "iu_up",
                          grepl("converg", party, ignore.case = TRUE) ~ "ciu",
                          grepl("eh bildu", party, ignore.case = TRUE) ~ "eh_bildu",
                          grepl("pnv", party, ignore.case = TRUE) ~ "basque_nationalist",
                          grepl("upyd", party, ignore.case = TRUE) ~ "upyd",
                          grepl("esquerra republicana de catalunya", party, ignore.case = TRUE) ~ "catalan_republican",
                          grepl("bng", party, ignore.case = TRUE) ~ "galician_nationalist",
                          grepl("canaria", party, ignore.case = TRUE) ~ "canarian_coalition", 
                          grepl("comprom", party, ignore.case = TRUE) ~ "compromis",
                          grepl("ciudadanos", party, ignore.case = TRUE) ~ "ciudadanos",
                          grepl("podemos", party, ignore.case = TRUE) ~ "podemos",
                          grepl("democr", party, ignore.case = TRUE) ~ "democracy_freedom")) %>%
  group_by(list, total_votes) %>%
  summarise(votes = sum(party_votes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = votes/total_votes) %>%
  na.omit()

results_2015.logit <- results_2015.natl %>%
  mutate(pct_logit = logit(pct))

#### 2016 ####
results_2016.natl <- results_2016 %>%
  mutate(total_votes = sum(votes, na.rm = TRUE)) %>%
  group_by(party, total_votes) %>%
  summarise(party_votes = sum(votes, na.rm = TRUE)) %>%
  mutate(list = case_when(grepl("psoe", party, ignore.case = TRUE) ~ "psoe",
                          grepl("partido popular", party, ignore.case = TRUE) ~ "pp",
                          grepl("eh bildu", party, ignore.case = TRUE) ~ "eh_bildu",
                          grepl("pnv", party, ignore.case = TRUE) ~ "basque_nationalist",
                          grepl("animalista", party, ignore.case = TRUE) ~ "animalist",
                          grepl("esquerra republicana de catalunya", party, ignore.case = TRUE) ~ "catalan_republican",
                          grepl("bng", party, ignore.case = TRUE) ~ "galician_nationalist",
                          grepl("canaria", party, ignore.case = TRUE) ~ "canarian_coalition", 
                          grepl("ciudadan", party, ignore.case = TRUE) ~ "ciudadanos",
                          grepl("podemos", party, ignore.case = TRUE) ~ "up",
                          grepl("democr", party, ignore.case = TRUE) ~ "democracy_freedom")) %>%
  group_by(list, total_votes) %>%
  summarise(votes = sum(party_votes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = votes/total_votes) %>%
  na.omit()

results_2016.logit <- results_2016.natl %>%
  mutate(pct_logit = logit(pct))


#### Estimating error variances ####
error_2011 <- polls_2011.logit %>%
  mutate(psoe = psoe - results_2011.logit$pct_logit[8],
         pp = pp - results_2011.logit$pct_logit[9],
         iu = iu - results_2011.logit$pct_logit[7],
         ciu = ciu - results_2011.logit$pct_logit[5],
         basque_nationalist = basque_nationalist - results_2011.logit$pct_logit[2],
         upyd = upyd - results_2011.logit$pct_logit[10],
         catalan_republican = catalan_republican - results_2011.logit$pct_logit[4],
         galician_nationalist = galician_nationalist - results_2011.logit$pct_logit[6],
         canarian_coalition = canarian_coalition - results_2011.logit$pct_logit[3],
         amaiur = amaiur - results_2011.logit$pct_logit[1],
         
         weight = 100*(age <= 45)/(exp(age^0.3)*ifelse(age <= 7, 3, 1)*sqrt(sqrt(0.25/n))*ifelse(date_spread == 1, 5, 1)*sqrt(1+abs(date_spread - 5))))

error_2015 <- polls_2015.logit %>%
  mutate(pp = pp - results_2015.logit$pct_logit[11],
         psoe = psoe - results_2015.logit$pct_logit[12],
         iu_up = iu_up - results_2015.logit$pct_logit[9],
         upyd = upyd - results_2015.logit$pct_logit[13],
         ciu = NA,
         eh_bildu = eh_bildu - results_2015.logit$pct_logit[7],
         basque_nationalist = basque_nationalist - results_2015.logit$pct_logit[1],
         catalan_republican = catalan_republican - results_2015.logit$pct_logit[3],
         galician_nationalist = democracy_freedom - results_2015.logit$pct_logit[8],
         canarian_coalition = canarian_coalition - results_2015.logit$pct_logit[2],
         compromis = compromis - results_2015.logit$pct_logit[5],
         ciudadanos = ciudadanos - results_2015.logit$pct_logit[4],
         podemos = podemos - results_2015.logit$pct_logit[10],
         democracy_freedom = democracy_freedom - results_2015.logit$pct_logit[6],
         
         weight = 100*(age <= 45)/(exp(age^0.3)*ifelse(age <= 7, 3, 1)*sqrt(sqrt(0.25/n))*ifelse(date_spread == 1, 5, 1)*sqrt(1+abs(date_spread - 5))))

error_2016 <- polls_2016.logit %>%
  mutate(pp = pp - results_2016.logit$pct_logit[9],
         psoe = psoe - results_2016.logit$pct_logit[10],
         ciudadanos = ciudadanos - results_2016.logit$pct_logit[5],
         catalan_republican = catalan_republican - results_2016.logit$pct_logit[4],
         democracy_freedom = democracy_freedom - results_2016.logit$pct_logit[6],
         basque_nationalist = basque_nationalist - results_2016.logit$pct_logit[2],
         animalist = animalist - results_2016.logit$pct_logit[1],
         eh_bildu = eh_bildu - results_2016.logit$pct_logit[7],
         canarian_coalition = canarian_coalition - results_2016.logit$pct_logit[3],
         galician_nationalist = democracy_freedom - results_2016.logit$pct_logit[8],
         up = up - results_2016.logit$pct_logit[11],
         
         weight = 100*(age <= 45)/(exp(age^0.3)*ifelse(age <= 7, 3, 1)*sqrt(sqrt(0.25/n))*ifelse(date_spread == 1, 5, 1)*sqrt(1+abs(date_spread - 5))))

## Covariance matrices
error_cov_2011 <- (error_2011 %>%
  dplyr::select(pp, psoe, up = iu, ciudadanos = ciu, catalan_republican, basque_nationalist, eh_bildu = weight,
                canarian_coalition) %>%
  mutate(eh_bildu = (basque_nationalist + catalan_republican)/2) %>%
  na.omit() %>%
  as.matrix() %>%
  cov.wt(wt = error_2011 %>% 
           dplyr::select(pp, psoe, up = iu, ciudadanos = ciu, catalan_republican, basque_nationalist, eh_bildu = basque_nationalist,
                         canarian_coalition, weight) %>% 
           na.omit() %>% 
           pull(weight))
)$cov
error_cov_2015 <- (error_2015 %>%
  dplyr::select(pp, psoe, up = podemos, ciudadanos, catalan_republican, basque_nationalist, eh_bildu, canarian_coalition) %>%
  na.omit() %>%
  as.matrix() %>%
  cov.wt(wt = error_2015 %>% 
           dplyr::select(pp, psoe, podemos, ciudadanos, catalan_republican, basque_nationalist, eh_bildu, canarian_coalition, weight) %>% 
           na.omit() %>% 
           pull(weight))
)$cov

error_cov_2016 <- (error_2016 %>%
  dplyr::select(pp, psoe, up, ciudadanos, catalan_republican, basque_nationalist, eh_bildu, canarian_coalition) %>%
  na.omit() %>%
  as.matrix() %>%
  cov.wt(wt = error_2016 %>% 
           dplyr::select(pp, psoe, up, ciudadanos, catalan_republican, basque_nationalist, eh_bildu, canarian_coalition, weight) %>% 
           na.omit() %>% 
           pull(weight))
)$cov

error_cov <- (error_cov_2011/8 + error_cov_2015/4 + error_cov_2016/3)*(24/17)

## Add in new parties: Catalan European Democrats, Animalists, Vox (covariance 0)
catalan_european_democrat <- rep(0, nrow(error_cov))
error_cov <- cbind(error_cov, 0)
catalan_european_democrat <- c(catalan_european_democrat, error_cov[5,5] + error_cov[6,6])
error_cov <- rbind(error_cov, catalan_european_democrat)

vox <- rep(0, nrow(error_cov))
error_cov <- cbind(error_cov, 0)
vox <- c(vox, 0.5*error_cov[1,1] + error_cov[3,3] + error_cov[4,4])
error_cov <- rbind(error_cov, vox)[c(1:5, 9, 6, 7, 8, 10), c(1:5, 9, 6, 7, 8, 10)]

colnames(error_cov) <- rownames(error_cov)
