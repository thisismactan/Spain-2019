## Testing linear regression models
source("Code/process_results.R")

#### 2016 on 2015 ####
results_allyears <- bind_rows(results_2011, results_2015, results_2016) %>%
  group_by(community, province, list) %>%
  arrange(community, province, list, year) %>%
  mutate(lag_pct = lag(pct),
         swing = pct - lag_pct)

## Simple linear regressions
pp_simple <- lm(pct~lag_pct, data = results_allyears %>% filter(list == "pp", year == 2016))
psoe_simple <- lm(pct~lag_pct, data = results_allyears %>% filter(list == "psoe", year == 2016))
ciudadanos_simple <- lm(pct~lag_pct, data = results_allyears %>% filter(list == "ciudadanos", year == 2016))
up_simple <- lm(pct~lag_pct, data = results_allyears %>% filter(list == "up", year == 2016))

summary(pp_simple)
summary(psoe_simple)
summary(ciudadanos_simple)
summary(up_simple)

## No intercept
pp_noint <- lm(I(pct-lag_pct)~lag_pct, data = results_allyears %>% filter(list == "pp", year %in% 2015:2016))
psoe_noint <- lm(I(pct-lag_pct)~lag_pct, data = results_allyears %>% filter(list == "psoe", year %in% 2015:2016))
ciudadanos_noint <- lm(I(pct-lag_pct)~lag_pct, data = results_allyears %>% filter(list == "ciudadanos", year %in% 2015:2016))
up_noint <- lm(I(pct-lag_pct)~lag_pct, data = results_allyears %>% filter(list == "up", year %in% 2015:2016))

summary(pp_noint)
summary(psoe_noint)
summary(ciudadanos_noint)
summary(up_noint)

