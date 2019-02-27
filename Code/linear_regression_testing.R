## Testing linear regression models
source("Code/process_results.R")

#### 2016 on 2015 ####
results_1516 <- bind_rows(results_2015, results_2016) %>%
  group_by(community, province, list) %>%
  arrange(community, province, list, year) %>%
  mutate(lag_pct = lag(pct))

## Simple linear regressions
pp_simple <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "pp", year == 2016))
psoe_simple <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "psoe", year == 2016))
ciudadanos_simple <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "ciudadanos", year == 2016))
up_simple <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "up", year == 2016))

summary(pp_simple)
summary(psoe_simple)
summary(ciudadanos_simple)
summary(up_simple)

## Adding in popular vote
pp_popularvote <- lm(pct~lag_pct+, data = results_1516 %>% filter(list == "pp", year == 2016))
psoe_popularvote <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "psoe", year == 2016))
ciudadanos_popularvote <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "ciudadanos", year == 2016))
up_popularvote <- lm(pct~lag_pct, data = results_1516 %>% filter(list == "up", year == 2016))

summary(pp_popularvote)
summary(psoe_popularvote)
summary(ciudadanos_popularvote)
summary(up_popularvote)