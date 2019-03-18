## What's the distribution of province-level swings from 2015 to 2016
source("Code/process_results.R")

swings <- results_1516 %>%
  na.omit() %>%
  filter(list %in% c("pp", "psoe", "ciudadanos", "up")) %>%
  mutate(swing = pct - lag_pct)

pp_swings <- swings %>%
  filter(list == "pp")

psoe_swings <- swings %>%
  filter(list == "psoe")

ciudadanos_swings <- swings %>%
  filter(list == "ciudadanos")

up_swings <- swings %>%
  filter(list == "up")

pp_swingmodel <- lmer(swing~(1|community_name), data = pp_swings)
psoe_swingmodel <- lmer(swing~(1|community_name), data = psoe_swings)
ciudadanos_swingmodel <- lmer(swing~(1|community_name), data = ciudadanos_swings)
up_swingmodel <- lmer(swing~(1|community_name), data = up_swings)

pp_swingmodel.summary <- summary(pp_swingmodel)
psoe_swingmodel.summary <- summary(psoe_swingmodel)
ciudadanos_swingmodel.summary <- summary(ciudadanos_swingmodel)
up_swingmodel.summary <- summary(up_swingmodel)