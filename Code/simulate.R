#### Simulation ####
source("Code/historical_polling_error.R")

## Polling average
poll_average <- polls_2019.logit_long %>%
  mutate(weight = 100*(age < 45)/(exp(age^0.3)*sqrt(sqrt(0.25/n))*sqrt(abs(date_spread - 5) + 1))) %>%
  group_by(party) %>%
  summarise(avg = wtd.mean(pct, weights = weight))

means <- poll_average$avg
names(means) <- poll_average$party %>% as.character()

## Party covariance in polls
poll_cov <- (polls_2019.logit %>%
  dplyr::select(pp:vox) %>%
  na.omit() %>%
  as.matrix() %>%
  cov.wt(wt = polls_2019.logit %>%
           dplyr::select(pp:vox, weight) %>%
           na.omit() %>%
           pull(weight))
  )$cov

set.seed(2019)
simulations <- rmvn(100000, means, poll_cov + error_cov) %>%
  invlogit() %>%
  as.data.frame()

names(simulations) <- names(means)

simulations <- simulations %>%
  as.tbl()
