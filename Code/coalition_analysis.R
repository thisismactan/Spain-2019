source("Code/simulate.R")

#### Probabilities of interest ####

## Which party is largest?
simulation_results.natl %>%
  group_by(sim_number) %>%
  mutate(max_seats = max(seats)) %>%
  spread(party, seats) %>%
  mutate(pp_largest = pp == max_seats,
         psoe_largest = psoe == max_seats,
         up_largest = up == max_seats,
         ciudadanos_largest = ciudadanos == max_seats,
         vox_largest = vox == max_seats) %>%
  ungroup() %>%
  summarise(PP = mean(pp_largest),
            PSOE = mean(psoe_largest),
            UP = mean(up_largest),
            Ciudadanos = mean(ciudadanos_largest),
            Vox = mean(vox_largest)) %>%
  melt(variable.name = "Party", value.name = "Probability")

## Possible coalitions

## Two-party coalitions
simulation_results.natl %>%
  group_by(sim_number) %>%
  spread(party, seats) %>%
  mutate(`PSOE + UP` = psoe + up > 175,
         `PSOE + Ciudadanos` = psoe + ciudadanos > 175,
         `PP + Ciudadanos` = pp + ciudadanos > 175,
         `PP + Vox` = pp + vox > 175) %>%
  ungroup() %>%
  summarise_at(vars(c("PSOE + UP", "PSOE + Ciudadanos", "PP + Ciudadanos", "PP + Vox")), mean) %>%
  melt(variable.name = "Coalition", value.name = "Prob") %>% 
  as.tbl()

## Three-party coalitions
simulation_results.natl %>%
  group_by(sim_number) %>%
  spread(party, seats) %>%
  mutate(`PSOE + UP + Ciudadanos` = psoe + up + ciudadanos > 175,
         `PSOE + UP + Catalan Republican` = psoe + up + catalan_republican > 175,
         `PP + Ciudadanos + Vox` = pp + ciudadanos + vox > 175) %>%
  ungroup() %>%
  summarise_at(vars(c("PSOE + UP + Ciudadanos", "PSOE + UP + Catalan Republican", "PP + Ciudadanos + Vox")), mean) %>%
  melt(variable.name = "Coalition", value.name = "Prob") %>%
  as.tbl()
