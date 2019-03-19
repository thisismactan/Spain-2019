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
