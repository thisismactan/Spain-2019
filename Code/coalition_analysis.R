source("Code/simulate.R")

#### Probabilities of interest ####
simulation_results.natl %>%
  group_by(sim_number) %>%
  mutate(max_seats = max(seats)) %>%
  spread()