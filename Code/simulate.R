#### Simulation ####
source("Code/polling_average.R")
source("Code/historical_polling_error.R")
source("Code/process_results.R")

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

## Simulations
set.seed(2019)

# National popular vote
n.iter <- 25000
simulations <- rmvn(n.iter, means, poll_cov + error_cov) %>%
  invlogit() %>%
  t()

rownames(simulations) <- names(means)

results_2016.national <- results_2016 %>%
  ungroup() %>%
  mutate(nation_votes = sum(votes)) %>%
  group_by(list, nation_votes) %>%
  summarise(votes = sum(votes)) %>%
  mutate(pct = votes/nation_votes) %>%
  pull(pct) %>%
  "["(c(10, 11, 14, 4, 3, 5, 1, 6, 2, 15))

simulated_swings <- simulations - results_2016.national

## Projecting regional party swings
source("Code/regional_performance_estimation.R")
source("Code/swing_variance.R")
simulated_swings[9,] <- predict(regional_model_list[[1]], newdata = data.frame(national_vote = simulated_swings[9,]))
simulated_swings[5,] <- predict(regional_model_list[[2]], newdata = data.frame(national_vote = simulated_swings[5,]))
simulated_swings[6,] <- predict(regional_model_list[[2]], newdata = data.frame(national_vote = simulated_swings[6,]))
simulated_swings[7,] <- predict(regional_model_list[[3]], newdata = data.frame(national_vote = simulated_swings[7,]))
simulated_swings[8,] <- predict(regional_model_list[[3]], newdata = data.frame(national_vote = simulated_swings[8,]))

results_2016.wide <- results_2016 %>%
  dplyr::select(list, pct) %>%
  spread(list, pct) %>%
  ungroup() %>%
  mutate(junts_catalunya = `Converg&egrave;ncia Democr&agrave;tica de Catalunya (CDC)`) %>%
  dplyr::select(names(means)) %>%
  replace_na(list(pp = 0, psoe = 0, up = 0, ciudadanos = 0, catalan_republican = NA, junts_catalunya = NA, basque_nationalist = NA,
                  eh_bildu = NA, canarian_coalition = NA, vox = 0)) %>%
  as.matrix()

provincial_seats <- results_2016 %>%
  dplyr::select(list, pct) %>%
  spread(list, pct) %>%
  ungroup() %>%
  mutate(junts_catalunya = NA) %>%
  dplyr::select(province, names(means)) %>%
  left_join(province_key) %>%
  pull(total_seats)

## Order of provinces
provinces_ordered <- results_2016 %>%
  dplyr::select(list, pct) %>%
  spread(list, pct) %>%
  ungroup()

## Add community- and province-level noise
community_sd.pp <- sqrt(pp_swingmodel.summary$varcor$community_name)
community_sd.psoe <- sqrt(psoe_swingmodel.summary$varcor$community_name)
community_sd.ciudadanos <- sqrt(ciudadanos_swingmodel.summary$varcor$community_name)
community_sd.up <- sqrt(up_swingmodel.summary$varcor$community_name)
community_sd.vox <- sqrt(community_sd.pp^2 + community_sd.psoe^2 + community_sd.ciudadanos^2 + community_sd.up^2)

province_sd.pp <- pp_swingmodel.summary$sigma
province_sd.psoe <- psoe_swingmodel.summary$sigma
province_sd.ciudadanos <- ciudadanos_swingmodel.summary$sigma
province_sd.up <- up_swingmodel.summary$sigma
province_sd.vox <- sqrt(province_sd.pp^2 + province_sd.psoe^2 + province_sd.ciudadanos^2 + province_sd.up^2)

## Simulate!
province_sims <- rep(list(simulated_swings), 52)
province_seatsims <- province_votesims <- vector("list", 52)

start_time <- Sys.time()
set.seed(2019)
for(i in 1:52) {
  cat("Province ", provinces_ordered$province[i], ": ", provinces_ordered$province_name[i], ", ", provinces_ordered$community_name[i], "\n", sep = "")
  ## Uniform national swing
  province_sims[[i]] <- pmax(province_sims[[i]] + results_2016.wide[i,], 0)
  
  ## Add province-level noise
  province_sims[[i]][1,] <- province_sims[[i]][1,] + rnorm(n.iter, 0, province_sd.pp) 
  province_sims[[i]][2,] <- province_sims[[i]][2,] + rnorm(n.iter, 0, province_sd.psoe) 
  province_sims[[i]][3,] <- province_sims[[i]][3,] + rnorm(n.iter, 0, province_sd.up) 
  province_sims[[i]][4,] <- province_sims[[i]][4,] + rnorm(n.iter, 0, province_sd.ciudadanos) 
  province_sims[[i]][10,] <- ((1 - colSums(province_sims[[i]][1:9,], na.rm = TRUE)) + province_sims[[i]][10,])/2 + rnorm(n.iter, 0, province_sd.vox)
  
  ## Add community-level noise
  if(i %in% c(1,9,12,13,14,15,17,18,22,31,35,37,41,42,43,46,47,48,51,52)) {
    province_sims[[i]][1,] <- province_sims[[i]][1,] + rnorm(n.iter, 0, community_sd.pp) 
    province_sims[[i]][2,] <- province_sims[[i]][2,] + rnorm(n.iter, 0, community_sd.psoe) 
    province_sims[[i]][3,] <- province_sims[[i]][3,] + rnorm(n.iter, 0, community_sd.up) 
    province_sims[[i]][4,] <- province_sims[[i]][4,] + rnorm(n.iter, 0, community_sd.ciudadanos) 
    province_sims[[i]][10,] <- province_sims[[i]][10,] + rnorm(n.iter, 0, community_sd.vox)
  }
  
  if(i %in% 31:34) {
    province_sims[[i]][10,] <- (province_sims[[i]][10,])/4
  }
  
  ## Record "actual" vote shares before recalculating
  province_sims[[i]][province_sims[[i]] < 0] <- 0
  province_votesims[[i]] <- province_sims[[i]]/rep(colSums(province_sims[[i]], na.rm = TRUE), each = 10)
  
  province_sims[[i]][province_sims[[i]] < 0.03] <- 0
  province_seatsims[[i]] <- floor(province_sims[[i]]*provincial_seats[i])
  province_sims[[i]] <- province_sims[[i]]/rep(colSums(province_sims[[i]], na.rm = TRUE), each = 10)
  
  ## Allocate missings
  counter <- 0
  seats_missing <- provincial_seats[i] - colSums(province_seatsims[[i]], na.rm = TRUE)
  while(sum(seats_missing) > 0) { # repeat until every simulation has no seats missing
    add_seats <- seats_missing > 0 # indicator for whether there are still seats to be allocated
    hypothetical_votes_per_seat <- province_sims[[i]]/(province_seatsims[[i]] + add_seats) # new vote-to-seat ratio
    max_share <- colMaxs(hypothetical_votes_per_seat) # which party has the highest new vote-to-seat ratio
    new_seat_inds <- cbind(row = max_share, col = 1:n.iter)[add_seats,]
    
    # Ensure this is a two-column matrix or break if no more seats to be added
    if(length(dim(new_seat_inds)) == 1) {
      new_seat_inds <- t(as.matrix(new_seat_inds))
    } else if(length(dim(new_seat_inds)) == 0) {
      break
    }
    
    add_new_seats <- matrix(0, 10, n.iter)
    add_new_seats[new_seat_inds] <- 1
    
    province_seatsims[[i]] <- province_seatsims[[i]] + add_new_seats
    seats_missing <- provincial_seats[i] - colSums(province_seatsims[[i]], na.rm = TRUE)
    counter <- counter + 1
  }
}
Sys.time() - start_time

## Reshape into tibble
province_simulations_list <- province_seatsims %>%
  lapply(t) %>%
  lapply(as.data.frame) %>%
  lapply(as.tbl)

for(i in 1:52) {
  province_simulations_list[[i]] <- province_simulations_list[[i]] %>%
    mutate(sim_number = 1:n(),
           community = provinces_ordered$community[i],
           community_name = provinces_ordered$community_name[i],
           province = provinces_ordered$province[i],
           province_name = provinces_ordered$province_name[i])
}

province_simulations_tbl <- bind_rows(province_simulations_list) %>%
  dplyr::select(community, province, community_name, province_name, sim_number, everything()) %>%
  melt(id.vars = c("community", "province", "community_name", "province_name", "sim_number"),
       variable.name = "party", value.name = "seats") %>%
  as.tbl()

## National results
simulation_results.natl <- province_simulations_tbl %>%
  group_by(sim_number, party) %>%
  summarise(seats = sum(seats, na.rm = TRUE))

## Distribution
distributions <- simulation_results.natl %>%
  group_by(party) %>%
  summarise(mean = mean(seats),
            sd = sd(seats),
            pct_5 = quantile(seats, 0.05),
            pct_25 = quantile(seats, 0.25),
            pct_50 = quantile(seats, 0.5),
            pct_75 = quantile(seats, 0.75),
            pct_95 = quantile(seats, 0.95))

distributions

## Community distributions
community_means <- province_simulations_tbl %>%
  mutate(community_name = case_when(community_name %in% c("Ceuta", "Melilla") ~ "Ceuta and Melilla",
                                    !(community_name %in% c("Ceuta", "Melilla")) ~ community_name)) %>%
  group_by(party, community_name, sim_number) %>%
  summarise(seats = sum(seats, na.rm = TRUE)) %>%
  group_by(party, community_name) %>%
  summarise(mean_seats = mean(seats, na.rm = TRUE)) %>%
  spread(party, mean_seats) %>%
  dplyr::select(community_name, pp_mean = pp, psoe_mean = psoe, up_mean = up, ciudadanos_mean = ciudadanos, 
                catalan_republican_mean = catalan_republican, junts_catalunya_mean = junts_catalunya,
                basque_nationalist_mean = basque_nationalist, eh_bildu_mean = eh_bildu, 
                canarian_coalition_mean = canarian_coalition, vox_mean = vox)

community_pct5 <- province_simulations_tbl %>%
  mutate(community_name = case_when(community_name %in% c("Ceuta", "Melilla") ~ "Ceuta and Melilla",
                                    !(community_name %in% c("Ceuta", "Melilla")) ~ community_name)) %>%
  group_by(party, community_name, sim_number) %>%
  summarise(seats = sum(seats, na.rm = TRUE)) %>%
  group_by(party, community_name) %>%
  summarise(pct5_seats = quantile(seats, 0.05, na.rm = TRUE)) %>%
  spread(party, pct5_seats) %>%
  dplyr::select(community_name, pp_pct5 = pp, psoe_pct5 = psoe, up_pct5 = up, ciudadanos_pct5 = ciudadanos, 
                catalan_republican_pct5 = catalan_republican, junts_catalunya_pct5 = junts_catalunya, 
                basque_nationalist_pct5 = basque_nationalist, eh_bildu_pct5 = eh_bildu, 
                canarian_coalition_pct5 = canarian_coalition, vox_pct5 = vox)

community_pct95 <- province_simulations_tbl %>%
  mutate(community_name = case_when(community_name %in% c("Ceuta", "Melilla") ~ "Ceuta and Melilla",
                                    !(community_name %in% c("Ceuta", "Melilla")) ~ community_name)) %>%
  group_by(party, community_name, sim_number) %>%
  summarise(seats = sum(seats, na.rm = TRUE)) %>%
  group_by(party, community_name) %>%
  summarise(pct95_seats = quantile(seats, 0.95, na.rm = TRUE)) %>%
  spread(party, pct95_seats) %>%
  dplyr::select(community_name, pp_pct95 = pp, psoe_pct95 = psoe, up_pct95 = up, ciudadanos_pct95 = ciudadanos, 
                catalan_republican_pct95 = catalan_republican, junts_catalunya_pct95 = junts_catalunya, 
                basque_nationalist_pct95 = basque_nationalist, eh_bildu_pct95 = eh_bildu, 
                canarian_coalition_pct95 = canarian_coalition, vox_pct95 = vox)

## Votes by province
province_vote_simulations <- province_votesims %>%
  lapply(t) %>%
  lapply(as.data.frame) %>%
  lapply(as.tbl)

for(i in 1:52) {
  province_vote_simulations[[i]] <- province_vote_simulations[[i]] %>%
    mutate(sim_number = 1:n(),
           community = provinces_ordered$community[i],
           community_name = provinces_ordered$community_name[i],
           province = provinces_ordered$province[i],
           province_name = provinces_ordered$province_name[i])
}

province_vote_simulation_tbl <- bind_rows(province_vote_simulations) %>%
  dplyr::select(community, province, community_name, province_name, sim_number, everything()) %>%
  melt(id.vars = c("community", "province", "community_name", "province_name", "sim_number"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

province_means <- province_vote_simulation_tbl %>%
  group_by(party, province_name) %>%
  summarise(mean_vote = mean(pct, na.rm = TRUE)) %>%
  spread(party, mean_vote) %>%
  dplyr::select(province_name, pp_vote_mean = pp, psoe_vote_mean = psoe, up_vote_mean = up, ciudadanos_vote_mean = ciudadanos, 
                catalan_republican_vote_mean = catalan_republican, junts_catalunya_vote_mean = junts_catalunya,
                basque_nationalist_vote_mean = basque_nationalist, eh_bildu_vote_mean = eh_bildu, 
                canarian_coalition_vote_mean = canarian_coalition, vox_vote_mean = vox)

province_pct5 <- province_vote_simulation_tbl %>%
  group_by(party, province_name) %>%
  summarise(pct5_vote = quantile(pct, 0.05, na.rm = TRUE)) %>%
  spread(party, pct5_vote) %>%
  dplyr::select(province_name, pp_vote_pct5 = pp, psoe_vote_pct5 = psoe, up_vote_pct5 = up, ciudadanos_vote_pct5 = ciudadanos, 
                catalan_republican_vote_pct5 = catalan_republican, junts_catalunya_vote_pct5 = junts_catalunya,
                basque_nationalist_vote_pct5 = basque_nationalist, eh_bildu_vote_pct5 = eh_bildu, 
                canarian_coalition_vote_pct5 = canarian_coalition, vox_vote_pct5 = vox)

province_pct95 <- province_vote_simulation_tbl %>%
  group_by(party, province_name) %>%
  summarise(pct95_vote = quantile(pct, 0.95, na.rm = TRUE)) %>%
  spread(party, pct95_vote) %>%
  dplyr::select(province_name, pp_vote_pct95 = pp, psoe_vote_pct95 = psoe, up_vote_pct95 = up, ciudadanos_vote_pct95 = ciudadanos, 
                catalan_republican_vote_pct95 = catalan_republican, junts_catalunya_vote_pct95 = junts_catalunya,
                basque_nationalist_vote_pct95 = basque_nationalist, eh_bildu_vote_pct95 = eh_bildu, 
                canarian_coalition_vote_pct95 = canarian_coalition, vox_vote_pct95 = vox)

