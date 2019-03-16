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

## Simulations
set.seed(2019)

# National popular vote
n.iter <- 10000
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
simulated_swings[9,] <- predict(regional_model_list[[1]], newdata = data.frame(national_vote = simulated_swings[9,]))
simulated_swings[5,] <- predict(regional_model_list[[2]], newdata = data.frame(national_vote = simulated_swings[5,]))
simulated_swings[6,] <- predict(regional_model_list[[2]], newdata = data.frame(national_vote = simulated_swings[6,]))
simulated_swings[7,] <- predict(regional_model_list[[3]], newdata = data.frame(national_vote = simulated_swings[7,]))
simulated_swings[8,] <- predict(regional_model_list[[3]], newdata = data.frame(national_vote = simulated_swings[8,]))

results_2016.wide <- results_2016 %>%
  dplyr::select(list, pct) %>%
  spread(list, pct) %>%
  ungroup() %>%
  mutate(catalan_european_democrat = `Converg&egrave;ncia Democr&agrave;tica de Catalunya (CDC)`) %>%
  dplyr::select(names(means)) %>%
  replace_na(list(pp = 0, psoe = 0, up = 0, ciudadanos = 0, catalan_republican = NA, catalan_european_democrat = NA, basque_nationalist = NA,
                  eh_bildu = NA, canarian_coalition = NA, vox = 0)) %>%
  as.matrix()

provincial_seats <- results_2016 %>%
  dplyr::select(list, pct) %>%
  spread(list, pct) %>%
  ungroup() %>%
  mutate(catalan_european_democrat = NA) %>%
  dplyr::select(province, names(means)) %>%
  left_join(province_key) %>%
  pull(total_seats)


# Regional simulations: distribute seats
province_sims <- rep(list(simulated_swings), 52)
province_seatsims <- vector("list", 52)

for(i in 1:52) {
  ## Distribute initial seats
  province_sims[[i]] <- pmax(province_sims[[i]] + results_2016.wide[i,], 0)
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

