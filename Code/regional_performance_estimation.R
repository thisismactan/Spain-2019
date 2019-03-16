## Regional party performance: national vs. regional
regional_performance <- read_csv("Data/regional_party_results.csv")
regional_regions <- unique(regional_performance$region)
regional_model_list <- vector("list", length(regional_regions))

for(i in 1:length(regional_regions)) {
  regional_model_list[[i]] <- lm(regional_vote~national_vote, 
                                 data = regional_performance %>%
                                   filter(region == regional_regions[i]))
}

lapply(regional_model_list, summary)
