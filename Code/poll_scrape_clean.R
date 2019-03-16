source("Code/library.R")

#### Scraping Wikipedia page for national polls ####
polls_url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Spanish_general_election"

## Get the Wikipedia polls page
polls_GET <- httr::GET(polls_url) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html()

## Parse into data frame
polls_2019.dirty <- rvest::html_nodes(polls_GET, css = "table")[1] %>%
  
  # Parse as a table
  rvest::html_table(fill = TRUE) %>%
  
  # Convert to tibble
  as.data.frame() %>%
  as.tbl() %>%
  
  # Clear out non-data rows
  slice(-1) %>%
  filter(Fieldwork.date != "", Polling.firm.Commissioner != "2016 general election")

names(polls_2019.dirty) <- c("pollster", "dates", "n", "projected_turnout", "pp", "psoe", "up", "ciudadanos", "catalan_republican", "catalan_european_democrat",
                             "basque_nationalist", "animalist", "eh_bildu", "canarian_coalition", "vox", "compromis", "lead")

## Parse fieldwork dates
poll_dates <- polls_2019.dirty$dates %>%
  str_split("[[:punct:]]|[[:space:]]")

single_day <- which(sapply(poll_dates, length) == 3)
single_month <- which(sapply(poll_dates, length) == 4)

first_month_dec <- poll_dates %>%
  sapply(function(x) return(x[2] == "Dec"))

multi_month <- which(sapply(poll_dates, length) == 5 & !first_month_dec)
multi_year <- which(sapply(poll_dates, length) == 5 & first_month_dec)

start_dates <- end_dates <- rep(NA, nrow(polls_2019.dirty))
start_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
start_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[1], x[3], x[4]))
start_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[1], x[2], x[5]))
start_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[1], x[2], as.numeric(x[5]) - 1))

end_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
end_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[2], x[3], x[4]))
end_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[3], x[4], x[5]))
end_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[3], x[4], x[5]))

start_dates <- unlist(start_dates) %>% as.Date(format = "%e %b %Y")
end_dates <- unlist(end_dates) %>% as.Date(format = "%e %b %Y")

## Clean up
party_vars <- c("pp", "psoe", "up", "ciudadanos", "catalan_republican", "catalan_european_democrat", "basque_nationalist",
                "eh_bildu", "canarian_coalition", "vox")

polls_2019 <- polls_2019.dirty %>%
  mutate(pollster = gsub("\\[.*?\\]", "", pollster),
         start_date = start_dates,
         end_date = end_dates,
         median_date = round(as.numeric(end_date - start_date)/2) + start_date,
         age = as.numeric(today() - median_date), 
         date_spread = as.numeric(end_date - start_date) + 1,
         n = gsub(",", "", n),
         n = as.numeric(n),
         n = case_when(is.na(n) ~ min(n, na.rm = TRUE),
                       !is.na(n) ~ n)) %>%
  mutate_at(vars(party_vars), function(x) gsub("/", "", x)) %>%
  mutate_at(vars(party_vars), as.numeric) %>%
  mutate_at(vars(party_vars), function(x) floor(10*x)/10) %>%
  slice(n():1) %>%
  distinct(dates, n, .keep_all = TRUE) %>%
  dplyr::select(pollster, median_date, date_spread, age, n, party_vars) %>%
  arrange(age) %>%
  mutate(weight = 100*(age < 60)/(exp(age^0.3)*sqrt(sqrt(0.25/n))*sqrt(abs(date_spread - 5) + 1))) 

## To long (as well as on logit scale)
polls_2019.long <- polls_2019 %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n", "weight"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

polls_2019.logit <- polls_2019 %>%
  mutate_at(vars(party_vars), function(x) logit(x/100))

polls_2019.logit_long <- polls_2019.logit %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n", "weight"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

#### The same but for 2016 ####
polls_url.old <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2016_Spanish_general_election"

polls_GET.old <- httr::GET(polls_url.old) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html()

## Parse into data frame
polls_2016.dirty <- rvest::html_nodes(polls_GET.old, css = "table")[1] %>%
  
  # Parse as a table
  rvest::html_table(fill = TRUE) %>%
  
  # Convert to tibble
  as.data.frame() %>%
  as.tbl() %>%
  
  # Clear out non-data rows
  slice(-1) %>%
  filter(Fieldwork.date != "", Polling.firm.Commissioner != "2016 general election")

names(polls_2016.dirty) <- c("pollster", "dates", "n", "projected_turnout", "pp", "psoe", "podemos", "ciudadanos", "iu", "catalan_republican", 
                             "democracy_freedom", "basque_nationalist", "animalist", "eh_bildu", "canarian_coalition", "galician_nationalist",
                             "up", "lead")

## Parse fieldwork dates
poll_dates <- polls_2016.dirty$dates %>%
  str_split("[[:punct:]]|[[:space:]]")

single_day <- which(sapply(poll_dates, length) == 3)
single_month <- which(sapply(poll_dates, length) == 4)

first_month_dec <- poll_dates %>%
  sapply(function(x) return(x[2] == "Dec"))

multi_month <- which(sapply(poll_dates, length) == 5 & !first_month_dec)
multi_year <- which(sapply(poll_dates, length) == 5 & first_month_dec)

start_dates <- end_dates <- rep(NA, nrow(polls_2016.dirty))
start_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
start_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[1], x[3], x[4]))
start_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[1], x[2], x[5]))
start_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[1], x[2], as.numeric(x[5]) - 1))

end_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
end_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[2], x[3], x[4]))
end_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[3], x[4], x[5]))
end_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[3], x[4], x[5]))

start_dates <- unlist(start_dates) %>% as.Date(format = "%e %b %Y")
end_dates <- unlist(end_dates) %>% as.Date(format = "%e %b %Y")

## Clean up
party_vars <- c("pp", "psoe", "ciudadanos", "catalan_republican", "democracy_freedom", "basque_nationalist", 
                "animalist", "eh_bildu", "canarian_coalition", "galician_nationalist", "up", "lead")

polls_2016 <- polls_2016.dirty %>%
  mutate(pollster = gsub("\\[.*?\\]", "", pollster),
         start_date = start_dates,
         end_date = end_dates,
         median_date = round(as.numeric(end_date - start_date)/2) + start_date,
         age = as.numeric(as.Date("2016-06-26") - median_date), 
         date_spread = as.numeric(end_date - start_date) + 1,
         n = gsub(",", "", n),
         n = as.numeric(n),
         n = case_when(is.na(n) ~ min(n, na.rm = TRUE),
                       !is.na(n) ~ n)) %>%
  mutate_at(vars(party_vars), function(x) gsub("/", "", x)) %>%
  mutate_at(vars(party_vars), as.numeric) %>%
  mutate_at(vars(party_vars), function(x) floor(10*x)/10) %>%
  slice(n():1) %>%
  distinct(dates, n, .keep_all = TRUE) %>%
  dplyr::select(pollster, median_date, date_spread, age, n, party_vars) %>%
  arrange(age) %>%
  filter(!is.na(pp), median_date < as.Date("2016-06-26"))

## To long (as well as on logit scale)
polls_2016.long <- polls_2016 %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

polls_2016.logit <- polls_2016 %>%
  mutate_at(vars(party_vars), function(x) logit(x/100))

polls_2016.logit_long <- polls_2016.logit %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

#### 2015 ####
polls_url.old <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_Spanish_general_election"

polls_GET.old <- httr::GET(polls_url.old) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html()

## Parse into data frame
polls_2015.dirty <- rvest::html_nodes(polls_GET.old, css = "table")[1] %>%
  
  # Parse as a table
  rvest::html_table(fill = TRUE) %>%
  
  # Convert to tibble
  as.data.frame() %>%
  as.tbl() %>%
  
  # Clear out non-data rows
  slice(-1) %>%
  filter(Fieldwork.date != "", Polling.firm.Commissioner != "2016 general election")

names(polls_2015.dirty) <- c("pollster", "dates", "n", "projected_turnout", "pp", "psoe", "iu_up", "upyd", "ciu", "eh_bildu", "basque_nationalist", 
                             "catalan_republican", "galician_nationalist", "canarian_coalition", "compromis", "ciudadanos", "podemos", 
                             "democracy_freedom", "lead")

## Parse fieldwork dates
poll_dates <- polls_2015.dirty$dates %>%
  str_split("[[:punct:]]|[[:space:]]")

single_day <- which(sapply(poll_dates, length) == 3)
single_month <- which(sapply(poll_dates, length) == 4)

first_month_dec <- poll_dates %>%
  sapply(function(x) return(x[2] == "Dec"))

multi_month <- which(sapply(poll_dates, length) == 5 & !first_month_dec)
multi_year <- which(sapply(poll_dates, length) == 5 & first_month_dec)

start_dates <- end_dates <- rep(NA, nrow(polls_2015.dirty))
start_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
start_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[1], x[3], x[4]))
start_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[1], x[2], x[5]))
start_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[1], x[2], as.numeric(x[5]) - 1))

end_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
end_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[2], x[3], x[4]))
end_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[3], x[4], x[5]))
end_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[3], x[4], x[5]))

start_dates <- unlist(start_dates) %>% as.Date(format = "%e %b %Y")
end_dates <- unlist(end_dates) %>% as.Date(format = "%e %b %Y")

## Clean up
party_vars <- c("pp", "psoe", "iu_up", "upyd", "ciu", "eh_bildu", "basque_nationalist", 
                "catalan_republican", "galician_nationalist", "canarian_coalition", "compromis", "ciudadanos", "podemos", 
                "democracy_freedom")

polls_2015 <- polls_2015.dirty %>%
  mutate(pollster = gsub("\\[.*?\\]", "", pollster),
         start_date = start_dates,
         end_date = end_dates,
         median_date = round(as.numeric(end_date - start_date)/2) + start_date,
         age = as.numeric(as.Date("2015-12-20") - median_date), 
         date_spread = as.numeric(end_date - start_date) + 1,
         n = gsub(",", "", n),
         n = as.numeric(n),
         n = case_when(is.na(n) ~ min(n, na.rm = TRUE),
                       !is.na(n) ~ n)) %>%
  mutate_at(vars(party_vars), function(x) gsub("/", "", x)) %>%
  mutate_at(vars(party_vars), as.numeric) %>%
  mutate_at(vars(party_vars), function(x) floor(10*x)/10) %>%
  slice(n():1) %>%
  distinct(dates, n, .keep_all = TRUE) %>%
  dplyr::select(pollster, median_date, date_spread, age, n, party_vars) %>%
  arrange(age) %>%
  filter(!is.na(pp), median_date < as.Date("2015-12-20"))

## To long (as well as on logit scale)
polls_2015.long <- polls_2015 %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

polls_2015.logit <- polls_2015 %>%
  mutate_at(vars(party_vars), function(x) logit(x/100))

polls_2015.logit_long <- polls_2015.logit %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

#### 2011 ####
polls_url.old <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2011_Spanish_general_election"

polls_GET.old <- httr::GET(polls_url.old) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html()

## Parse into data frame
polls_2011.dirty <- rvest::html_nodes(polls_GET.old, css = "table")[1] %>%
  
  # Parse as a table
  rvest::html_table(fill = TRUE) %>%
  
  # Convert to tibble
  as.data.frame() %>%
  as.tbl() %>%
  
  # Clear out non-data rows
  slice(-1) %>%
  filter(Fieldwork.date != "", Polling.firm.Commissioner != "2016 general election")

names(polls_2011.dirty) <- c("pollster", "dates", "n", "projected_turnout", "psoe", "pp", "iu", "ciu", "basque_nationalist", "upyd",
                             "catalan_republican", "galician_nationalist", "canarian_coalition", "amaiur", "lead")

## Parse fieldwork dates
poll_dates <- polls_2011.dirty$dates %>%
  str_split("[[:punct:]]|[[:space:]]")

single_day <- which(sapply(poll_dates, length) == 3)
single_month <- which(sapply(poll_dates, length) == 4)

first_month_dec <- poll_dates %>%
  sapply(function(x) return(x[2] == "Dec"))

multi_month <- which(sapply(poll_dates, length) == 5 & !first_month_dec)
multi_year <- which(sapply(poll_dates, length) == 5 & first_month_dec)

start_dates <- end_dates <- rep(NA, nrow(polls_2011.dirty))
start_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
start_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[1], x[3], x[4]))
start_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[1], x[2], x[5]))
start_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[1], x[2], as.numeric(x[5]) - 1))

end_dates[single_day] <- poll_dates[single_day] %>% lapply(function(x) paste(x[1], x[2], x[3]))
end_dates[single_month] <- poll_dates[single_month] %>% lapply(function(x) paste(x[2], x[3], x[4]))
end_dates[multi_month] <- poll_dates[multi_month] %>% lapply(function(x) paste(x[3], x[4], x[5]))
end_dates[multi_year] <- poll_dates[multi_year] %>% lapply(function(x) paste(x[3], x[4], x[5]))

start_dates <- unlist(start_dates) %>% as.Date(format = "%e %b %Y")
end_dates <- unlist(end_dates) %>% as.Date(format = "%e %b %Y")

## Clean up
party_vars <- c("projected_turnout", "psoe", "pp", "iu", "ciu", "basque_nationalist", "upyd",
                "catalan_republican", "galician_nationalist", "canarian_coalition", "amaiur")

polls_2011 <- polls_2011.dirty %>%
  mutate(pollster = gsub("\\[.*?\\]", "", pollster),
         start_date = start_dates,
         end_date = end_dates,
         median_date = round(as.numeric(end_date - start_date)/2) + start_date,
         age = as.numeric(as.Date("2011-11-20") - median_date), 
         date_spread = as.numeric(end_date - start_date) + 1,
         n = gsub(",", "", n),
         n = as.numeric(n),
         n = case_when(is.na(n) ~ min(n, na.rm = TRUE),
                       !is.na(n) ~ n)) %>%
  mutate_at(vars(party_vars), function(x) gsub("/", "", x)) %>%
  mutate_at(vars(party_vars), as.numeric) %>%
  mutate_at(vars(party_vars), function(x) floor(10*x)/10) %>%
  slice(n():1) %>%
  distinct(dates, n, .keep_all = TRUE) %>%
  dplyr::select(pollster, median_date, date_spread, age, n, party_vars) %>%
  arrange(age) %>%
  filter(!is.na(pp), median_date < as.Date("2011-11-15"))

## To long (as well as on logit scale)
polls_2011.long <- polls_2011 %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()

polls_2011.logit <- polls_2011 %>%
  mutate_at(vars(party_vars), function(x) logit(x/100))

polls_2011.logit_long <- polls_2011.logit %>%
  melt(id.vars = c("pollster", "median_date", "date_spread", "age", "n"),
       variable.name = "party", value.name = "pct") %>%
  as.tbl()