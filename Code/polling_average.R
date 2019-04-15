source("Code/poll_scrape_clean.R")

## Average
# Logit scale
poll_avg.logit <- polls_2019.logit_long %>%
  mutate(weight = 100*(age < 45)/(exp(age^0.3)*sqrt(sqrt(0.25/n))*sqrt(abs(date_spread - 5) + 1))) %>%
  group_by(party) %>%
  summarise(avg = wtd.mean(pct, weights = weight),
            sd = sqrt(wtd.var(pct, weights = weight))) %>%
  mutate(upper = avg + 1.677*sd,
         lower = avg - 1.677*sd) %>%
  mutate_at(vars(c("avg", "upper", "lower")), invlogit) %>%
  dplyr::select(party, avg, upper, lower)

poll_avg.logit

# % scale
poll_avg <- polls_2019.long %>%
  mutate(weight = 10*(age < 45)/(exp(age^0.4)*sqrt(sqrt(0.25/n))*sqrt(abs(date_spread - 5) + 1))) %>%
  group_by(party) %>%
  summarise(avg = wtd.mean(pct, weights = weight),
            sd = sqrt(wtd.var(pct, weights = weight))) %>%
  mutate(upper = 100*poll_avg.logit$upper,
         lower = 100*poll_avg.logit$lower)

## Plots
# Current average
poll_avg %>%
  ggplot(aes(x = party, fill = party)) +
  geom_col(aes(y = avg)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "darkgray") +
  geom_text(aes(y = avg + 0.6, label = round(avg, 1)), size = 3) +
  scale_fill_manual(name = "Candidate / Party", values = party_colors, labels = party_labels) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "2019 Spanish general election polling", y = "%", caption = "Error bars indicate 90% CI",
       subtitle = paste0(month(today(), label =  TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

# Over time
polls_2019.long %>%
  filter(party %in% names(major_party_labels)) %>%
  ggplot(aes(x = median_date, y = pct, col = party)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(span = 1/3) +
  scale_colour_manual(name = "Candidate / Party", values = party_colors, labels = party_labels) +
  scale_x_date(date_breaks = "months", limits = c(as.Date("2018-10-01"), as.Date("2019-04-28")), date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "2019 Spanish general election polling", x = "Date", y = "%",
       subtitle = paste0("October 1, 2018 - ", month(today(), label =  TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))
