library(tidyverse)
library(gganimate)
library(viridis)

popvote <- read_csv("data/popvote_bystate_1948-2016.csv")

states_map <- map_data("state")
unique(states_map$region)

popvote$region <- tolower(popvote$state)
  
vote_stats <- popvote %>%
  filter(!is.na(D_pv2p), year >= 1992) %>% 
  mutate(dem_win = ifelse(D_pv2p > 50, TRUE, FALSE),
         vote_margin = D_pv2p - R_pv2p) %>%
  group_by(region) %>% 
  summarize(average_margin = abs(mean(vote_margin)),
            percent_won = sum(dem_win)/n())

stats_2016 <- popvote %>%
  filter(!is.na(D_pv2p), year == 2016) %>% 
  mutate(dem_win = ifelse(D_pv2p > 50, TRUE, FALSE),
         vote_margin = D_pv2p - R_pv2p) %>%
  group_by(region) %>% 
  summarize(average_margin = abs(mean(vote_margin)),
            percent_won = sum(dem_win)/n())

vote_stats %>% 
  left_join(states_map, by = "region") %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = percent_won), color = "black") +
    scale_fill_gradient(
      low = "red",
      high = "blue",
      name = "Percent"
    ) +
    labs(title = "Percent of Elections Won by Democrats Since 1992") + 
    theme_void()
  
vote_stats %>% 
  mutate(average_margin = abs(average_margin)) %>% 
  left_join(states_map, by = "region") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = average_margin), color = "black") +
  scale_fill_viridis(
option = "plasma", limits = c(0, 35), name = "Vote Margin"
  ) +
  labs(title = "Average Percentage Point Difference Between Party Vote Shares Since 1992") + 
  theme_void()

stats_2016 %>% 
  left_join(states_map, by = "region") %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = average_margin), color = "black") +
  scale_fill_viridis(
    option = "plasma", limits = c(0, 60), name = "Vote Margin"
  ) +
  labs(title = "Percentage Point Difference Between Party Vote Shares in 2016") + 
  theme_void() + 
  ggsave(filename = "2016_margin.jpg")
