library(tidyverse)

state_polls <- read_csv("../data/polls_2020.csv")
population <- read_csv("../data/population.csv")

states_map <- map_data("state")
unique(states_map$region)

polls <- state_polls %>% 
  left_join(population, by = c("state")) %>% 
  mutate(trump_votes = trump_poll * population,
         biden_votes = biden_poll * population,
         trump_2p = trump_votes/(trump_votes + biden_votes),
         trump_margin = 100* (2 * trump_2p - 1))

polls %>% 
  summarize(trump_total = sum(trump_votes)/sum(population),
            biden_total = sum(biden_votes)/sum(population))

polls$region <- tolower(polls$state)

polls %>% 
  left_join(states_map, by = "region") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = trump_margin), color = "black") +
  scale_fill_gradient2(
    high = "red",
    mid = "white",
    low = "blue",
    midpoint = 0,
    name = "Percentage
    Points"
  ) +
  labs(title = "Poll Margin for President Trump by State") + 
  theme_void() +
  ggsave(filename = "poll_margin.jpg")
