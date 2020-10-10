library(tidyverse)
library(datasets)
library(lubridate)

population <- read_csv("../data/population.csv")

turnout <- read.csv("../data/turnout_2016.csv", header = FALSE) %>% 
  rename(state = V1, perc = V2) %>% 
  mutate(perc = as.numeric(gsub("[\\%,]", "", perc)),
         state = as.character(state))

state_voters <- population %>% 
  left_join(turnout, by = "state") %>% 
  mutate(voters = population * (perc/100))

states <- state.name

states_map <- map_data("state")
unique(states_map$region)

# 2016

state_vote <- read_csv("../data/popvote_bystate_1948-2016.csv") %>% 
  filter(year == 2016) %>% 
  select(state, R_pv2p, D_pv2p) %>% 
  rename(Democratic = D_pv2p,
         Republican = R_pv2p) %>% 
  pivot_longer(cols = c(Republican, Democratic), names_to = "party", values_to = "pv2p")

market_data_2016 <- NULL

market_data_2016 <- read_csv(file = "../data/prediction_market/Alabama.csv") %>% 
  select(ContractName, Date, CloseSharePrice) %>%
  rename(
    party = ContractName,
    date = Date,
    Alabama = CloseSharePrice
  ) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(Alabama = as.numeric(gsub("\\$", "", Alabama)))

add_market_data <- function(state_name){
    
  file_name = paste("../data/prediction_market/", state_name,".csv",sep=
            "")
  
  state_data <- read_csv(file = file_name) %>% 
    select(ContractName, Date, CloseSharePrice) %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>% 
    mutate(CloseSharePrice = as.numeric(gsub("\\$", "", CloseSharePrice)))
  
  colnames(state_data) = c("party", "date", state_name)
  
  market_data_2016 <- market_data_2016 %>%
    full_join(state_data, by = c("party", "date"))
  
}

states <- states %>% 
  as_tibble() %>% 
  filter(value != "Louisiana",
         value != "Rhode Island", 
         value != "Washington",
         value != "New Jersey",
         value != "South Carolina",
         value != "South Dakota",
         value != "Alabama",
         !is.na(value)) %>% 
  pull(value)

for (k in 1:43) {
  
  state_k = states[k]
 
  market_data_2016 <- add_market_data(state_k)
   
}

market_vote_2016 <- market_data_2016 %>% 
  filter(date == as.Date("10/1/2016", "%m/%d/%Y"),
         party != "Libertarian") %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price") %>% 
  left_join(state_vote, by = c("state","party")) %>% 
  mutate(win = ifelse(pv2p > 50, 1, 0),
         predicted_win = ifelse(price > 0.50, 1, 0),
         predicted_correct = ifelse(win == 1 & predicted_win == 1 | win == 0 & predicted_win == 0, 1, 0))

market_vote_2016 %>% 
  filter(party == "Democratic") %>% 
  summarize(sum(predicted_correct)/n())

market_vote_2016 %>% 
  filter(party == "Democratic") %>% 
  ggplot(aes(x = price, y = pv2p)) + 
  geom_point() +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 3, raw = TRUE)) + 
  labs(x = "Closing Market Price on Nov 7, 2016",
       y = "Two-Party Popular Vote Percent for Clinton",
       title = "PredictIt Market Price and Final Vote Percent by State")+
  ggsave("../figures/predictit_model.png")

model <- lm(pv2p ~ poly(price,3), data = market_vote_2016)

summary(model)

resid(model) %>% 
  as_tibble() %>% 
  summarize(mean(abs(value)))

new_data <- market_data_2016 %>% 
  filter(date == as.Date("10/1/2016", "%m/%d/%Y")) %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price") %>% 
  filter(party == "Republican") %>% 
  select(price)

predictions_2016 <- predict(model, new_data) %>% 
  as_tibble()

market_data_2016 %>% 
  filter(date == as.Date("10/1/2016", "%m/%d/%Y")) %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price") %>% 
  filter(party == "Republican") %>% 
  bind_cols(., predictions_2016) %>% 
  rename(expected_vote_trump = value) %>% 
  mutate(expected_vote_clinton = 100 - expected_vote_trump) %>% 
  left_join(state_voters, by = "state") %>% 
  mutate(clinton_voters = expected_vote_clinton/100 * voters,
         trump_voters = expected_vote_trump/100 * voters) %>% 
  summarize(clinton = sum(clinton_voters)/sum(voters),
            trump = sum(trump_voters)/sum(voters))

# 2020

market_data_2020 <- NULL

market_data_2020 <- read_csv(file = "../data/prediction_market/2020/Alabama.csv") %>% 
  select(ContractName, Date, CloseSharePrice) %>%
  rename(
    party = ContractName,
    date = Date,
    Alabama = CloseSharePrice
  ) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(Alabama = as.numeric(gsub("\\$", "", Alabama)))

add_market_data <- function(state_name){
  
  file_name = paste("../data/prediction_market/2020/", state_name,".csv",sep=
                      "")
  
  state_data <- read_csv(file = file_name) %>% 
    select(ContractName, Date, CloseSharePrice) %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>% 
    mutate(CloseSharePrice = as.numeric(gsub("\\$", "", CloseSharePrice)))
  
  colnames(state_data) = c("party", "date", state_name)
  
  market_data_2020 <- market_data_2020 %>%
    full_join(state_data, by = c("party", "date"))
  
}

states <- state.name %>% 
  as_tibble() %>% 
  filter(value != "Alabama") %>% 
  pull()

for (k in 1:49) {
  
  state_k = states[k]
  
  market_data_2020 <- add_market_data(state_k)
  
}

new_data <- market_data_2020 %>% 
  filter(date == as.Date("10/1/2020", "%m/%d/%Y")) %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price") %>% 
  filter(party == "Republican") %>% 
  select(price)

predictions_2020 <- predict(model, new_data) %>% 
  as_tibble()

state_predictions <- market_data_2020 %>% 
  filter(date == as.Date("10/1/2020", "%m/%d/%Y")) %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price") %>% 
  filter(party == "Republican") %>% 
  bind_cols(., predictions_2020) %>% 
  rename(expected_vote_trump = value) %>% 
  mutate(expected_vote_biden = 100 - expected_vote_trump,
         biden_win = ifelse(expected_vote_biden > 50, TRUE, FALSE)) %>% 
  select(-party)

state_predictions %>% 
  left_join(state_voters, by = "state") %>% 
  mutate(biden_voters = expected_vote_biden/100 * voters,
         trump_voters = expected_vote_trump/100 * voters) %>% 
  summarize(biden = sum(biden_voters)/sum(voters),
            trump = sum(trump_voters)/sum(voters))

state_predictions$region <- tolower(state_predictions$state)

state_predictions %>% 
  left_join(states_map, by = "region") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = biden_win), color = "black") +
  scale_fill_manual(values = c("red","blue"),
                    name = "Election Winner", labels = c("Trump", "Biden")) + 
  theme_void() +
  labs(title = "Predicted Winner of Two-Party Popular Vote by State - Model") +
  ggsave("../figures/predictit_model_map.png")

state_predictions %>% 
  left_join(states_map, by = "region") %>%
  mutate(predictit_biden_win = ifelse(price < 0.50, TRUE, FALSE)) %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = predictit_biden_win), color = "black") +
  scale_fill_manual(values = c("red","blue"),
                    name = "Election Winner", labels = c("Trump", "Biden")) + 
  theme_void() +
  labs(title = "Predicted Winner of Two-Party Popular Vote by State - PredictIt") + 
  ggsave("../figures/predictit_map.png")
  

