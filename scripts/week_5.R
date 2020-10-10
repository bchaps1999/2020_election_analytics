library(tidyverse)

states <- state.name

# Poll data

polls <- read_csv("../data/pollavg_bystate_1968-2016.csv") %>% 
  filter(year == 2016)

results <- read_csv("../data/popvote_bystate_1948-2016.csv") %>% 
  filter(year == 2016) %>%
  select(state, R_pv2p, D_pv2p) %>% 
  rename(Republican = R_pv2p,
         Democratic = D_pv2p) %>% 
  pivot_longer(cols = c(Republican, Democratic), names_to = "party", values_to = "pv2p")

polls_2020 <- read_csv("../data/state_polls_2020.csv") %>% 
  pivot_wider(names_from = party, values_from = avg_poll) %>% 
  mutate(democrat_2p = 100 * (Democratic)/(Democratic + Republican),
         republican_2p = 100 * (Republican)/(Democratic + Republican)) %>% 
  select(state, democrat_2p, republican_2p) %>% 
  ungroup() %>% 
  rename(Democratic = democrat_2p, Republican = republican_2p) %>%    
  pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "avg_poll_2p")

polls_2016 <- polls %>% 
  filter(days_left >= 30) %>% 
  group_by(state, party) %>%  
  filter(days_left == min(days_left)) %>% 
  select(state, party, avg_poll) %>% 
  pivot_wider(names_from = party, values_from = avg_poll) %>% 
  mutate(democrat_2p = 100 * (democrat)/(democrat + republican),
         republican_2p = 100 * (republican)/(democrat + republican)) %>% 
  select(state, democrat_2p, republican_2p) %>% 
  ungroup() %>% 
  rename(Democratic = democrat_2p, Republican = republican_2p) %>%    
  pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "avg_poll_2p") %>% 
  left_join(results, by = c("state","party")) %>% 
  mutate(error = avg_poll_2p - pv2p)

# Market data

population <- read_csv("../data/population.csv")

turnout <- read.csv("../data/turnout_2016.csv", header = FALSE) %>% 
  rename(state = V1, perc = V2) %>% 
  mutate(perc = as.numeric(gsub("[\\%,]", "", perc)),
         state = as.character(state))

state_voters <- population %>% 
  left_join(turnout, by = "state") %>% 
  mutate(voters = population * (perc/100))

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
  filter(date == as.Date("10/14/2016", "%m/%d/%Y"),
         party != "Libertarian") %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price")

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

market_vote_2020 <- market_data_2020 %>% 
  filter(date == as.Date("10/09/2020", "%m/%d/%Y"),
         party != "Libertarian") %>% 
  pivot_longer(cols = c(everything(),-party,-date), names_to = "state", values_to = "price")

# Combining poll and market data

polls_markets <- market_vote_2016 %>% 
  pivot_wider(names_from = party, values_from = price) %>%
  mutate(dem_price_2p = (Democratic)/(Democratic + Republican),
         rep_price_2p = (Republican)/(Democratic + Republican)) %>%
  select(state, dem_price_2p, rep_price_2p) %>%
  rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
  pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "price") %>%
  left_join(polls_2016, by = c("state", "party"))

rep_polls_markets <- polls_markets %>% 
  filter(party == "Republican")

dem_polls_markets <- polls_markets %>% 
  filter(party == "Democratic")

polls_markets_2020 <- market_vote_2020 %>% 
  pivot_wider(names_from = party, values_from = price) %>%
  mutate(dem_price_2p = (Democratic)/(Democratic + Republican),
         rep_price_2p = (Republican)/(Democratic + Republican)) %>%
  select(state, dem_price_2p, rep_price_2p) %>%
  rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
  pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "price") %>%
  left_join(polls_2020, by = c("state", "party"))

rep_polls_markets_2020 <- polls_markets_2020 %>% 
  filter(party == "Republican")

dem_polls_markets_2020 <- polls_markets_2020 %>% 
  filter(party == "Democratic")

# Poll model

poll_model <- lm(pv2p ~ avg_poll_2p, data = rep_polls_markets)

summary(poll_model)

resid(poll_model) %>% 
  as_tibble() %>% 
  summarize(RMSE = sqrt(mean(value*value)))

poll_outsamp_errors <- sapply(1:1000, function(i){
  train_ind <- sample(seq_len(nrow(rep_polls_markets)), 
                      size = floor(0.75 * nrow(rep_polls_markets)))
  train <- rep_polls_markets[train_ind, ]
  test <- rep_polls_markets[-train_ind, ]
  outsamp_mod <- lm(pv2p ~ avg_poll_2p, data = train)
  predict(outsamp_mod, newdata = test) %>%
    as_tibble() %>% 
    bind_cols(test) %>% 
    mutate(error = value - pv2p) %>% 
    summarize(mean_error = sqrt(mean(error*error))) %>% 
    pull()
}) %>% 
  as_tibble()

poll_outsamp_errors %>% 
  summarize(mean(value))

# Market model

market_model <- lm(pv2p ~ poly(price,3), data = rep_polls_markets)

summary(market_model)

resid(market_model) %>% 
  as_tibble() %>% 
  summarize(RMSE = sqrt(mean(value*value)))

market_outsamp_errors <- sapply(1:1000, function(i){
  train_ind <- sample(seq_len(nrow(rep_polls_markets)), 
                      size = floor(0.75 * nrow(rep_polls_markets)))
  train <- rep_polls_markets[train_ind, ]
  test <- rep_polls_markets[-train_ind, ]
  outsamp_mod <- lm(pv2p ~ poly(price,3), data = train)
  predict(outsamp_mod, newdata = test) %>%
    as_tibble() %>% 
    bind_cols(test) %>% 
    mutate(error = value - pv2p) %>% 
    summarize(mean_error = sqrt(mean(error*error))) %>% 
    pull()
}) %>% 
  as_tibble()

market_outsamp_errors %>% 
  summarize(mean(value))

# Poll and market model

poll_market_model <- lm(pv2p ~ avg_poll_2p + poly(price,3), data = rep_polls_markets)

summary(poll_market_model)

resid(poll_market_model) %>% 
  as_tibble() %>% 
  summarize(RMSE = sqrt(mean(value*value)))

combined_outsamp_errors <- sapply(1:1000, function(i){
  train_ind <- sample(seq_len(nrow(rep_polls_markets)), 
                      size = floor(0.75 * nrow(rep_polls_markets)))
  train <- rep_polls_markets[train_ind, ]
  test <- rep_polls_markets[-train_ind, ]
  outsamp_mod <- lm(pv2p ~ avg_poll_2p + poly(price,3), data = train)
  predict(outsamp_mod, newdata = test) %>%
    as_tibble() %>% 
    bind_cols(test) %>% 
    mutate(error = value - pv2p) %>% 
    summarize(mean_error = sqrt(mean(error*error))) %>% 
    pull()
}) %>% 
  as_tibble()

combined_outsamp_errors %>% 
  summarize(mean(value))

# 2020 Predictions

dem_poll_market_model <- lm(pv2p ~ poly(price,3) + avg_poll_2p, data = dem_polls_markets)
rep_poll_market_model <- lm(pv2p ~ poly(price,3) + avg_poll_2p, data = rep_polls_markets)

predict(dem_poll_market_model, newdata = dem_polls_markets_2020) %>% 
  as_tibble() %>% 
  bind_cols(dem_polls_markets_2020) %>% 
  left_join(state_voters, by = c("state")) %>% 
  mutate(votes = voters*(value/100)) %>% 
  summarize(pv2p = sum(votes)/sum(voters))

predict(rep_poll_market_model, newdata = rep_polls_markets_2020) %>% 
  as_tibble() %>% 
  bind_cols(rep_polls_markets_2020) %>% 
  left_join(state_voters, by = c("state")) %>% 
  mutate(votes = voters*(value/100)) %>% 
  summarize(pv2p = sum(votes)/sum(voters))

# Plots

rep_polls_markets %>% 
  ggplot(aes(x = avg_poll_2p, y = pv2p, color = price)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_minimal() + 
  labs(title = "Average Poll and Vote Percent by State in 2016 Election - Trump",
       x = "Average Two-Party Poll (25 Days Before Election)",
       y = "Percent of Two-Party Popular Vote in Election",
       color = "PredictIt\nMarket\nPrice") +
  ggsave(filename = "poll_regression.png")

predict(poll_market_model, newdata = rep_polls_markets) %>% 
  as_tibble() %>% 
  bind_cols(rep_polls_markets) %>% 
  ggplot(aes(x = pv2p, y = value)) +
  geom_point() + 
  geom_abline() + 
  theme_minimal() + 
  labs(title = "Predicted and Real Vote Percents by State in 2016",
       x = "Percent of Two-Party Popular Vote for Donald Trump",
       y = "Predicted Vote Percent - Multivariate Regression") + 
  ggsave(filename = "multiple_regression.png")
