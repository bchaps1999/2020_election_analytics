library(tidyverse)
library(rvest)

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

polls_2020 <- read_csv("../data/presidential_poll_averages_2020.csv") %>% 
  filter(candidate_name == "Joseph R. Biden Jr." | candidate_name == "Donald Trump") %>% 
  mutate(party = ifelse(candidate_name == "Joseph R. Biden Jr.", "Democratic", "Republican"),
         date = as.Date(modeldate, "%m/%d/%Y")) %>% 
  filter(cycle == 2020) %>% 
  select(state, date, party, pct_estimate) %>% 
  pivot_wider(names_from = party, values_from = pct_estimate) %>% 
  mutate(democrat_2p = 100 * (Democratic)/(Democratic + Republican),
         republican_2p = 100 * (Republican)/(Democratic + Republican)) %>% 
  select(state, date, democrat_2p, republican_2p) %>%
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

# Electoral college data

electoral_html <- read_html("https://www.britannica.com/topic/United-States-Electoral-College-Votes-by-State-1787124")
electoral_college <- electoral_html %>%
  html_nodes("td , th") %>%
  html_text() %>% 
  matrix(ncol=2, byrow=TRUE) %>% 
  as_tibble() %>% 
  slice(4:54) %>% 
  rename(state = V1, ec_votes = V2) %>% 
  mutate(ec_votes = as.numeric(as.character(ec_votes)))


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
  filter(value != "New Jersey",
         value != "South Carolina",
         value != "Alabama",
         !is.na(value)) %>% 
  pull(value)

for (k in 1:47) {
  
  state_k = states[k]
  
  market_data_2016 <- add_market_data(state_k)
  
}

market_vote_2016 <- market_data_2016 %>% 
  filter(date == as.Date("11/7/2016", "%m/%d/%Y"),
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
  filter(date == as.Date("10/16/2020", "%m/%d/%Y"),
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

polls_today <- polls_2020 %>% 
  filter(date == as.Date("10/16/2020", "%m/%d/%Y"))

polls_markets_2020 <- market_vote_2020 %>% 
  pivot_wider(names_from = party, values_from = price) %>%
  mutate(dem_price_2p = (Democratic)/(Democratic + Republican),
         rep_price_2p = (Republican)/(Democratic + Republican)) %>%
  select(state, dem_price_2p, rep_price_2p) %>%
  rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
  pivot_longer(cols = c(Democratic, Republican), names_to = "party", values_to = "price") %>%
  left_join(polls_today, by = c("state", "party"))

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

dem_predictions <- predict(dem_poll_market_model, 
        newdata = dem_polls_markets_2020, 
        interval = "prediction") %>%  
  as_tibble() %>% 
  mutate(sd = (upr-lwr)/(1.96*2)) %>% 
  bind_cols(dem_polls_markets_2020) %>% 
  select(state, fit, sd) %>% 
  left_join(electoral_college, by = "state")

rep_predictions <- predict(rep_poll_market_model, 
                           newdata = rep_polls_markets_2020, 
                           interval = "prediction") %>%  
  as_tibble() %>% 
  mutate(sd = (upr-lwr)/(1.96*2)) %>% 
  bind_cols(rep_polls_markets_2020) %>% 
  select(state, fit, sd) %>% 
  left_join(electoral_college, by = "state")

dem_predictions %>% 
  left_join(state_voters, by = c("state")) %>% 
  mutate(votes = voters*(fit/100)) %>% 
  summarize(pv2p = sum(votes)/sum(voters))

rep_predictions %>% 
  left_join(state_voters, by = c("state")) %>% 
  mutate(votes = voters*(fit/100)) %>% 
  summarize(pv2p = sum(votes)/sum(voters))

# 2020 Probabilistic Predictions

num_states <- nrow(dem_predictions)

vector_length = 50000

dem_state_predicted <- data.frame(state = vector("character", length = vector_length),
                              prediction = vector("numeric", length = vector_length),
                              rep = vector("numeric", length = vector_length),
                              ec_votes = vector("numeric", length = vector_length),
                              stringsAsFactors = FALSE)

rep_state_predicted <- data.frame(state = vector("character", length = vector_length),
                                  prediction = vector("numeric", length = vector_length),
                                  rep = vector("numeric", length = vector_length),
                                  ec_votes = vector("numeric", length = vector_length),
                                  stringsAsFactors = FALSE)

set.seed(2020)

for (i in 1:1000) {
  
  for (j in 1:num_states) {
    
    k = (i - 1) * 50 + j
    state = dem_predictions$state[j]
    vote = dem_predictions$fit[j]
    vote_sd = dem_predictions$sd[j]
    ec_votes = dem_predictions$ec_votes[j]
    dem_state_distribution = rnorm(n = 1000, mean = vote, sd = vote_sd)
    dem_state_predicted$prediction[k] = sample(dem_state_distribution, size = 1)
    dem_state_predicted$rep[k] = i
    dem_state_predicted$state[k] = state
    dem_state_predicted$ec_votes[k] = ec_votes
    
    state = rep_predictions$state[j]
    vote = rep_predictions$fit[j]
    vote_sd = rep_predictions$sd[j]
    ec_votes = rep_predictions$ec_votes[j]
    rep_state_distribution = rnorm(n = 1000, mean = vote, sd = vote_sd)
    rep_state_predicted$prediction[k] = sample(rep_state_distribution, size = 1)
    rep_state_predicted$rep[k] = i
    rep_state_predicted$state[k] = state
    rep_state_predicted$ec_votes[k] = ec_votes
    
  }
  
}

dem_ec_predicted <- dem_state_predicted %>% 
  mutate(ec_votes = ifelse(prediction > 50, ec_votes, 0)) %>% 
  group_by(rep) %>% 
  summarize(national_ec_votes = sum(ec_votes) + 3)

rep_ec_predicted <- rep_state_predicted %>% 
  mutate(ec_votes = ifelse(prediction > 50, ec_votes, 0)) %>% 
  group_by(rep) %>% 
  summarize(national_ec_votes = sum(ec_votes))

dem_ec_predicted %>% 
  filter(national_ec_votes > 270) %>% 
  count() %>% 
  summarize(probability = n/1000)

rep_ec_predicted %>% 
  filter(national_ec_votes > 270) %>% 
  count() %>% 
  summarize(probability = n/1000)

# Plots

states <- state.name

states_map <- map_data("state")
unique(states_map$region)

dem_predictions$state <- tolower(dem_predictions$state)

dem_predictions %>% 
  rename(region = state) %>% 
  mutate(biden_win = ifelse(fit > 50, TRUE, FALSE)) %>% 
  left_join(states_map, by = "region") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = biden_win), color = "black") +
  scale_fill_manual(values = c("red","blue"),
                    name = "Election Winner", 
                    labels = c("Trump", "Biden")) + 
  labs(title = "Predicted Election Winner by State") + 
  theme_void() + 
  ggsave("oct_17_map.png")
  
dem_ec_predicted %>% 
  ggplot(aes(x = national_ec_votes)) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Distribution of Electoral College Votes for Joe Biden",
       x = "Number of Electoral College Votes",
       y = "Number of Simulations") +
  theme_minimal() +
  geom_vline(xintercept=270)  + 
  ggsave("oct_17_biden.png")

rep_ec_predicted %>% 
  ggplot(aes(x = national_ec_votes)) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Distribution of Electoral College Votes for Donald Trump",
       x = "Number of Electoral College Votes",
       y = "Number of Simulations") +
  theme_minimal() +
  geom_vline(xintercept=270)  + 
  ggsave("oct_17_trump.png")
