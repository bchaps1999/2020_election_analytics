library(tidyverse)
library(rvest)
library(fiftystater)

states <- state.name

# Poll data

polls <- read_csv("../data/pollavg_bystate_1968-2016.csv") %>%
  filter(year == 2016)

results <- read_csv("../data/popvote_bystate_1948-2016.csv") %>%
  filter(year == 2016) %>%
  select(state, R_pv2p, D_pv2p) %>%
  rename(Republican = R_pv2p,
         Democratic = D_pv2p) %>%
  pivot_longer(
    cols = c(Republican, Democratic),
    names_to = "party",
    values_to = "pv2p"
  )

polls_2020 <-
  read_csv("../data/presidential_poll_averages_2020.csv") %>%
  filter(candidate_name == "Joseph R. Biden Jr." |
           candidate_name == "Donald Trump") %>%
  mutate(
    party = ifelse(
      candidate_name == "Joseph R. Biden Jr.",
      "Democratic",
      "Republican"
    ),
    date = as.Date(modeldate, "%m/%d/%Y")
  ) %>%
  filter(cycle == 2020) %>%
  select(state, date, party, pct_estimate) %>%
  pivot_wider(names_from = party, values_from = pct_estimate) %>%
  mutate(
    democrat_2p = 100 * (Democratic) / (Democratic + Republican),
    republican_2p = 100 * (Republican) / (Democratic + Republican)
  ) %>%
  select(state, date, democrat_2p, republican_2p) %>%
  rename(Democratic = democrat_2p, Republican = republican_2p) %>%
  pivot_longer(
    cols = c(Democratic, Republican),
    names_to = "party",
    values_to = "avg_poll_2p"
  )

# Market data

population <- read_csv("../data/population.csv")

turnout <- read.csv("../data/turnout_by_state.csv", 
                    header = TRUE, 
                    skip = 1, stringsAsFactors = FALSE) %>% 
  pivot_longer(cols = c(everything(), -State), 
               names_to = "year", 
               values_to = "turnout", 
               names_prefix = "X") %>% 
  mutate(year = as.numeric(year),
         State = str_trim(State)) %>% 
  rename(state = State) %>% 
  group_by(state) %>% 
  summarize(avg_turnout = 0.01 * mean(turnout),
            sd_turnout = sd(turnout)) %>% 
  filter(!is.na(avg_turnout))
  
state_voters <- population %>%
  left_join(turnout, by = "state") %>%
  mutate(voters = population * avg_turnout)

# Electoral college data

electoral_html <-
  read_html(
    "https://www.britannica.com/topic/United-States-Electoral-College-Votes-by-State-1787124"
  )
electoral_college <- electoral_html %>%
  html_nodes("td , th") %>%
  html_text() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
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
  pivot_longer(
    cols = c(Republican, Democratic),
    names_to = "party",
    values_to = "pv2p"
  )

market_data_2016 <- NULL

market_data_2016 <-
  read_csv(file = "../data/prediction_market/Alabama.csv") %>%
  select(ContractName, Date, CloseSharePrice) %>%
  rename(party = ContractName,
         date = Date,
         Alabama = CloseSharePrice) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(Alabama = as.numeric(gsub("\\$", "", Alabama)))

add_market_data <- function(state_name) {
  file_name = paste("../data/prediction_market/", state_name, ".csv", sep =
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
  append(values = "District of Columbia") %>% 
  as_tibble() %>%
  filter(value != "New Jersey",
         value != "Alabama",!is.na(value)) %>%
  pull(value)

for (k in 1:49) {
  state_k = states[k]
  
  market_data_2016 <- add_market_data(state_k)
  
}

# 2020

market_data_2020 <- NULL

market_data_2020 <-
  read_csv(file = "../data/prediction_market/2020/Alabama.csv") %>%
  select(ContractName, Date, CloseSharePrice) %>%
  rename(party = ContractName,
         date = Date,
         Alabama = CloseSharePrice) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(Alabama = as.numeric(gsub("\\$", "", Alabama)))

add_market_data <- function(state_name) {
  file_name = paste("../data/prediction_market/2020/", state_name, ".csv", sep =
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
  append(values = "District of Columbia") %>% 
  as_tibble() %>%
  filter(value != "Alabama") %>% 
  pull()

for (k in 1:50) {
  state_k = states[k]
  
  market_data_2020 <- add_market_data(state_k)
  
}

# Functions

rmse_2016 <- function(date) {
  date_2016 = as.Date(date, "%m/%d/%Y")
  days_remaining = as.Date("11/8/2016", "%m/%d/%Y") - date_2016
  
  polls_2016 <- polls %>%
    filter(days_left >= days_remaining) %>%
    group_by(state, party) %>%
    filter(days_left == min(days_left)) %>%
    select(state, party, avg_poll) %>%
    pivot_wider(names_from = party, values_from = avg_poll) %>%
    mutate(
      democrat_2p = 100 * (democrat) / (democrat + republican),
      republican_2p = 100 * (republican) / (democrat + republican)
    ) %>%
    select(state, democrat_2p, republican_2p) %>%
    ungroup() %>%
    rename(Democratic = democrat_2p, Republican = republican_2p) %>%
    pivot_longer(
      cols = c(Democratic, Republican),
      names_to = "party",
      values_to = "avg_poll_2p"
    ) %>%
    left_join(results, by = c("state", "party")) %>%
    mutate(error = avg_poll_2p - pv2p)
  
  market_vote_2016 <- market_data_2016 %>%
    filter(date == date_2016,
           party != "Libertarian") %>%
    pivot_longer(
      cols = c(everything(), -party, -date),
      names_to = "state",
      values_to = "price"
    )
  
  polls_markets <- market_vote_2016 %>%
    pivot_wider(names_from = party, values_from = price) %>%
    mutate(
      dem_price_2p = (Democratic) / (Democratic + Republican),
      rep_price_2p = (Republican) / (Democratic + Republican)
    ) %>%
    select(state, dem_price_2p, rep_price_2p) %>%
    rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
    pivot_longer(
      cols = c(Democratic, Republican),
      names_to = "party",
      values_to = "price"
    ) %>%
    left_join(polls_2016, by = c("state", "party"))
  
  dem_polls_markets <- polls_markets %>%
    filter(party == "Democratic")
  
  poll_market_model <-
    lm(pv2p ~ avg_poll_2p + poly(price, 3), data = dem_polls_markets)
  
  resid(poll_market_model) %>%
    as_tibble() %>%
    summarize(RMSE = sqrt(mean(value * value))) %>%
    pull() %>%
    as.numeric()
  
}

predict_state_vote <- function(date_today) {
  
  # 2016 Data
  days_remaining = as.numeric(difftime(
    as.Date("11/3/2020", "%m/%d/%Y"),
    as.Date(date_today, "%m/%d/%Y")
  ))
  date_2016 = as.Date("11/8/2016", "%m/%d/%Y") - days_remaining
  
  polls_2016 <- polls %>%
    filter(days_left >= days_remaining) %>%
    group_by(state, party) %>%
    filter(days_left == min(days_left)) %>%
    select(state, party, avg_poll) %>%
    pivot_wider(names_from = party, values_from = avg_poll) %>%
    mutate(
      democrat_2p = 100 * (democrat) / (democrat + republican),
      republican_2p = 100 * (republican) / (democrat + republican)
    ) %>%
    select(state, democrat_2p, republican_2p) %>%
    ungroup() %>%
    rename(Democratic = democrat_2p, Republican = republican_2p) %>%
    pivot_longer(
      cols = c(Democratic, Republican),
      names_to = "party",
      values_to = "avg_poll_2p"
    ) %>%
    left_join(results, by = c("state", "party"))
  
  market_vote_2016 <- market_data_2016 %>%
    filter(date == date_2016,
           party != "Libertarian") %>%
    pivot_longer(
      cols = c(everything(), -party, -date),
      names_to = "state",
      values_to = "price"
    )
  
  polls_markets <- market_vote_2016 %>%
    pivot_wider(names_from = party, values_from = price) %>%
    mutate(
      dem_price_2p = (Democratic) / (Democratic + Republican),
      rep_price_2p = (Republican) / (Democratic + Republican)
    ) %>%
    select(state, dem_price_2p, rep_price_2p) %>%
    rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
    pivot_longer(
      cols = c(Democratic, Republican),
      names_to = "party",
      values_to = "price"
    ) %>%
    left_join(polls_2016, by = c("state", "party"))
  
  dem_polls_markets <- polls_markets %>%
    filter(party == "Democratic")%>% 
    mutate(log_price = log(price))
  
  # 2020 Data
  
  market_vote_2020 <- market_data_2020 %>%
    filter(date == as.Date(date_today, "%m/%d/%Y"),
           party != "Libertarian") %>%
    pivot_longer(
      cols = c(everything(), -party, -date),
      names_to = "state",
      values_to = "price"
    )
  
  polls_today <- polls_2020 %>%
    filter(date == as.Date(date_today, "%m/%d/%Y"))
  
  polls_markets_2020 <- market_vote_2020 %>%
    pivot_wider(names_from = party, values_from = price) %>%
    mutate(
      dem_price_2p = (Democratic) / (Democratic + Republican),
      rep_price_2p = (Republican) / (Democratic + Republican)
    ) %>%
    select(state, dem_price_2p, rep_price_2p) %>%
    rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
    pivot_longer(
      cols = c(Democratic, Republican),
      names_to = "party",
      values_to = "price"
    ) %>%
    left_join(polls_today, by = c("state", "party"))
  
  dem_polls_markets_2020 <- polls_markets_2020 %>%
    filter(party == "Democratic") %>% 
    mutate(log_price = log(price))
  
  # 2020 Predictions
  
  dem_poll_market_model <-
    lm(pv2p ~ avg_poll_2p + poly(log_price, 3), data = dem_polls_markets)
  
  predict(dem_poll_market_model,
          newdata = dem_polls_markets_2020,
          interval = "prediction") %>%
    as_tibble() %>%
    mutate(sd = (upr - fit)/1.96) %>%
    bind_cols(dem_polls_markets_2020) %>%
    select(state, fit, sd) %>% 
    left_join(electoral_college, by = "state") %>%
    left_join(state_voters, by = c("state")) %>%
    mutate(votes = voters * (fit / 100))
  
}

predict_vote <- function(date) {

  predict_state_vote(date) %>% 
    summarize(sum(votes)/sum(voters)) %>% 
    pull()
  
}

predict_ec <- function(date) {
  
  predict_state_vote(date) %>% 
    mutate(ec_votes = ifelse(fit > 50, ec_votes, 0)) %>% 
    summarize(sum(ec_votes)) %>% 
    pull()
  
}

predict_probability <- function(date) {
  
  dem_predictions <- predict_state_vote(date)
  
  vector_length = 51000
  
  dem_state_predicted <-
    data.frame(
      state = vector("character", length = vector_length),
      prediction = vector("numeric", length = vector_length),
      rep = vector("numeric", length = vector_length),
      ec_votes = vector("numeric", length = vector_length),
      stringsAsFactors = FALSE
    )
  
  for (i in 1:1000) {
    for (j in 1:51) {
      k = (i - 1) * 51 + j
      state = dem_predictions$state[j]
      vote = dem_predictions$fit[j]
      vote_sd = dem_predictions$sd[j]
      ec_votes = dem_predictions$ec_votes[j]
      dem_state_distribution = rnorm(n = 1000, mean = vote, sd = vote_sd)
      dem_state_predicted$prediction[k] = sample(dem_state_distribution, size = 1)
      dem_state_predicted$rep[k] = i
      dem_state_predicted$state[k] = state
      dem_state_predicted$ec_votes[k] = ec_votes
      
    }
    
  }
  
  dem_state_predicted %>%
    mutate(ec_votes = ifelse(prediction > 50, ec_votes, 0)) %>%
    group_by(rep) %>%
    summarize(national_ec_votes = sum(ec_votes)) %>% 
    filter(national_ec_votes >= 270) %>% 
    count() %>% 
    summarize(n/1000)
  
}

oct_31 <- predict_state_vote("10/31/2020")

model_over_time <-
  data.frame(
    date = seq(
      as.Date("10/3/2020", "%m/%d/%Y"),
      as.Date("10/31/2020", "%m/%d/%Y"),
      by = "day"
    ),
    dem_pv2p = vector("numeric", 29),
    dem_ec = vector("numeric", 29),
    chances = vector("numeric", 29)
  ) %>% 
  mutate(
    dem_pv2p = map_dbl(date, predict_vote),
    dem_ec = map_dbl(date, predict_ec),
    chances = map_dbl(date, predict_probability)
  )

## Model validation

date_today = "10/31/2020"
# 2016 Data
days_remaining = as.numeric(difftime(
  as.Date("11/3/2020", "%m/%d/%Y"),
  as.Date(date_today, "%m/%d/%Y")
))
date_2016 = as.Date("11/8/2016", "%m/%d/%Y") - days_remaining

polls_2016 <- polls %>%
  filter(days_left >= days_remaining) %>%
  group_by(state, party) %>%
  filter(days_left == min(days_left)) %>%
  select(state, party, avg_poll) %>%
  pivot_wider(names_from = party, values_from = avg_poll) %>%
  mutate(
    democrat_2p = 100 * (democrat) / (democrat + republican),
    republican_2p = 100 * (republican) / (democrat + republican)
  ) %>%
  select(state, democrat_2p, republican_2p) %>%
  ungroup() %>%
  rename(Democratic = democrat_2p, Republican = republican_2p) %>%
  pivot_longer(
    cols = c(Democratic, Republican),
    names_to = "party",
    values_to = "avg_poll_2p"
  ) %>%
  left_join(results, by = c("state", "party"))

market_vote_2016 <- market_data_2016 %>%
  filter(date == date_2016,
         party != "Libertarian") %>%
  pivot_longer(
    cols = c(everything(), -party, -date),
    names_to = "state",
    values_to = "price"
  )

polls_markets <- market_vote_2016 %>%
  pivot_wider(names_from = party, values_from = price) %>%
  mutate(
    dem_price_2p = (Democratic) / (Democratic + Republican),
    rep_price_2p = (Republican) / (Democratic + Republican)
  ) %>%
  select(state, dem_price_2p, rep_price_2p) %>%
  rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
  pivot_longer(
    cols = c(Democratic, Republican),
    names_to = "party",
    values_to = "price"
  ) %>%
  left_join(polls_2016, by = c("state", "party"))

dem_polls_markets <- polls_markets %>%
  filter(party == "Democratic")%>% 
  mutate(log_price = log(price))

# 2020 Data

market_vote_2020 <- market_data_2020 %>%
  filter(date == as.Date(date_today, "%m/%d/%Y"),
         party != "Libertarian") %>%
  pivot_longer(
    cols = c(everything(), -party, -date),
    names_to = "state",
    values_to = "price"
  )

polls_today <- polls_2020 %>%
  filter(date == as.Date(date_today, "%m/%d/%Y"))

polls_markets_2020 <- market_vote_2020 %>%
  pivot_wider(names_from = party, values_from = price) %>%
  mutate(
    dem_price_2p = (Democratic) / (Democratic + Republican),
    rep_price_2p = (Republican) / (Democratic + Republican)
  ) %>%
  select(state, dem_price_2p, rep_price_2p) %>%
  rename(Democratic = dem_price_2p, Republican = rep_price_2p) %>%
  pivot_longer(
    cols = c(Democratic, Republican),
    names_to = "party",
    values_to = "price"
  ) %>%
  left_join(polls_today, by = c("state", "party"))

dem_polls_markets_2020 <- polls_markets_2020 %>%
  filter(party == "Democratic") %>% 
  mutate(log_price = log(price))

# Poll and market model

dem_poll_market_model <-
  lm(pv2p ~ avg_poll_2p + poly(log_price, 3), data = dem_polls_markets)

summary(dem_poll_market_model)

resid(dem_poll_market_model) %>% 
  as_tibble() %>% 
  summarize(RMSE = sqrt(mean(value*value)))

combined_outsamp_errors <- sapply(1:1000, function(i){
  train_ind <- sample(seq_len(nrow(dem_polls_markets)), 
                      size = 49)
  train <- dem_polls_markets[train_ind, ]
  test <- dem_polls_markets[-train_ind, ]
  outsamp_mod <- lm(pv2p ~ avg_poll_2p + poly(log_price,3), data = train)
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

# Plots

dem_polls_markets %>% 
  filter(state != "District of Columbia") %>% 
  ggplot(aes(x = price, y = pv2p)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3)) + 
  labs(title = "PredictIt Market Price and Vote Percent by State - Hillary Clinton",
       x = "PredictIt Market Price on 5 Nov. 2016(USD)", 
       y = "Two-Party Vote Percent in 2016 Election") +
  theme_minimal()+
  ggsave("market_price.png")

data("fifty_states")

oct_31$state <- tolower(oct_31$state)

ggplot(oct_31, aes(map_id = state)) + 
  geom_map(aes(fill = fit), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 50
  ) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", fill = "Percent", title = "Predicted Percent of Two-Party Popular Vote for Biden by State") + 
  theme_minimal() +
  ggsave(filename = "final_map.png")

oct_31 %>% 
  mutate(winner = ifelse(fit > 50, TRUE, FALSE)) %>% 
  ggplot(aes(map_id = state)) + 
  geom_map(aes(fill = winner), map = fifty_states, color = "black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_manual(values = c("red","blue"),
                    labels = c("Trump", "Biden")) +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", fill = "Winner", title = "Predicted Election Winner by State") + 
  theme_minimal()  +
  ggsave(filename = "final_winner_map.png")

simulations %>% 
  ggplot(aes(x = national_ec_votes)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = "Disitribution of Electoral College Votes Over 1000 Simlations",
       x = "Electoral College Votes for Joe Biden",
       y = "Number of Simulations") + 
  geom_vline(xintercept = 270, color = "red") + 
  ggsave(filename = "final_ec.png")
