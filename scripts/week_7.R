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


polls_2016 <- polls %>%
  filter(days_left >= 9) %>%
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

# Market data

population <- read_csv("../data/population.csv")

turnout <- read.csv("../data/turnout_2016.csv", header = FALSE) %>%
  rename(state = V1, perc = V2) %>%
  mutate(perc = as.numeric(gsub("[\\%,]", "", perc)),
         state = as.character(state))

state_voters <- population %>%
  left_join(turnout, by = "state") %>%
  mutate(voters = population * (perc / 100))

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
  as_tibble() %>%
  filter(value != "New Jersey",
         value != "Alabama",!is.na(value)) %>%
  pull(value)

for (k in 1:47) {
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
  as_tibble() %>%
  filter(value != "Alabama") %>%
  pull()

for (k in 1:49) {
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

predict_vote <- function(date_today) {
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
    filter(party == "Democratic")
  
  # 2020 Predictions
  
  dem_poll_market_model <-
    lm(pv2p ~ poly(price, 3) + avg_poll_2p, data = dem_polls_markets)
  
  predict(dem_poll_market_model,
                             newdata = dem_polls_markets_2020,
                             interval = "prediction") %>%
    as_tibble() %>%
    mutate(sd = (upr - lwr) / (1.96 * 2)) %>%
    bind_cols(dem_polls_markets_2020) %>%
    select(state, fit, sd) %>%
    left_join(electoral_college, by = "state") %>%
    left_join(state_voters, by = c("state")) %>%
    mutate(votes = voters * (fit / 100)) %>%
    summarize(dem_pv2p = sum(votes) / sum(voters)) %>%
    pull() %>%
    as.numeric()
  
}

predict("10/24/2020")

predict_election <- function(date_today) {
  
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
    filter(party == "Democratic")
  
  dem_poll_market_model <-
    lm(pv2p ~ poly(price, 3) + avg_poll_2p, data = dem_polls_markets)
  
  dem_poll_market_model %>% 
    summary()
  
  set.seed(2020)
  
  dem_predictions <- predict(dem_poll_market_model,
                             newdata = dem_polls_markets_2020,
                             interval = "prediction") %>%
    as_tibble() %>%
    mutate(sd = (upr - lwr) / (1.96 * 2)) %>%
    bind_cols(dem_polls_markets_2020) %>%
    select(state, fit, sd) %>%
    left_join(electoral_college, by = "state")
  
  vector_length = 50000
  
  dem_state_predicted <-
    data.frame(
      state = vector("character", length = vector_length),
      prediction = vector("numeric", length = vector_length),
      rep = vector("numeric", length = vector_length),
      ec_votes = vector("numeric", length = vector_length),
      stringsAsFactors = FALSE
    )
  
  for (i in 1:1000) {
    for (j in 1:50) {
      i = 1
      j = 1
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
      
    }
    
  }
  
  dem_state_predicted %>%
    mutate(ec_votes = ifelse(prediction > 50, ec_votes, 0)) %>%
    group_by(rep) %>%
    summarize(national_ec_votes = sum(ec_votes) + 3) %>% 
    filter(national_ec_votes > 270) %>%
    count() %>%
    summarize(probability = n / 1000) %>%
    pull()
  
}

rmse_changes <-
  data.frame(date = seq(
    as.Date("9/19/2016", "%m/%d/%Y"),
    as.Date("11/07/2016", "%m/%d/%Y"),
    by = "day"
  ),
  rmse = vector("numeric", 50)) %>%
  mutate(rmse = map(date, rmse_2016),
         rmse = as.numeric(rmse))

model_over_time <-
  data.frame(
    date = seq(
      as.Date("10/3/2020", "%m/%d/%Y"),
      as.Date("10/24/2020", "%m/%d/%Y"),
      by = "day"
    ),
    dem_pv2p = vector("numeric", 2),
    chances = vector("numeric", 2)
  ) %>% 
  mutate(
    dem_pv2p = map_dbl(date, predict_vote),
    chances = map_dbl(date, predict_election)
  )

rmse_changes %>%
  ggplot(aes(x = date, y = rmse)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Changes in Regression Model RMSE in Weeks Before 2016 Election",
       x = "Date",
       y = "RMSE")

model_over_time %>%
  ggplot(aes(x = date, y = chances)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Changes in the Probability of a Joe Biden Electoral College Victory",
       x = "Date",
       y = "Percent of Simulations Won by Biden") +
  ggsave("chances_over_time.png")

model_over_time %>%
  ggplot(aes(x = date, y = dem_pv2p)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Changes in the Predicted Two-Party Vote Percent for Joe Biden",
       x = "Date",
       y = "Predicted Two-Party Vote Percent") +
  ggsave("vote_percent_over_time.png")
