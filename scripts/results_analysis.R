library(tidyverse)

# Import final predictions and results

results_2020 <- read_csv("../data/results.csv") %>% 
  mutate(dem_votes = ifelse(leading_candidate_name == "Biden", leading_candidate_votes, trailing_candidate_votes),
         rep_votes = ifelse(leading_candidate_name == "Trump", leading_candidate_votes, trailing_candidate_votes)) %>% 
  group_by(state) %>% 
  filter(timestamp == max(timestamp)) %>% 
  select(state, dem_votes, rep_votes) %>% 
  ungroup() %>% 
  mutate(state = str_replace(state, " \\(.*\\)", ""),
         dem_pv2p = dem_votes/(dem_votes + rep_votes),
         rep_pv2p = rep_votes/(dem_votes + rep_votes))

results_2016 <- read_csv("../data/popvote_bystate_1948-2020.csv") %>% 
  filter(year == 2016) %>% 
  select(state, D_pv2p) %>% 
  rename(prev_pv2p = D_pv2p)

results_2012 <- read_csv("../data/popvote_bystate_1948-2020.csv") %>% 
  filter(year ==  2012) %>% 
  select(state, D_pv2p) %>% 
  rename(prev_pv2p = D_pv2p)

final <- read_csv("../data/final_predictions.csv") %>% 
  select(-X1) %>% 
  left_join(results_2020, by = "state") %>% 
  mutate(dem_pv2p = 100 * dem_pv2p)

state_voters_avg <- read_csv("../data/state_voters.csv") %>% 
  select(state, voters)

state_voters_2016 <- read_csv("../data/turnout_2016.csv") %>% 
  select(state, voters)

dem_final <- final %>% 
  select(state, fit, dem_pv2p) %>% 
  mutate(error = fit - dem_pv2p,
         abs_error = abs(error)) %>% 
  left_join(state_voters_avg, by = "state") %>% 
  rename(voters_avg = voters) %>% 
  left_join(state_voters_2016, by = "state") %>% 
  rename(voters_2016 = voters)

final_2016 <- read_csv("../data/dem_2016.csv") %>% 
  select(state, price, log_price, avg_poll_2p, pv2p) %>% 
  mutate(year = 2016) %>% 
  left_join(results_2012, by = "state")

final_2020 <- read_csv("../data/dem_2020.csv") %>% 
  left_join(dem_final) %>% 
  select(state, fit, error, price, log_price, avg_poll_2p, dem_pv2p) %>% 
  rename(pv2p = dem_pv2p) %>% 
  mutate(year = 2020) %>% 
  left_join(results_2016)

all <- final_2020 %>% 
  select(-fit, -error) %>% 
  rbind(final_estimates_2016)

# Plots and error analysis

dem_final %>% 
  filter(state != "District of Columbia") %>% 
  ggplot(aes(x = fit, y = dem_pv2p)) + 
  geom_point() +
  geom_abline() + 
  labs(title = "Predicted Versus Actual Election Results by State",
       x = "Predicted Vote Percent for Biden",
       y = "Actual Vote Percent for Biden") + 
  theme_minimal()

dem_final %>% 
  ggplot(aes(x = fit, y = error)) +
  geom_point() + 
  labs(title = "Vote Percent Prediction Error by State",
       x = "Predicted Vote Percent for Biden",
       y = "Prediction Error (Percentage Points)") + 
  geom_abline(slope = 0, intercept = 0) + 
  theme_minimal()

dem_final %>% 
  summarize(rmse = sqrt(mean(error * error)),
            average_abs_error = mean(abs(error)),
            average_error = mean(error))

final_2020 %>%
  mutate(over_under = ifelse(error > 0, 1, 0),
         model_correct = ifelse(fit > 50 & pv2p > 50, 1, ifelse(fit < 50 & pv2p < 50, 1, 0)),
         poll_over_under = ifelse(avg_poll_2p > pv2p, 1, 0),
         poll_correct = ifelse(avg_poll_2p > 50 & pv2p > 50, 1, 
                               ifelse(avg_poll_2p < 50 & pv2p < 50, 1, 0)),
         market_over_under = ifelse(price * 100 > pv2p, 1, 0), 
         market_correct = ifelse(price > 0.50 & pv2p > 50, 1, 
                               ifelse(price < 0.50 & pv2p < 50, 1, 0))) %>% 
  summarize(
    num_over_model = sum(over_under),
    num_under_model = 51 - num_over_model,
    model_correct = sum(model_correct),
    num_over_poll = sum(poll_over_under),
    num_under_poll = 51 - num_over_poll,
    poll_correct = sum(poll_correct),
    num_over_market = sum(market_over_under),
    num_under_market = 51 - num_over_market,
    market_correct = sum(market_correct)
    )

dem_final %>%   
  summarize(
    pred_vote = sum(fit * voters_2016) / sum(voters_2016),
    pred_vote_2016_turnout = sum(fit * voters_avg) / sum(voters_avg),
    actual_vote_model_turnout = sum(dem_pv2p * voters_avg) / sum(voters_avg),
    actual_vote_2016_turnout = sum(dem_pv2p * voters_2016)/sum(voters_2016)
  )

# Testing variations of model

summary(lm(formula = pv2p ~ avg_poll_2p + poly(log_price, 3), data = final_2016))

prev_results_model_2016 <- all %>% 
  filter(year == 2016) %>% 
  lm(pv2p ~ avg_poll_2p + poly(log_price, 3) + prev_pv2p, data = .)

summary(prev_results_model_2016)

predict(prev_results_model_2016,
        newdata = final_2020) %>%
  as_tibble() %>% 
  bind_cols(final_estimates_2020) %>%
  select(state, value, pv2p) %>% 
  mutate(error = value - pv2p) %>%
  left_join(state_voters_avg, by = "state") %>% 
  mutate(over_under = ifelse(error > 0, 1, 0),
         correct = ifelse(value > 50 & pv2p > 50, 1, ifelse(value < 50 & pv2p < 50, 1, 0))) %>%
  summarize(
    pred_vote = sum(value * voters) / sum(voters),
    rmse = sqrt(mean(error * error)),
    average_abs_error = mean(abs(error)),
    average_error = mean(error),
    num_over_pred = sum(over_under),
    num_under_pred = 51 - num_over_pred,
    num_correct = sum(correct))

prev_results_model_2020 <- all %>% 
  filter(year == 2020) %>% 
  lm(pv2p ~ avg_poll_2p + poly(log_price, 3) + prev_pv2p, data = .)

summary(prev_results_model_2020)

predict(prev_results_model_2020,
        newdata = final_2020) %>%
  as_tibble() %>% 
  bind_cols(final_estimates_2020) %>%
  select(state, value, pv2p) %>% 
  mutate(error = value - pv2p) %>% 
  summarize(
    rmse = sqrt(mean(error * error)),
    average_abs_error = mean(abs(error)))

prev_results_model_all <- all %>% 
  lm(pv2p ~ avg_poll_2p + poly(log_price, 3) + prev_pv2p, data = .)

summary(prev_results_model_all)

predict(prev_results_model_all,
        newdata = all) %>%
  as_tibble() %>% 
  bind_cols(all) %>%
  select(state, value, pv2p) %>% 
  mutate(error = value - pv2p) %>% 
  summarize(
    rmse = sqrt(mean(error * error)),
    average_abs_error = mean(abs(error)))
