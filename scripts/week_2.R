library(tidyverse)
library(readxl)
library(ggpubr)
library(janitor)
library(lubridate)
library(ggthemes)

# Data

popvote <- read_csv("data/popvote_1948-2016.csv") %>% 
  clean_names() %>% 
  filter(incumbent_party == TRUE)

econ <- read_csv("data/econ.csv") %>% 
  clean_names() %>% 
  mutate(stock_change = (stock_close - lag(stock_close))/lag(stock_close))

median_income <- read_csv("data/median_income_national.csv") %>% 
  clean_names %>% 
  mutate(year = year(date)) %>% 
  rename(
    income = mepainusa672n
  ) %>% 
  select(-date, year, income) %>% 
  mutate(change = (lag(income) - lag(lag(income)))/lag(lag(income)))

rdpi <- read_csv("data/rdpi.csv") %>% 
  clean_names() %>% 
  rename(disp_income = dspic96) %>% 
  mutate(month = month(date),
         year = year(date))

sp_500 <- read_csv("data/SP500.csv") %>% 
  clean_names() %>% 
  mutate(perc_change = (close - lag(close))/lag(close)) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(year, month, close, perc_change)

rdpi_oct <- rdpi %>% 
  filter(month == 1 | month == 10) %>% 
  group_by(year) %>% 
  mutate(change = 100 * (disp_income - lag(disp_income))/lag(disp_income)) %>% 
  filter(!is.na(change)) %>% 
  right_join(popvote, by = "year") %>% 
  filter(!is.na(change))

rdpi_july <- rdpi %>% 
  filter(month == 1 | month == 7) %>% 
  group_by(year) %>% 
  mutate(change = 100 * (disp_income - lag(disp_income))/lag(disp_income)) %>%
  filter(!is.na(change)) %>% 
  ungroup() %>% 
  right_join(popvote, by = "year") %>% 
  filter(!is.na(change))

sp_growth <- sp_500 %>% 
  filter(month == 7 | month == 10) %>% 
  group_by(year) %>% 
  mutate(change = 100 *(close - lag(close))/lag(close)) %>% 
  filter(!is.na(change)) %>% 
  left_join(popvote, by = c("year")) %>% 
  filter(!is.na(pv2p))

gdp_growth <- popvote %>% 
  right_join(econ, by = "year") %>% 
  filter(!is.na(pv2p), quarter == 2)

# Plots

rdpi_oct %>% 
  ggplot(aes(x = change, y = pv2p)) + 
  geom_point(aes(color = party)) +
  scale_color_manual(values = c("blue", "red")) +
  stat_cor() +
  geom_smooth(method = "lm") +
  labs(title = "Real Disposable Personal Income and Vote Share: Jan-Oct ",
       x = "Percent Change in RDPI Between January and October",
       y = "Vote Share for Incumbent Party",
       color = "Party",
       caption = "RDPI Source: FRED Economic Data") + 
  theme_minimal()

rdpi_july %>% 
  ggplot(aes(x = change, y = pv2p)) + 
  geom_point(aes(color = party)) +
  scale_color_manual(values = c("blue", "red")) +
  stat_cor() +
  geom_smooth(method = "lm") +
  labs(title = "Real Disposable Personal Income and Vote Share: Jan-July",
       x = "Percent Change in RDPI Between January and July",
       y = "Vote Share for Incumbent Party",
       color = "Party",
       caption = "RDPI Source: FRED Economic Data") + 
  theme_minimal()

sp_growth %>% 
  ggplot(aes(x = change, y = pv2p)) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = c("blue", "red")) +
  geom_smooth(method = "lm") +
  stat_cor() + 
  labs(title = "Change in S&P 500 and Incumbent Party Vote Share",
       x = "Percent Change in S&P 500 Closing Price From 7/31 to 10/31",
       y = "Incumbent Party Share of Two-Party Vote",
       color = "Party", 
       caption = "S&P 500 Data Source: Yahoo Finance") +
  theme_minimal()

gdp_growth %>% 
  ggplot(aes(x = gdp_growth_qt, y = pv2p)) +
  geom_point(aes(color = party)) +
  scale_color_manual(values = c("blue", "red")) +
  geom_smooth(method = "lm") +
  stat_cor() +
  labs(title = "GDP Growth in the 2nd Quarter and Vote Share",
       x = "GDP Percent Growth in 2nd Quarter",
       y = "Vote Share for Incumbent Party",
       color = "Party") +
  theme_minimal()

# Models and error

rdpi_oct_lm <- lm(rdpi_oct$pv2p ~ rdpi_oct$change)

resid(rdpi_oct_lm) %>% 
  as_tibble() %>% 
  summarize(mse = sqrt(mean(value*value)))

rdpi_july_lm <- lm(data = rdpi_july, pv2p ~ change)

summary(rdpi_july_lm)

resid(rdpi_july_lm) %>% 
  as_tibble() %>% 
  summarize(mse = sqrt(mean(value*value)))

july_2020 <- rdpi %>% 
  filter(month == 1 | month == 7) %>% 
  group_by(year) %>% 
  mutate(change = 100 * (disp_income - lag(disp_income))/lag(disp_income)) %>% 
  filter(!is.na(change)) %>% 
  ungroup() %>% 
  filter(year == 2020) %>% 
  select(change)

gdp_lm <- lm(gdp_growth$pv2p ~ gdp_growth$gdp_growth_qt)

resid(gdp_lm) %>% 
  as_tibble() %>% 
  summarize(mse = sqrt(mean(value*value)))

sp_lm <- lm(sp_growth$pv2p ~ sp_growth$perc_change)

resid(sp_lm) %>% 
  as_tibble() %>% 
  summarize(mse = sqrt(mean(value*value)))

outsamp_errors <- sapply(1:1000, function(i){
  rdpi_outsamp <- sample_n(rdpi_july, size = 8)
  outsamp_mod <- lm(rdpi_outsamp$pv2p ~ rdpi_outsamp$change)
  predict(outsamp_mod, newdata = rdpi_outsamp) %>%
    as_tibble() %>% 
    bind_cols(rdpi_outsamp) %>% 
    mutate(error = value - pv2p) %>% 
    summarize(mean_error = sqrt(mean(error*error))) %>% 
    pull()
}) %>% 
  as_tibble()

outsamp_errors %>% 
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.25)

outsamp_errors %>%
  summarize(mean(value))

predict(rdpi_july_lm, newdata = data.frame(july_2020), interval = "prediction")
