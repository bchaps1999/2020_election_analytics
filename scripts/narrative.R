library(tidyverse)
library(usmap)
library(readxl)
library(janitor)

counties <- map_data("county")

NCHS <- read_xlsx("../data/NCHS.xlsx") %>% 
  clean_names() %>% 
  mutate(class = ifelse(x2013_code == 1, "urban", ifelse(x2013_code %in% c(2,3,4), "suburban", "rural"))) %>% 
  select(fips_code, state_abr, county_name, x2013_code, class) %>% 
  rename(fips = fips_code) %>% 
  select(fips, class)

county_classifications <- read_csv("../data/county_classifications.csv") %>% 
  clean_names() %>% 
  mutate(state = as.character(as.numeric(gsub("\'", "", state))),
         county = gsub("\'", "", county),
         fips = paste(state,county,sep="")) %>% 
  group_by(fips) %>% 
  mutate(pop_weight = acs17_occupied_housing_units_est/sum(acs17_occupied_housing_units_est),
         weighted_urban = upsai_urban*pop_weight,
         weighted_suburban = upsai_suburban*pop_weight,
         weighted_rural = upsai_rural*pop_weight) %>% 
  summarize(sum_urban = sum(weighted_urban),
            sum_suburban = sum(weighted_suburban),
            sum_rural = sum(weighted_rural)) %>% 
  mutate(pred_class = ifelse(sum_urban > sum_suburban & sum_urban > sum_rural, "urban",
                        ifelse(sum_suburban > sum_urban & sum_suburban > sum_rural, 
                               "suburban", "rural")),
         fips = as.numeric(fips)) %>% 
  select(fips, pred_class)

county_results <- read_csv("../data/county_results.csv") %>% 
  clean_names() %>% 
  filter(geographic_name != "name") %>% 
  select(fips, geographic_name, geographic_subtype, joseph_r_biden_jr, donald_j_trump) %>% 
  mutate(fips = as.numeric(fips),
         joseph_r_biden_jr = as.numeric(joseph_r_biden_jr),
         donald_j_trump = as.numeric(donald_j_trump),
         dem_pv2p = joseph_r_biden_jr/(joseph_r_biden_jr + donald_j_trump),
         rep_pv2p = 1 - dem_pv2p) %>% 
  left_join(county_classifications, by = "fips") %>% 
  rename(dem_votes = joseph_r_biden_jr, rep_votes = donald_j_trump)

county_results_2016 <- read_csv("../data/county_results_2016.csv") %>% 
  mutate(dem_pv2p_2016 = votes_dem/(votes_dem + votes_gop),
         rep_pv2p_2016 = votes_gop/(votes_dem + votes_gop),
         fips = combined_fips) %>% 
  rename(dem_votes_2016 = votes_dem,
         rep_votes_2016 = votes_gop) %>% 
  select(fips, state_abbr, county_name, dem_pv2p_2016, rep_pv2p_2016, dem_votes_2016, rep_votes_2016)

county <- county_results %>%
  left_join(county_results_2016, by = "fips") %>%
  left_join(NCHS, by = "fips") %>%
  select(
    fips,
    pred_class,
    class,
    state_abbr,
    county_name,
    dem_pv2p_2016,
    rep_pv2p_2016,
    dem_pv2p,
    rep_pv2p,
    dem_votes_2016,
    rep_votes_2016,
    dem_votes,
    rep_votes
  ) %>%
  mutate(
    dem_win_2016 = ifelse(dem_pv2p_2016 > 0.50, 1, 0),
    dem_win = ifelse(dem_pv2p > 0.50, 1, 0),
    perc_change_rep = rep_votes / rep_votes_2016,
    total_votes = dem_votes + rep_votes,
    expected_dem_votes = total_votes * dem_pv2p_2016,
    expected_rep_votes = total_votes * rep_pv2p_2016
  ) %>%
  filter(!is.na(dem_pv2p),!is.na(dem_pv2p_2016),!is.na(class))

county %>%   
  filter(!is.na(class)) %>% 
  plot_usmap(regions = "counties", data = ., values = "class", exclude = c("AK", "HI")) +
  labs(title = "NCHS County Classifications",
       fill = "Classification") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.justification = "center") +
  ggsave("../figures/counties.png")

state_outcomes <- data.frame(
  state = state.name, 
  dem_actual = vector(mode = "numeric", length = "50"),
  rep_actual = vector(mode = "numeric", length = "50"),
  dem_suburb_constant = vector(mode = "numeric", length = "50"),
  rep_suburb_constant = vector(mode = "numeric", length = "50"),
  dem_urban_constant = vector(mode = "numeric", length = "50"),
  rep_urban_constant = vector(mode = "numeric", length = "50"),
  dem_rural_constant = vector(mode = "numeric", length = "50"),
  rep_rural_constant = vector(mode = "numeric", length = "50"))

for (i in 1:50){
  
  state = state.abb[i]
  
  sim <- county %>% 
    group_by(class) %>% 
    filter(state_abbr == state) %>% 
    summarize(dem_votes = sum(dem_votes),
              rep_votes = sum(rep_votes),
              expected_dem_votes = sum(expected_dem_votes),
              expected_rep_votes = sum(expected_rep_votes))
  
  # Biden vote actual
  state_outcomes$dem_actual[i] <- sum(sim$dem_votes)
  # Trump vote actual
  state_outcomes$rep_actual[i] <- sum(sim$rep_votes)
  # Biden vote, holding suburbs constant  
  state_outcomes$dem_suburb_constant[i] <- as.numeric(sim[1,2] + sim[3,2] + sim[2,4])
  # Trump vote, holding suburbs constant
  state_outcomes$rep_suburb_constant[i] <- as.numeric(sim[1,3] + sim[3,3] + sim[2,5])
  # Biden vote, holding urban constant
  state_outcomes$dem_urban_constant[i] <- as.numeric(sim[1,2] + sim[2,2] + sim[3,4])
  # Biden vote, holding urban constant
  state_outcomes$rep_urban_constant[i] <- as.numeric(sim[1,3] + sim[2,3] + sim[3,5])
  # Biden vote, holding rural constant
  state_outcomes$dem_rural_constant[i] <- as.numeric(sim[1,4] + sim[2,2] + sim[3,2])
  # Biden vote, holding rural constant
  state_outcomes$rep_rural_constant[i] <- as.numeric(sim[1,5] + sim[2,3] + sim[3,3])
  
}

state_outcomes <- state_outcomes %>% 
  mutate(winner = ifelse(dem_actual > rep_actual, "Biden", "Trump"),
         winner_suburbs_constant = ifelse(dem_suburb_constant > rep_suburb_constant, "Biden", "Trump"),
         winner_rural_constant = ifelse(dem_rural_constant > rep_rural_constant, "Biden", "Trump"),
         winner_urban_constant = ifelse(dem_urban_constant > rep_urban_constant, "Biden", "Trump"),
         winner_suburbs_constant = ifelse(is.na(winner_suburbs_constant), winner, winner_suburbs_constant),
         winner_urban_constant = ifelse(is.na(winner_urban_constant), winner, winner_urban_constant),
         winner_rural_constant = ifelse(is.na(winner_rural_constant), winner, winner_rural_constant))

state_outcomes %>%
  plot_usmap(regions = "states", data = ., values = "winner_suburbs_constant", exclude = c("AK", "HI")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Vote Share in Suburban Counties Held Constant",
       fill = "Winner") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.justification = "center") +
  ggsave("../figures/suburban.png")

state_outcomes %>%   
  plot_usmap(regions = "states", data = ., values = "winner_urban_constant", exclude = c("AK", "HI")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Vote Share in Urban Counties Held Constant",
       fill = "Winner") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.justification = "center")  +
  ggsave("../figures/urban.png")

state_outcomes %>%   
  plot_usmap(regions = "states", data = ., values = "winner_rural_constant", exclude = c("AK", "HI")) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Vote Share in Rural Counties Held Constant",
       fill = "Winner") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.justification = "center") +
  ggsave("../figures/rural.png")

county %>% 
  group_by(class) %>% 
  summarize(mean(dem_pv2p),
            mean(dem_pv2p_2016),
            sum(dem_win)/n(),
            sum(dem_win_2016)/n())

