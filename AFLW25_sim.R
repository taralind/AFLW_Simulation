library(fitzRoy)
library(tidyverse)
library(purrr)
library(elo)
library(lubridate)
library(scales)

# 2025 fixture
fixture <- fitzRoy::fetch_fixture_afl(2025, comp="AFLW")

fixture <- fixture %>%
  mutate(date = as.Date(ymd_hms(utcStartTime)))

# 2024 final ladder
ladder_2024 <- fitzRoy::fetch_ladder_afl(2024, comp="AFLW")

# 2017-2024 results
results2017 <- fitzRoy::fetch_results_afl(2017, comp="AFLW")
results2018 <- fitzRoy::fetch_results_afl(2018, comp="AFLW")
results2019 <- fitzRoy::fetch_results_afl(2019, comp="AFLW")
results2020 <- fitzRoy::fetch_results_afl(2020, comp="AFLW")
results2021 <- fitzRoy::fetch_results_afl(2021, comp="AFLW")
results2022 <- fitzRoy::fetch_results_afl(2022, comp="AFLW")
results2023 <- fitzRoy::fetch_results_afl(2023, comp="AFLW")
results2024 <- fitzRoy::fetch_results_afl(2024, comp="AFLW")

results <- bind_rows(
  results2017,
  results2018,
  results2019,
  results2020,
  results2021,
  results2022,
  results2023,
  results2024
)

results <- results %>%
  mutate(seas_rnd = paste0(round.year, ".", round.roundNumber))

HGA <- 30 # home ground advantage
carryOver <- 0.7 # season carry over
k_val <- 30 # update weighting factor

# map the match outcome to a probability scale between 0 and 1
map_margin_to_outcome <- function(margin, marg.max = 80, marg.min = -80) {
  norm <- (margin - marg.min) / (marg.max - marg.min)
  norm %>%
    pmin(1) %>%
    pmax(0)
}

# run ELO
elo.data <- elo.run(
  map_margin_to_outcome(homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore) ~
    adjust(match.homeTeam.name, HGA) +
    match.awayTeam.name +
    regress(round.year, 1500, carryOver) +
    group(seas_rnd),
  k = k_val,
  data = results
)

# make team names compatible
fixture <- fixture %>%
  rename(
    match.homeTeam.name = home.team.name,
    match.awayTeam.name = away.team.name
  )

# calculate win probabilities
fixture <- fixture %>%
  mutate(Hwin_prob_Elo2024 = predict(elo.data, newdata = fixture))

#write.csv(fixture, "fixture2025_elo_winprobs_w.csv")

### Simulation ###

num_simulations <- 10000

# simulate the season
sim_results <- fixture %>%
  expand_grid(sim = 1:num_simulations) %>%
  mutate(Home.Win = rbinom(n(), 1, Hwin_prob_Elo2024),
         Winner = ifelse(Home.Win == 1, match.homeTeam.name, match.awayTeam.name)) %>%  
  group_by(sim, Winner) %>%  
  summarise(Win.Count = n(), .groups = "drop") 

# determine final ladder positions per simulation
ladder_positions <- sim_results %>%
  group_by(sim) %>%
  arrange(sim, desc(Win.Count)) %>%
  mutate(Position = row_number()) %>%
  ungroup()

# calculate the probability of each team finishing in each position
prob_table <- ladder_positions %>%
  count(Winner, Position) %>%
  mutate(Probability = n / num_simulations) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = Position, values_from = Probability, values_fill = 0)

colnames(prob_table) <- c("Team", paste0("Prob_", 1:(ncol(prob_table)-1)))

# add in 2024 ladder
prob_table <- prob_table %>%
  left_join(
    ladder_2024 %>% dplyr::select(team.name, position),
    by = c("Team" = "team.name")
  ) %>%
  rename(Ladder_pos_2024 = position) %>%
  relocate(Ladder_pos_2024, .after = Team) %>% 
  arrange(Ladder_pos_2024)

## plotting

heatmap_data <- prob_table %>%
  pivot_longer(cols = starts_with("Prob_"), 
               names_to = "Position", 
               values_to = "Probability") %>%
  mutate(
    Position = as.numeric(sub("Prob_", "", Position)),
    Team = factor(Team, levels = rev(prob_table$Team))  # 2024 ladder order
  )

heatmap_data <- heatmap_data %>%
  mutate(label = paste0(round(Probability * 100, 0), "%"))

ggplot(heatmap_data, aes(x = Position, y = Team, fill = Probability)) +
  geom_tile() +
  geom_text(aes(label = label), size = 2.5) + 
  scale_fill_gradient(low = "white", high = "red", guide = "none") +
  scale_x_continuous(breaks = 1:max(heatmap_data$Position)) +
  labs(
    title = "2025 Ladder Probabilities",
    subtitle = "Elo model",
    x = "Ladder Position",
    y = "Team\n(ordered by 2024 ladder)",
    fill = "Probability"
  ) +
  theme_minimal()

