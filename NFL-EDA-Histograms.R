---
  title: "NFL Project"
output: html_document
date: '2022-06-13'
---
  
# Load all regular season passes from the 2021 regular season:
library(tidyverse)
library(nflreadr)
nfl_2021_data <- nflreadr::load_pbp(2021, file_type = "rds")

nfl_passing_plays <- nfl_2021_data %>%
  filter(play_type == "pass", season_type == "REG", 
         !is.na(epa), !is.na(posteam), posteam != "") %>%
  select(# Player info attempting the pass:
    passer_player_name, passer_player_id, posteam, 
    # Info about the pass:
    complete_pass, interception, yards_gained, touchdown, 
    pass_location, pass_length, air_yards, yards_after_catch, epa, wpa,
    shotgun, no_huddle, qb_dropback, qb_hit, sack,
    # Context about the receiver:
    receiver_player_name, receiver_player_id	,
    # Team context:
    posteam, defteam, posteam_type, 
    # Play and game context:
    play_id, yardline_100, side_of_field, down, qtr, play_clock,
    half_seconds_remaining, game_half, game_id,
    home_team, away_team, home_score, away_score,
    # Description of play
    desc)
head(nfl_passing_plays)

## HYPOTHESIS: HOW DOES THE NUMBER OF PASS COMPLETIONS VARY AS A TEAM GETS CLOSER TO HALF TIME / END OF THE GAME?

##HALF 1 PASS COMPLETIONS

first200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 1600) %>%
  mutate(TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(first200)

second200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 1400 &   half_seconds_remaining < 1600) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(second200)

third200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 1200 &   half_seconds_remaining < 1400) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(third200)

forth200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 1000 &   half_seconds_remaining < 1200) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(forth200)

fifth200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 800 &   half_seconds_remaining < 1000) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(fifth200)

sixth200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 600 &   half_seconds_remaining < 800) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(sixth200)

seventh200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 400 &   half_seconds_remaining < 600) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(seventh200)

eighth200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining > 200 &   half_seconds_remaining < 400) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(eighth200)

ninth200 <- nfl_passing_plays %>%
  filter(game_half == "Half1", half_seconds_remaining < 200) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(ninth200)
```
## HALF 2 PASS COMPLETIONS

first200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 1600) %>%
  mutate(TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(first200)

second200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 1400 &   half_seconds_remaining < 1600) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(second200)

third200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 1200 &   half_seconds_remaining < 1400) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(third200)

forth200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 1000 &   half_seconds_remaining < 1200) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(forth200)

fifth200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 800 &   half_seconds_remaining < 1000) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(fifth200)

sixth200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 600 &   half_seconds_remaining < 800) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(sixth200)

seventh200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 400 &   half_seconds_remaining < 600) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(seventh200)

eighth200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining > 200 &   half_seconds_remaining < 400) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(eighth200)

ninth200 <- nfl_passing_plays %>%
  filter(game_half == "Half2", half_seconds_remaining < 200) %>%
  mutate( TotalCompletions = sum(complete_pass)) %>%
  select(TotalCompletions, game_half, half_seconds_remaining)
head(ninth200)

## ATTEMPT TO CREATE TABLE

Half1 <- nfl_passing_plays %>%
  mutate(TotalCompletions = sum(complete_pass)) %>%
  filter(game_half == "Half1") %>%
  select(TotalCompletions, half_seconds_remaining)
hist(x = Half1$half_seconds_remaining, main = "Passes Completed in Half 1", xlab = "Seconds Remaining", ylab = "Passes Completed")

Half1 <- nfl_passing_plays %>%
  filter(game_half == "Half1") %>%
  select(half_seconds_remaining, complete_pass)

Half1 %>%
  filter(complete_pass == "1") %>%
  ggplot(aes(x=half_seconds_remaining)) +
  geom_histogram(center = 1700, binwidth = 200, fill = "pink") +
  scale_x_reverse(breaks = seq(1800, 0, by = -200)) +
  geom_vline(xintercept = 900) +
  labs(x = "Seconds Remaining", y = "Complete Passes",
       title = "Passes Completed in Half 1",
       caption = "Data courtesy of NFL") +
  theme_bw() 

Half2 <- nfl_passing_plays %>%
  filter(game_half == "Half2") %>%
  select(half_seconds_remaining, complete_pass)

Half2 %>%
  filter(complete_pass == "1") %>%
  ggplot(aes(x=half_seconds_remaining)) +
  geom_histogram(center = 1700, binwidth = 200, fill = "pink") +
  scale_x_reverse(breaks = seq(1800, 0, by = -200)) +
  geom_vline(xintercept = 900) +
  labs(x = "Seconds Remaining", y = "Complete Passes",
       title = "Passes Completed in Half 2",
       caption = "Data courtesy of NFL") +
  theme_bw() 
