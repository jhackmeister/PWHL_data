library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)
library(ggrepel)
library(datapasta)
library(gt)

# PWHL Cumulative Points Chart by Games Played

# Function to calculate cumulative points by games played
calculate_cumulative_standings <- function(schedule_w_points) {
  if("date_played" %in% names(schedule_w_points) || "date_played" %in% names(schedule_w_points)) {
    schedule_sorted <- schedule_w_points %>%
      arrange()
  } 
  
  # Process home team results
  home_results <- schedule_sorted %>%
    select(team = home_team_code, 
           points = home_points,
           game_id) %>%  # Keep game_id for ordering
    arrange(game_id)
  
  # Process away team results  
  away_results <- schedule_sorted %>%
    select(team = visiting_team_code,
           points = away_points,
           game_id) %>%
    arrange(game_id)
  
  # Combine all results
  all_results <- bind_rows(home_results, away_results) %>%
    arrange(game_id) %>%
    group_by(team) %>%
    mutate(
      game_number = row_number(),
      cumulative_points = cumsum(points)
    ) %>%
    ungroup()
  
  return(all_results)
}

ytd_schedule <- calculate_cumulative_standings(schedule_w_points)

team_colors <- c(
  "BOS" = "#183F35",
  "MIN" = "#262160", 
  "MTL" = "#862836",
  "NY" = "#00B9B3",
  "OTT" = "#A61D31",
  "TOR" = "#1468B3",
  "SEA" = "#074F51", 
  "VAN" = "#af6c45"
)

# Get the last point for each team for labeling
label_data <- ytd_schedule %>%
  group_by(team) %>%
  filter(game_number == max(game_number)) %>%
  ungroup()

ggplot(ytd_schedule, aes(x=game_number, y = cumulative_points,
                         color = team, group = team)) +
  geom_line(linewidth = 1.3)+
  geom_text_repel(data = label_data,
                  aes(label = team),
                  nudge_x = 0.2,
                  hjust = 0,
                  segment.size = 0.2,
                  segment.alpha = 0.5,
                  size = 4) +
  scale_color_manual(values = team_colors, name = "team", guide = "none") +
  scale_x_continuous(
    breaks = seq(0, max(ytd_schedule$game_number), by = 1)) +
  labs(
    title = "2025-26 PWHL Standings",
    x = "Games Played",
    y = "Cumulative Points"
  ) +
  theme_minimal()