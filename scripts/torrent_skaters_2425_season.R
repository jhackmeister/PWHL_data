###################################
### Torrent Players Last Season ###
###################################

# Run the skaters.R first 

players_df <- players_df %>% 
  mutate(torrent = if_else(player_id %in% roster_df_simple$player_id, 1, 0))

players_df %>%
  filter(position != "G") %>%
  ggplot(aes(x = shots, y = goals)) +
  geom_point(color = "gray80") +
  geom_point(data = filter(players_df, torrent == 1, position != "G"),
             aes(color = "Torrent"), size = 2) +
  geom_text_repel(data = filter(players_df, torrent == 1, position != "G"),
                  aes(label = name, color = "Torrent")) +
  scale_color_manual(values = c("Torrent" = "#074F51")) +
  labs(
    title = "PHWL Skaters by Goals and Shots Taken",
    subtitle = "2024-2025 Season",
    x = "Total Shots",
    y = "Goals",
    caption = "Torrent Players Highlighted"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

teams <- read.csv("data/pwhl_teams.csv")

feeder_teams <- players_df %>%
  filter(torrent == 1) %>%
  group_by(team_code) %>%
  summarise(count = n(), .groups = "drop") %>% 
  left_join(teams, by = c("team_code" = "code"))


ggplot(feeder_teams, aes(x = reorder(team_code, desc(count)), y = count, fill = team_code)) +
  geom_col() +
  scale_fill_manual(values = setNames(feeder_teams$primary_color, feeder_teams$team_code))+
  labs(
    title = "Previous Teams for Torrent Players",
    x = "",
    y = "Player Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Torrent Skaters from roster and skaters scripts 
torrent_skaters <- players_df %>% 
  filter(player_id %in% roster_df_simple$id) %>% 
  mutate(position = case_when(
    position %in% c("RW", "LW") ~ "F",
    position %in% c("LD", "RD") ~ "D",
    TRUE ~ position
  )) %>%
  mutate(time_on_ice_min = as.numeric(str_extract(ice_time_minutes_seconds, "^[0-9]+")) +
           as.numeric(str_extract(ice_time_minutes_seconds, "[0-9]+$")) / 60) %>% 
  mutate(across(c(faceoff_pct, shooting_percentage, points_per_game, plus_minus,
                  penalty_minutes_per_game, hits_per_game_avg), as.numeric))

position_colors <- c("C"="#8ab5c6", "F"="#6098AE","D"="#074F51")

torrent_skaters %>%
  filter(position != "G") %>%
  ggplot(aes(x = time_on_ice_min, y = points, color = position)) +
  geom_point() +
  scale_color_manual(values = position_colors) +
  geom_text_repel(aes(label = name)) +
  labs(
    title = "Torrent Players Points and Time on Ice",
    subtitle = "2024-2025 Season",
    color = "Position",
    x = "Time on Ice", 
    y = "Total Points"
  ) +
  theme_minimal()
  
torrent_skaters %>%
  filter(position != "G") %>%
  ggplot(aes(x = shots, y = goals, color = position)) +
  geom_point() +
  scale_color_manual(values = position_colors) +
  geom_text_repel(aes(label = name)) +
  labs(
    title = "Torrent Players Points and Games Played",
    subtitle = "2024-2025 Season",
    color = "Position",
    x = "Total Shots Taken", 
    y = "Goals"
  ) +
  theme_minimal()


torrent_skaters %>%
  filter(faceoff_attempts > 20) %>%
  ggplot(aes(x = reorder(name, faceoff_pct), y = faceoff_pct, fill = position)) +
  geom_col() +
  geom_text(aes(label = faceoff_attempts), 
            hjust = 1.5, size = 3) +  
  scale_fill_manual(values = position_colors) +
  coord_flip() + 
  labs(
    title = "Faceoff Attempts and Win Percentage (Players with 20+ Attempts)",
    subtitle = "2024-2025 Season",
    x = NULL, y = "Faceoff %", fill = "Position"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.position = "none")



