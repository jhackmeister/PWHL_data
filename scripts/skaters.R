library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)

# Season 5 
url <- "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season=5&team=all&position=skaters&rookies=0&statsType=standard&rosterstatus=undefined&site_id=0&league_id=1&lang=en&division=-1&conference=-1&key=446521baf8c38984&client_code=pwhl&league_id=1&limit=500&sort=points"

# Make the GET request
response <- GET(url)

# Get the raw text
data_text <- content(response, "text", encoding = "UTF-8")

# Check if it's JSONP (wrapped in parentheses or a function call)
# Remove JSONP wrapper if present
if (grepl("^\\w+\\(", data_text)) {
  # Remove function name and opening parenthesis
  data_text <- sub("^\\w+\\(", "", data_text)
  # Remove closing parenthesis and possible semicolon
  data_text <- sub("\\);?$", "", data_text)
} else if (grepl("^\\(", data_text)) {
  # Just wrapped in parentheses
  data_text <- sub("^\\(", "", data_text)
  data_text <- sub("\\)$", "", data_text)
}

# Now parse the clean JSON
data_list <- fromJSON(data_text, flatten = TRUE)

# View the structure
str(data_list, max.level = 2)

# If the data contains player information, it's likely in a nested structure
# You might need to extract it like this:
if ("sections" %in% names(data_list)) {
  # Extract player data (adjust based on actual structure)
  players <- data_list$sections[[1]]$data
  
  # Convert to dataframe if needed
  if (!is.null(players)) {
    players_df <- as.data.frame(players)
    
    # View first few rows
    head(players_df)
    
    # Check column names
    names(players_df)
  }
}

players_df <- players_df %>%
  rename_with(~ gsub("^row\\.", "", .x)) %>% 
  rename_with(~ gsub("^prop\\.", "", .x))

# create rankings
players_df <- players_df %>%
  select(-c(1:6)) %>% 
  mutate(across(
    .cols = where(~ all(grepl("^\\d+$", .x) | is.na(.x))),
    .fns = as.numeric
  )) %>% 
  mutate(
    goal_rank = rank(-goals, ties.method = "min"),
    asst_rank = rank(-assists, ties.method = "min"),
    points_rank = rank(-points, ties.method = "min"), 
    pim_rank = rank(-penalty_minutes, ties.method = "min")
  ) 

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
