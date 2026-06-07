library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(ggrepel)


# data scrapping via https://github.com/IsabelleLefebvre97/PWHL-Data-Reference?tab=readme-ov-file#hockeytech-base-url

# schedule season ID is 8 for 2025-26 regular season (7 for preseason) 

url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&season_id=8&key=446521baf8c38984&client_code=pwhl"

response <- GET(url)
data_json <- content(response, "text")
data_list <- fromJSON(data_json, flatten = TRUE)

# Check structure
str(data_list, max.level = 2)

# Extract schedule
schedule <- as.data.frame(data_list$SiteKit$Schedule)

# Clean column names
schedule <- schedule %>% 
  janitor::clean_names() %>% 
  select(game_id, date_played, home_team_code, home_goal_count, visiting_team_code,
         visiting_goal_count, period, overtime, shootout, attendance, venue_name, notes_text) %>% 
  filter(attendance > 1) # keep only games played

calculate_points_tidyverse <- function(df) {
  df %>%
    mutate(
      # Convert numeric to logical
      ot_flag = as.logical(as.numeric(overtime)),
      so_flag = as.logical(as.numeric(shootout)),
      
      # Replace NA with FALSE
      ot_flag = replace_na(ot_flag, FALSE),
      so_flag = replace_na(so_flag, FALSE),
      
      # Determine if home team won
      home_win = home_goal_count > visiting_goal_count,
      
      # Calculate points
      home_points = case_when(
        home_win & !ot_flag & !so_flag ~ 3,
        home_win & (ot_flag | so_flag) ~ 2,
        !home_win & !ot_flag & !so_flag ~ 0,
        !home_win & (ot_flag | so_flag) ~ 1,
        TRUE ~ NA_real_
      ),
      
      away_points = case_when(
        !home_win & !ot_flag & !so_flag ~ 3,
        !home_win & (ot_flag | so_flag) ~ 2,
        home_win & !ot_flag & !so_flag ~ 0,
        home_win & (ot_flag | so_flag) ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-ot_flag, -so_flag, -home_win)
}

schedule_w_points <- calculate_points_tidyverse(schedule)


# PWHL Cumulative Points Chart by Games Played

# Function to calculate cumulative points by games played
calculate_cumulative_standings <- function(schedule_w_points) {

  # Option 1: If you have a date column (adjust column name as needed)
  if("date_played" %in% names(schedule_w_points) || "date_played" %in% names(schedule_w_points)) {
    schedule_sorted <- schedule_w_points %>%
      arrange()  # or arrange(game_date)
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

ytd_schedule <- calculate_cumulative_standings(schedule_w_points) %>% 
  mutate(
    cumulative_points = as.integer(cumulative_points),
    game_id = as.integer(game_id)
  )

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
            nudge_x = 0.1,
            hjust = 0,
            segment.size = 0.1,
            segment.alpha = 0.5,
            size = 4) +
  scale_color_manual(values = team_colors, name = "team", guide = "none") +
  #scale_x_continuous(breaks = seq(0, max(ytd_schedule$game_number), by = 1)) +
  #scale_y_continuous(breaks = seq(0, max(ytd_schedule$cumulative_points), by = 1)) +
  labs(
    title = paste0("2025-26 PWHL Standings as of ", format(Sys.Date(), "%m-%d-%y")),
    subtitle = paste0(n_distinct(ytd_schedule$game_id), " League Games Played"),
    x = "Games Played",
    y = "Cumulative Points"
  ) +
  theme_minimal()

n_distinct(ytd_schedule$game_id)


