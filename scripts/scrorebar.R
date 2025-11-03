library(tidyverse)

# data scrapping via https://github.com/IsabelleLefebvre97/PWHL-Data-Reference?tab=readme-ov-file#hockeytech-base-url
# schedule 
url <- "https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&view=scorebar&numberofdaysback=1000&numberofdaysahead=1000&key=446521baf8c38984&client_code=pwhl"

response <- GET(url)
data_json <- content(response, "text")
data_list <- fromJSON(data_json, flatten = TRUE)

# Check structure
str(data_list, max.level = 2)

# Extract
scorebar <- as.data.frame(data_list$SiteKit$Scorebar)

# clean

game_data <- scorebar %>% 
  janitor::clean_names() %>% 
  select(id, season_id, game_number, date, home_code, home_goals, visitor_code, visitor_goals,
         game_status_string_long, venue_name)
