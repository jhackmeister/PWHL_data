library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)
library(ggrepel)
library(datapasta)
library(gt)


# Define the base URL
url <- "https://lscluster.hockeytech.com/feed/index.php"

# Set query parameters
params <- list(
  feed = "statviewfeed",
  view = "players",
  season = 8,
  team = "all",
  position = "skaters",
  rookies = 0,
  statsType = "standard",
  rosterstatus = "undefined",
  site_id = 0,
  league_id = 1,
  lang = "en",
  division = -1,
  conference = -1,
  key = "446521baf8c38984",
  client_code = "pwhl",
  limit = 500,
  sort = "points"
)

# Make GET request
response <- GET(url, query = params)


raw_text <- content(response, "text")

# Remove leading "(" and trailing ")" if present
clean_text <- gsub("^\\(|\\)$", "", raw_text)

# Parse JSON
data <- fromJSON(clean_text, flatten = TRUE)

# Extract the nested data frame
players_df <- data$sections[[1]]$data[[1]]
