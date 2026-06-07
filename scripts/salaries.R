library(tidyverse)
library(pdftools)

# read in the data from web
url <- "https://cdn.prod.website-files.com/65038094882d837d666bf1bb/69efbccd65fdec269086fc2c_PlayerSalaries_2025-2026.pdf"
text <- pdf_text(url)

# combine pages and split by line
lines <- text |>
  paste(collapse = "\n") |>
  str_split("\n") |>
  unlist() |>
  str_trim() |>
  # Remove blank lines and header rows
  (\(x) x[x != "" & !str_detect(x, "^(Player Salaries|First Name)")])()

# parse the lines into fields
# anchor on known teams to hanlde spaaces in team and player names
teams <- c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Seattle",
            "Toronto", "Vancouver")
team_pattern <- paste(teams, collapse = "|")

salaries_df <- tibble(raw = lines) |>
  mutate(
    team    = str_extract(raw, team_pattern),
    salary  = str_extract(raw, "[\\d,]+\\.\\d{2}$") |>
                parse_number(),
    name    = str_remove(raw, paste0("\\s*(", team_pattern, ")\\s+[\\d,]+\\.\\d{2}$")) |>
                str_squish()
  ) |>
  select(name, team, salary) |>
  filter(!is.na(team))  # drop any malformed rows

glimpse(salaries_df)

by_team <- salaries_df |> 
  group_by(team) |> 
  summarise(team_salary = sum(salary)) |> 
  arrange(team_salary)

ggplot(by_team, aes(x=reorder(team, -team_salary), y = team_salary, fill = team)) +
  geom_col()

library(googlesheets4)
gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1Yd2UnTm4fqr6-nrlZ6UrF54ky5s92fFjX_Da9BWM0fE/edit?gid=1205449064#gid=1205449064"
rs_2526 <- read_sheet(sheet_url, sheet = "25-26 RS")

# fix names between the two files 
name_fixes <- tribble(
  ~name_rs, ~name_salary,
  "Abby Boreen", "Abigail Boreen",
  "Jenn Gardiner", "Jennifer Gardiner",
  "Kristýna Kaltounková", "Kristyna Kaltounkova",
  "Casey O'Brien", "Casey O’Brien",
  "Gabbie Hughes", "Gabrielle Hughes",
  "Jessica DiGirolamo", "Jessica Digirolamo",
  "Abbey Levy", "Abigail Levy",
  "Mellissa Channell-Watkins", "Mellissa Channell Watkins",
  "Rylind MacKinnon", "Rylind Mackinnon"
)

rs_2526_fixed <- rs_2526 |>
  left_join(name_fixes, by = c("Name...1" = "name_rs")) |>
  mutate(Name...1 = coalesce(name_salary, Name...1)) |>
  select(-name_salary)

rs_2526_salaries <- left_join(rs_2526_fixed, salaries_df, by= c("Name...1" = "name")) |>
  rename(name = Name...1) |> 
  janitor::clean_names() |> 
  select(name, position, team, gs, salary) |> 
  mutate(salary_vs_avg = salary - mean(salary, na.rm = TRUE))

ggplot(rs_2526_salaries, aes(x=salary_vs_avg, y=gs, color = team)) +
  geom_point()
