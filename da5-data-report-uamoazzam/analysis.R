# setup
library(modeest)
library(dplyr)
library(knitr)
library(ggplot2)
library(plotly)
library(leaflet)

shooting_data <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# summary analysis
total_shootings <- nrow(shooting_data)
total_lives_lost <- sum(shooting_data$num_killed)

most_impacted_city <- shooting_data %>%
  group_by(city) %>%
  summarise(total_impact = sum(num_killed + num_injured)) %>%
  filter(total_impact == max(total_impact)) %>%
  pull(city)

people_impacted <- shooting_data %>%
  group_by(city) %>%
  summarise(total_impact = sum(num_killed + num_injured)) %>%
  filter(total_impact == max(total_impact)) %>%
  pull(total_impact)

most_shootings_one_day <- shooting_data %>%
  group_by(date) %>%
  summarise(shootings = n()) %>%
  filter(shootings == max(shootings)) %>%
  select(shootings)

most_shootings <- most_shootings_one_day[[1, 1]]

# summary table
month_summary <- shooting_data %>%
  mutate(
    date_format = as.Date(date, "%B %d, %Y"),
    month = format(date_format, "%B")
  ) %>%
  group_by(month) %>%
  summarize(
    number_shootings = n(),
    number_killed = sum(num_killed),
    number_injured = sum(num_injured),
    most_common_state = mfv1(state)
  ) %>%
  arrange(-number_shootings)

colnames(month_summary) <- c(
  "Month",
  "Number of Shootings",
  "Number Killed",
  "Number Injured",
  "Most Common State"
)

# specific shooting sample
obs_most_impact <- shooting_data %>%
  mutate(total_impact = num_killed + num_injured) %>%
  select(city, state, date, total_impact) %>%
  filter(total_impact == max(total_impact))

date_most_impact <- pull(obs_most_impact, date)
city_most_impact <- pull(obs_most_impact, city)
state_most_impact <- pull(obs_most_impact, state)
num_most_impact <- pull(obs_most_impact, total_impact)

# interactive map
shootings_map_data <- shooting_data %>%
  mutate(shooting_location = paste0(city, ", ", state)) %>%
  select(
    lat,
    long,
    date,
    shooting_location,
    num_killed
  )

shootings_map <- leaflet(shootings_map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
    popup = paste(
      "Date:", shootings_map_data$date, "<br>",
      "Location:", shootings_map_data$shooting_location, "<br>",
      "Casualties:", shootings_map_data$num_killed
    ),
    color = "red",
    stroke = FALSE,
    radius = 2 + shootings_map_data$num_killed,
    fillOpacity = 0.4
  )

shootings_map

# bar graph

shootings_by_state <- shooting_data %>%
  group_by(state) %>%
  summarise(num_shootings = n()) %>%
  arrange(-num_shootings) %>%
  top_n(10, num_shootings)

shootings_by_state$state <- reorder(
  shootings_by_state$state,
  shootings_by_state$num_shootings
)

state_shootings_bar_graph <- ggplot(
  shootings_by_state,
  aes(
    num_shootings,
    state
  )
) +
  geom_col(aes(
    group = 1,
    fill = num_shootings,
    text = paste("Number of Shootings: ", num_shootings)
  )) +
  labs(
    y = "US States",
    x = "Number of Shootings",
    title = "Top 10 US States by Number of Shootings"
  )

ggplotly(state_shootings_bar_graph, tooltip = "text")
