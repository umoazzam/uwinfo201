# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library(dplyr)

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):

# Load your data, making sure to not interpret strings as factors.
projects_df <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?
colnames(projects_df)
nrow(projects_df)
ncol(projects_df)

# Use the `summary` function to get some summary information
summary(projects_df)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(col_name, df) {
  df_column <- df[[col_name]]
  if (is.numeric(df_column)) {
    return(list(
      min_col = min(df_column),
      max_col = max(df_column),
      mean_col = mean(df_column)
    ))
  } else if (n_distinct(df_column) < 10) {
    return(list(
      n_values = n_distinct(df_column),
      unique_values = unique(df_column)
    ))
  } else {
    return(list(
      n_values = n_distinct(df_column),
      sample_values = sample(df_column, size = 10)
    ))
  }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
backers_col_case_1 <- get_col_info("backers", projects_df)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(df) {
  return(sapply(colnames(df), get_col_info, df))
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
projects_df_summary <- get_summary_info(projects_df)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# The average pledge was about 1/5th of the average goal
# How often was the pledge greater than the goal?
# Which category was the most successful?

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
max_goal_project <- projects_df %>%
  filter(usd_goal_real == max(usd_goal_real)) %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?
cat_min_goal <- projects_df %>%
  filter(usd_goal_real == min(usd_goal_real)) %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
num_projects_2018 <- projects_df %>%
  select(deadline) %>%
  filter(as.numeric(format(as.Date(deadline, "%Y-%m-%d"), "%Y")) == 2018) %>%
  nrow()

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
unsuccessful_rate <- projects_df %>%
  select(state) %>%
  filter(state != "successful") %>%
  nrow() / nrow(projects_df)

# What was the amount pledged for the project with the most backers?
most_backed_project_pledges <- projects_df %>%
  filter(backers == max(backers)) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
failed_highest_pledge <- projects_df %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(name)


# How much total money was pledged to projects that weren't marked successful?
total_unsuccessful_pledges <- projects_df %>%
  filter(state != "successful") %>%
  select(usd_pledged_real) %>%
  sum()

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
most_pledged_category <- projects_df %>%
  group_by(category) %>%
  summarise(max_pledges = max(usd_pledged_real)) %>%
  filter(max_pledges == max(max_pledges)) %>%
  pull(category)

# Which country had the most backers?
most_backed_country <- projects_df %>%
  group_by(country) %>%
  summarise(max_backers = max(backers)) %>%
  filter(max_backers == max(max_backers)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
most_pledged_year <- projects_df %>%
  mutate(year = as.numeric(format(as.Date(deadline, "%Y-%m-%d"), "%Y"))) %>%
  group_by(year) %>%
  summarise(max_pledge = max(usd_pledged_real)) %>%
  filter(max_pledge == max(max_pledge)) %>%
  pull(year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from:
# I chose deadline because it made more sense to me to count the pledges from
# the deadline of the product compared to when it first launched, that way
# each year was based on total pledges received for completed campaigns

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3_categories <- projects_df %>%
  mutate(year = as.numeric(format(as.Date(deadline, "%Y-%m-%d"), "%Y"))) %>%
  filter(year == 2018) %>%
  group_by(main_category) %>%
  summarise(max_backers = max(backers)) %>%
  top_n(3, max_backers) %>%
  arrange(-max_backers) %>%
  pull(main_category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
common_launch_day <- projects_df %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarise(launch_count = n()) %>%
  filter(launch_count == max(launch_count)) %>%
  pull(day)

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
least_successful_day <- projects_df %>%
  mutate(day = weekdays(as.Date(launched))) %>%
  group_by(day) %>%
  summarise(
    num_projects = n(),
    num_success = sum(state == "successful")
  ) %>%
  mutate(success_rate = num_success / num_projects) %>%
  filter(success_rate == min(success_rate)) %>%
  pull(day)
