# Assignment 3: Using Data
#
# Before you get started:
# - Set your working directory to "source file location" using the Session menu
# - Run the following line of code to delete all variables in your workspace
#     (This will make it easier to test your script)
rm(list = ls())
library(lintr)
library(styler)

### Built in R Data ###########################################################

# In this section, you'll work with the variable `Titanic`, a data set which is
# built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Hint: Be sure to **not** treat strings as factors!
titanic_df <- as.data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: which class the individual sits in, 1st/2nd/3rd/Crew
# Sex: individual's sex, Male/Female (categorical)
# Age: individuals general age, Child/Adult (categorical)
# Survived: individual's survival status, Yes/No (categorical)
# Freq: # of individuals that fit the row categorical descriptions (numerical)


# Create a variable `children` that is a data frame containing only the rows
# from `titanic_df` with information about children on the Titanic
# Hints:
# - Filter rows using a vector of boolean values (like vector filtering)
# - See chapter 10.2.3
children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: Remember the `sum()` function!
num_children <- sum(children$Freq)

# Create a variable `most_lost` that is the *row* from `titanic_df` with the
# largest absolute number of losses (people who did not survive)
# You can use multiple lines of code if you find that helpful
# to create this variable
# Hint: Filter for those who did not survive, then look for the row
most_lost_df <- titanic_df[titanic_df$Survived == "No", ]
most_lost <- most_lost_df[most_lost_df$Freq == max(most_lost_df$Freq), ]

# Define a function called `survival_rate()` that takes in two arguments which
# must be in *the following order*:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
#
# This function should return a sentence that states the *survival rate*
# (# survived / # in group) of adult men and "women and children" in that
# ticketing class.
# It should read (for example):
# >"Of Crew class, 87% of women and children survived and 22% of men survived."
#
# This is a complicated function! We recommend the following approach:
# - Filter for all rows representing the given ticketing class and save the
#   new data frame to a variable
# - Using this data frame, filter for all rows representing Adult Males
# - Find the total number of men and total number of male survivors to
#   calculate the survival rate
# - Likewise, use the data frame to filter for all Children and Adult Females
# - Perform the above calculation for this group as well
#
# Other approaches are also acceptable, please comment to explain what you do!
survival_rate <- function(class, df) {
  class_df <- df[df$"Class" == class, ]

  # Adult Male Statistics
  adult_men_df <- class_df[class_df$"Age" == "Adult"
  & class_df$"Sex" == "Male", ]
  total_men <- sum(adult_men_df$"Freq")
  m_survived <- sum(adult_men_df$"Freq"[adult_men_df$"Survived" == "Yes"])
  m_survival_rate <- round(m_survived * 100 / total_men)

  # Women and Children Statistics
  w_c_df <- class_df[class_df$"Age" == "Children"
  | class_df$"Sex" == "Female", ]
  total_w_c <- sum(w_c_df$"Freq")
  w_c_survived <- sum(w_c_df$"Freq"[w_c_df$"Survived" == "Yes"])
  w_c_survival_rate <- round(w_c_survived * 100 / total_w_c)

  # Return Statement
  return(paste0(
    "Of ", class, " class, ",
    w_c_survival_rate, "% of women and children survived and ",
    m_survival_rate, "% of men survived."
  ))
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# Survival rate decreases with the class ranking
# Third class has the lowest survival rate, first has highest

# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# On average significantly more men died than women/children
# In third class, men had higher survival rate than they did in 2nd class


### Reading in Data ###########################################################

# In this section, you'll work with .csv data of life expectancy by country
# First, download the csv file of `Life Expectancy` data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory

# Before getting started, explore the GapMinder website to better understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)
# For the years 1800-1970, Gapminder used data compiled by Mattias Lindgren
# into 100 sources, based on educated estimates of life expectancy based on
# fatal world events in history. For the years 1970-2016, Gapminder used data
# from the Global Burden of Disease Study, which was # conducted by the
# Institute for Health Metrics and Evaluation of the University of Washington
# in Seattle, WA. This survey was published in November 2018, and was accessed
# in October 2019. Finally, for 2017 and onward, Gapminder uses data from UN
# forecasts in the World Population Prospects 2019, which is published in the
# file with annually interpolated demographic indicators.

# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`. Make sure not to read strings as factors
life_exp <- read.csv("data/life_expectancy_years.csv", stringsAsFactors = FALSE)

# Write a function `get_col_mean()` that takes a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
# Hint: `mean()` takes in an argument called `na.rm`
get_col_mean <- function(column, df) {
  return(mean(df[[column]], na.rm = TRUE))
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
# Hint: Use an `*apply` function (lapply, sapply, etc.)
col_means <- sapply(
  colnames(life_exp)[2:302],
  get_col_mean,
  life_exp
)

# Create a variable `avg_diff` that is the difference in average country life
# expectancy between 1800 and 2018
avg_diff <- col_means["X2018"] - col_means["X1800"]

# Create a column `life_exp$change` that is the change in life
# expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
life_exp$change <- life_exp$X2018 - life_exp$X2000

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy. Make sure to filter NA values
# Hint: `max()` takes in an argument called `na.rm`
most_improved <- life_exp$country[max(life_exp$change, na.rm = TRUE)]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values
# Hint: Lookup `is.na()`
num_small_gain <- length(life_exp$country[!is.na(life_exp$change)
& life_exp$change < 1])

# Write a function `country_change()` that takes in a country's name,
# two years as numbers (not strings), and the `life_exp` data frame
# Parameters should be written *in the above order*
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
# Hint: Use an if/else statement to help compute DIRECTION
country_change <- function(name, year1, year2, df) {
  y1_col <- paste0("X", year1)
  y2_col <- paste0("X", year2)

  year1_value <- df[life_exp$"country" == name, y1_col]
  year2_value <- df[life_exp$"country" == name, y2_col]

  country_diff <- year2_value - year1_value
  if (country_diff < 0) {
    direction <- "down"
  } else {
    direction <- "up"
  }

  paste0(
    "Between ", year1, " and ", year2,
    ", the life expectancy in ", name,
    " went ", direction,
    " by ", abs(country_diff), " years."
  )
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", 1960, 1990, life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations)
# Hint: Use an if/else statement to paste the countries in the correct order
compare_change <- function(name1, name2, df) {
  name1_change <- round(df$change[df$"country" == name1], 1)
  name2_change <- round(df$change[df$"country" == name2], 1)
  country_compare <- name1_change - name2_change

  if (country_compare >= 0) {
    return(paste0(
      "The country with the bigger change in life expectancy was ",
      name1, " (gain=", name1_change,
      "), whose life expectancy grew by ", country_compare,
      " years more than ", name2, "'s (gain=", name2_change
    ))
  } else {
    return(paste0(
      "The country with the bigger change in life expectancy was ",
      name2, " (gain=", name2_change,
      "), whose life expectancy grew by ", -1 * country_compare,
      " years more than ", name1, "'s (gain=", name1_change, ")."
    ))
  }
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- compare_change("United States", "France", life_exp)

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "data/life_exp_with_change.csv", row.names = FALSE)
