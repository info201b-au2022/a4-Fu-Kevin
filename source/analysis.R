library(tidyverse)
library(usmap)
library(dplyr)
library(ggplot2)
library(scales)


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
#The country level incarceration trend data
country_level <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#The jurisdiction level incarceration trend data
jurisdiction_level <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

#The proportion of total black prisoners in 2018
blackprop_2018 <- country_level %>% filter(year == 2018) %>%
    summarise(proportion = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE)) %>%
    pull(proportion)

#The proportion of total black prisoners in 1998 
blackprop_1998 <- country_level %>% filter(year == 1998) %>%
  summarise(proportion = sum(black_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)

#The proportion of total white prisoners in 2018
whiteprop_2018 <- country_level %>% filter(year == 2018) %>%
  summarise(proportion = sum(white_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)

#The proportion of total white prisoners in 1998
whiteprop_1998 <- country_level %>% filter(year == 1998) %>%
  summarise(proportion = sum(white_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)

#The proportion of total Latin prisoners in 2018
latinprop_2018 <- country_level %>% filter(year == 2018) %>%
  summarise(proportion = sum(latinx_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)

#The proportion of total Latin prisoners in 1998
latinprop_1998 <- country_level %>% filter(year == 1998) %>%
  summarise(proportion = sum(latinx_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)

#The proportion of total Asian prisoners in 1998
asianprop_2018 <- country_level %>% filter(year == 2018) %>%
  summarise(proportion = sum(aapi_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)

#The proportion of total Asian prisoners in 1998
asianprop_1998 <- country_level %>% filter(year == 1998) %>%
  summarise(proportion = sum(aapi_jail_pop, na.rm = TRUE)  / sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(proportion)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here
#----------------------------------------------------------------------------#
# This function produce a data frame that shows prison population growth in U.S.
get_year_jail_pop <- function() {
  population_data <- country_level %>%
    group_by(year) %>%
    select(year,total_jail_pop) %>%
    summarise(total = sum(total_jail_pop, na.rm = TRUE))
  return(population_data)   
}

# This function produce a chart that shows prison population growth in U.S.
plot_jail_pop_for_us <- function()  {
  jail_plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total)) +
    xlab("Year") + ylab("Total Jail Population") + 
    labs(title = "Increase of Jail Population in U.S.(1970-2018)",
         caption = "Figure 1. Increase of Jail Population in U.S.(1970-2018).") +
    scale_y_continuous(labels = c("0", "200,000", "400,000", "600,000", "800,000"))
  return(jail_plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here
# See Canvas
#----------------------------------------------------------------------------#
# This function produce a data frame that shows the U.S. prison population by one or more states
get_jail_pop_by_states <- function(states){
  state_population <- country_level %>%
    group_by(year, state) %>%
    filter(state %in% states) %>%
    select(year, total_jail_pop, state) %>%
    summarise(total = sum(total_jail_pop, na.rm = TRUE))
  return(state_population)
}

# This function produce a chart that shows the U.S. prison population by one or more states
plot_jail_pop_by_states <- function(states){
  states_jail_plot <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total, color = state)) +
    xlab("Year") + ylab("Total Jail population") +
    labs(title = "Increase of Jail Population in U.S. by state(1970-2018)",
         caption = "Figure 2. Increase of Jail Population in U.S. by state(1970-2018)")
  return(states_jail_plot)   
} 

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Comparison of proportion of African American in Prison and the whole population
# Your functions might go here 
# See Canvas
#----------------------------------------------------------------------------#
get_black_prop_for_us <- function(){
  black_proportion <- country_level %>%
    group_by(year) %>%
    select(year, black_jail_pop, black_pop_15to64, total_jail_pop, total_pop_15to64) %>%
    summarise(prop_black_jail = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
              prop_black_pop = sum(black_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE))
  return(black_proportion)
}

plot_black_prop_for_us <- function(){
  black_prop_us <- ggplot(get_black_prop_for_us()) +
    geom_line(aes(x = year, y = prop_black_jail, colour = "prop_black_jail")) +
    geom_line(aes(x = year,y = prop_black_pop, colour = "prop_black_pop")) +
    scale_color_manual(name = "Proportions", values = c("prop_black_jail" = "blue",
                                                      "prop_black_pop" = "red")) +
    xlab("Year") + ylab("Proportion of the population") + 
    labs(title = "Comparison of proportion of African American in Prison and the whole population",
         caption = "Figure 3.Comparison of proportion of African American in Prison and the whole population")
  return(black_prop_us)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Map of African American Proportion in Prison by selected year
# Your functions might go here
# See Canvas
#----------------------------------------------------------------------------#
get_black_prop_year <- function(years){
  state_prop <- country_level %>%
    group_by(state) %>%
    filter(year == years) %>%
    select(year, total_jail_pop, state, black_jail_pop) %>%
    summarise(total = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE))
  return(state_prop)
}

plot_black_prop_year <- function(years){
  black_prop_map <- plot_usmap(data = get_black_prop_year(years), 
                                values = "total", color = "white") + 
    theme(legend.position = "right") +
    scale_fill_continuous(low = "#ADD8E6", high = "#1c2e4a", 
                          name = paste("African American proportion in Jail in", years),
                          label = scales ::comma)+
    labs(title = paste("Figure 4.African American proportion in Jail in", years),
         caption = paste("Figure 4.African American proportion in Jail in", years))
  return(black_prop_map)
}
## Load data frame ---- 


