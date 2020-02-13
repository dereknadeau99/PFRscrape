# housekeeping
rm(list = ls())
library(rvest)
library(dplyr)
library(scales)
library(tibble)
library(tidyverse)
source("C:/Users/Derek/Documents/R/PFRscrape/player.R")
source("C:/Users/Derek/Documents/R/PFRscrape/team.R")
teams = read.csv("C:/Users/Derek/Documents/R/PFRscrape/data/teams.csv", stringsAsFactors = FALSE)

glimpse(teams)

# data retrieval

get_data    = function(type, args) {
  
  return(get_team_stats(2002:2019))
  
}

# setup
team_data  = get_data()

# data analysis
scatterplot  = function(data, x, y) {
  
  p = ggplot(data = data, aes(
    x = as.numeric(x),
    y = as.numeric(y)
  )) + 
    geom_point(aes(
      fill = data$L
    )) +
    scale_x_continuous(
      breaks = c(0:16)
    ) +
    labs(
      x = enquo(x),
      y = enquo(y)
    )
  return(p)
}
getmode      = function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# user interaction
readline(prompt = "Enter team name: ") %>% find_team() %>% team_summary()
readline(prompt = "Enter player name: ") %>% get_player()
