# housekeeping
rm(list = ls())
library(rvest)
library(dplyr)
library(scales)
library(tibble)
library(tidyverse)
source(file.path(getwd(), "/PFRscrape/player.R"))
source(file.path(getwd(), "/PFRscrape/team.R"))
source(file.path(getwd(), "/PFRscrape/util.R"))
teams = read.csv(file.path(getwd(), "/PFRscrape/data/teams.csv"), stringsAsFactors = FALSE)

### code ###

teamstats = get_data()

############

# data retrieval
get_data    = function(type, args) {
  
  return(get_team_stats(2002:2019))
  
}

# data analysis


# user interaction
readline(prompt = "Enter team name: ") %>% find_team() %>% team_summary()
readline(prompt = "Enter player name: ") %>% get_player()
