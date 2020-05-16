# housekeeping
rm(list = ls())
library(rvest)
library(dplyr)
library(scales)
library(tibble)
library(tidyverse)
path = "C:/Users/Derek/Desktop/Coding/PFRscrape/"
source(file.path(path, "player.R"))
source(file.path(path, "team.R"))
source(file.path(path, "util.R"))
teams = read.csv(file.path(path, "data/teams.csv"), stringsAsFactors = FALSE)

## data retrieval
get_data    = function(type, args) {
  
  return(get_team_stats(2002:2019))
  
}

### code ###

teamstats = get_data()

############

## data analysis

scatterplot(teamstats, teamstats$W, teamstats$SoS)

## user interaction
# readline(prompt = "Enter team name: ") %>% find_team() %>% team_summary()
#readline(prompt = "Enter player name: ") %>% get_player()
