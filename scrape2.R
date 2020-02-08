# housekeeping
rm(list = ls())
library(rvest)
library(dplyr)
library(scales)
library(tibble)
library(tidyverse)

get_data = function(year_range) {
  
  data = c()
  team = 'rav'
  
  for (year in year_range) {
    
    url = paste(
      'https://www.pro-football-reference.com/teams/', 
      team,
      '/',
      as.character(year),
      '.htm',
      sep = "")
    html = read_html(url)
    
    data = html_table(
      x = html, 
      header = TRUE
    ) %>% .[[2]] %>% as_tibble(.name_repair = 'minimal')
    
    for (i in 1:ncol(data)) {
      names(data)[i] = data[[i]]
    } data = data[ -c(1), ]
    
  }
  
  return(data)
  
}

data = get_data(2000)

