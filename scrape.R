# housekeeping
rm(list = ls())
library(rvest)
library(dplyr)
library(scales)
library(tibble)
library(tidyverse)
source(file = paste('C:/Users/Derek/Documents/R/PFR/search_types.R', sep = ""), )

# data retrieval
get_team_stats = function(year_range = NULL) {
  
  tryCatch({
    
    data = read.csv(file = file.path("C:/Users/Derek/Documents/R/PFR/teamstats.csv"))
    
  }, warning = function(err) {
    
    AFC = c()
    NFC = c()
    
    if (is.null(year_range)) { year_range = 2002:2019 }
    
    for (year in year_range) {
      url = paste('https://www.pro-football-reference.com/years/', as.character(year), '/', sep = "")
      html = read_html(url)
      
      temp = (html_table(x = html, header = TRUE) %>% .[[1]] %>% as_tibble())[ -c(1,6,11,16), ]
      AFC = format_data(AFC, temp, year)
      
      temp = (html_table(x = html, header = TRUE) %>% .[[2]] %>% as_tibble())[ -c(1,6,11,16), ]
      temp %>% add_column(year = year)
      NFC = format_data(NFC, temp, year)
    }
    
    data = rbind(AFC, NFC)
    write.csv(data, file.path(getwd(), "/R/Webscraping/teamstats.csv"), row.names = FALSE)
    
    n = 0;
    while (!file.exists(file.path(getwd(), "/R/Webscraping/teamstats.csv")) & n < 10) {
      Sys.sleep(1)
      n = n+1
    }
    
    data = read.csv(file = file.path(getwd(), "/R/Webscraping/teamstats.csv"))
    
  })
  
  return(data)
  
}

get_player = function(name = NULL, link = NULL) {
  
  if (!is.null(link)) {
    url = paste('https://www.pro-football-reference.com/', link, sep="")
  } else if (!is.null(name)) {
    # split name
    name = strsplit(name, " ")
    
    # form url
    url = paste(
      'https://www.pro-football-reference.com/players/',
      substring(name[[1]][2], 1, 1),
      '/',
      substring(name[[1]][2], 1, 4),
      substring(name[[1]][1], 1, 2),
      '00.htm',
      sep = ""
    )
  } 
  
  # get data from web
  html = read_html(url)
  ## try table #1 first
  data = html_table(
    x = html,
    header = TRUE
  ) %>% .[[1]] %>% as_tibble(
    .name_repair = "minimal"
  )
  
  # remove the vague header rows
  if (colnames(data)[1] != "Year") {
    for (i in 1:ncol(data)) {
      colnames(data)[i] = data[[i]][1] 
    }
    data = data[-1, ]
  }
  
  ## if not the correct table, try #2
  if (colnames(data)[1] != "Year") {
    data = html_table(
      x = html,
      header = TRUE
    ) %>% .[[2]] %>% as_tibble(
      .name_repair = "minimal"
    )
    
    # remove the vague header rows
    if (colnames(data)[1] != "Year") {
      for (i in 1:ncol(data)) {
        colnames(data)[i] = data[[i]][1] 
      }
      data = data[-1, ]
    }
  }
  
  # get name from webpage and add column for it
  nodes = html %>% html_nodes('h1') 
  name = html_text(nodes)
  
  
  # "Yds" may be duplicated for some positions
  colnames(data) = make.unique(colnames(data))
  
  # remove Career and Team rows
  rm_rows = c()
  for (i in 1:nrow(data)){
    if (nchar(data$Age[i]) > 2 || data$Year[i] == "") {
      rm_rows = c(rm_rows, i)
    }
  } 
  data = data[-rm_rows, ]
  
  # fix column datatypes
  data$No. = as.integer(data$No.)
  
  # add columns
  data = mutate(data, Name = name)
  data = mutate(data, ProBwl = FALSE)
  data = mutate(data, FTAP = FALSE)
  data = mutate(data, W = 0)
  data = mutate(data, L = 0)
  data = mutate(data, T = 0)
  
  # column by column fixes / additions
  for (i in 1:nrow(data)) {
    
    # fix year and get probowl/1stteam
    if (grepl(x = data$Year[i], pattern = "*", fixed = TRUE)) {
      data$ProBwl[i] = TRUE
    }
    if (grepl(x = data$Year[i], pattern = "+", fixed = TRUE)) {
      data$FTAP[i] = TRUE
    }
    data$Year[i] = as.integer(substring(data$Year[i], 1, 4))
    transform(data, Year = as.integer(Year))
    
    # # get wins/losses/ties
    # for (i in 1:length(colnames(data))) {
    #   if (colnames(data)[i] == "QBrec") {
    # 
    #   }
    # }
    # 
    # if ("QBrec" %in% names(data)) {
    #   if (data$QBrec[i] == "") {data$QBrec[i] = "0-0-0"}
    #   record = strsplit(data$QBrec[i], "-")
    #   data$W[i] = as.integer(record[[1]][1])
    #   data$L[i] = as.integer(record[[1]][2])
    #   data$T[i] = as.integer(record[[1]][3])
    # }
    
  }
  
  return(data)
  
}

#url_list = get_all_player_urls()


get_all_player_info = function() {
  players  = c()
  bad_urls = c()
  
  for (i in 1:length(url_list)) {
      
      tryCatch({
        players = bind_rows(players, get_player(link = url_list[i]))
        print(i)
      }, error = function(err) {
        print(paste(i, "skipped -", err))
        bad_urls = c(bad_urls, url_list[i])
      })
      
  }
  
  write.csv('C:/Users/Derek/Documents/R/PFR/bad_urls.csv', row.names = FALSE)
  
  return(players)
  
}

all_player_info = get_all_player_info()

#






















get_all_player_urls = function() {
  
  url_array = c()
  for (letter in LETTERS) {
    page <- read_html(paste("https://www.pro-football-reference.com/players/", letter, "/", sep = ""))
    nodes = page %>% html_nodes('#div_players') 
    nodes = strsplit(toString(nodes), "<p>")[[1]]
    
    for (i in 2:length(nodes)) {
      s = regmatches(nodes[i], gregexpr('"[^"]*"', nodes[i]))[[1]]
      s = substring(s, 3, nchar(s)-1)
      url_array = c(url_array, s)
    }
  }
  
  return(url_array)
  
}


get_data    = function(type, args) {
  
  return(get_team_stats(2002:2019))
  
}
format_data = function(data, temp, year) {
  
  #fix format
  temp$SoS = as.numeric(temp$SoS)
  temp$W = as.integer(temp$W)
  temp$L = as.integer(temp$L)
  temp$'W-L%' = as.numeric(temp$'W-L%')
  temp$PD = as.integer(temp$PD)
  temp$MoV = as.numeric(temp$MoV)
  temp$PF = as.integer(temp$PF)
  temp$PA = as.integer(temp$PA)
  temp$DSRS = as.numeric(temp$DSRS)
  temp$SRS = as.numeric(temp$SRS)
  temp$OSRS = as.numeric(temp$OSRS)
  
  #fix ties
  if (('T' %in% colnames(temp))) { temp = temp[-c(4)] }
  temp = mutate(temp, year = year)
  temp = add_data(temp)
  colnames(temp)[4] = "WPer"
  
  return(rbind(data, temp))
  
}
add_data    = function(data) {
  
  data = mutate(data, conf = "")
  data = mutate(data, div = "")
  data = mutate(data, div_champ = FALSE)
  data = mutate(data, wildcard = FALSE)
  data = mutate(data, T = 0)
  
  for (i in 1:nrow(data)) {
    
    if (grepl(x = data$Tm[i], pattern = "*", fixed = TRUE)) {
      data$div_champ[i] = TRUE
    } else if (grepl(x = data$Tm[i], pattern = "+", fixed = TRUE)) {
      data$wildcard[i] = TRUE
    }
    
    data$T[i] = (16 - data$W[i] - data$L[i])
    
    for (t in 1:nrow(teams)) {
      
      if (grepl(x = data$Tm[i], pattern = teams$name[t], fixed = TRUE)) {
        data$conf[i] = teams$conf[t]
        data$div[i]  = teams$div[t]
        data$Tm[i] = teams$abbr[t]
      }
      
    }
    
  }
  
  return(data)
  
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
team_summary = function(name = NULL) {
  
  if (!is.null(name)) { 
  
    team = filter(data, data$Tm == name)
    
    for (i in 1:32) {
      if (teams$abbr[i] == name) {
        print(paste(teams$abbr[i], " - ", teams$name[i]))
        
        print(paste("Average wins:", as.character(format(mean(team$WPer)*16, digits = 3))))
        print(paste("Division Championships: ", as.character(sum(team$div_champ))))
        print(paste("Playoff appearances: ", as.character(sum(team$div_champ, team$wildcard))))
        
        break()
      }
    }
  
  }
  
}

# util
find_team = function(s = NULL) {
  
  if (!is.null(s)) {
  
    for (i in 1:32) {
      if (grepl(s, teams$name[i], ignore.case = TRUE) || grepl(s, teams$abbr[i], ignore.case = TRUE)) {
        return(teams$abbr[i])
      }
    }
    
  }
  
  print("Team could not be found.")
  
}

# user interaction
readline(prompt = "Enter team name: ") %>% find_team() %>% team_summary()
readline(prompt = "Enter player name: ") %>% get_player()
