get_team_stats = function(year_range = NULL) {
  
  tryCatch({
    
    data = read.csv(file = file.path(getwd(), "/PFRscrape/data/teamstats.csv"))
    
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
    write.csv(data, file.path(getwd(), "/PFRscrape/data/teamstats.csv"), row.names = FALSE)
    
    n = 0;
    while (!file.exists(file.path(getwd(), "/PFRscrape/data/teamstats.csv")) & n < 10) {
      Sys.sleep(1)
      n = n+1
    }
    
    data = read.csv(file = file.path(getwd(), "/PFRscrape/data/teamstats.csv"))
    
  })
  
  return(data)
  
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
