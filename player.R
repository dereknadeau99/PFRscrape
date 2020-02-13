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
