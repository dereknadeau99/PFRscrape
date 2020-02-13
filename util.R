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