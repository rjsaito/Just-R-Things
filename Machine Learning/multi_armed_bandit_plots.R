pacman::p_load(dplyr, ggplot2, data.table)

experiment_plot = function(x, y){
    
  x_sim = data.frame(ver = "x", x, stringsAsFactors = F); names(x_sim)[2] = "outcome"
  y_sim = data.frame(ver = "y", y, stringsAsFactors = F); names(y_sim)[2] = "outcome"

  xy_sim = rbind(x_sim, y_sim) %>%
    group_by(round, ver) %>%
    summarise(count = n()) %>%
    arrange(ver, round) %>%
    data.table()

  round_max = group_by(xy_sim, by = ver) %>% summarise(max_round = max(round))
  
  if(any(round_max$max_round < 20)){
    missing_round = suppressWarnings(apply(round_max, 1, function(z){
      max_round = as.numeric(z[2]) + 1
      ver = z[1]
      if((max_round-1) < 20) {
        data.frame(round = max_round:20, ver = ver, count = 0)
      }
   }) %>% do.call(rbind, .))
  }
  
  pl <- ggplot(data = xy_sim, aes(round, count, fill = ver)) + 
    geom_area(aes(fill = ver), position = 'stack')
  pl
}

ABtest_sim = ABtest(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5))
experiment_plot(ABtest_sim$x, ABtest_sim$y)

eg_sim = mab_eg(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5))
experiment_plot(eg_sim$x, eg_sim$y)

ed_sim = mab_ed(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5))
experiment_plot(ed_sim$x, ed_sim$y)

ucb_sim = mab_ucb(rounds= 20, n = 200, p.crit = 0, p = c(.4, .5))
experiment_plot(ucb_sim$x, ucb_sim$y)
