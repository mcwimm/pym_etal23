wd = "/path/to/hpc/access/point/"
library(tidyverse)
library(data.table)

fs = list.files(wd)

trees = data.frame()
for (f in fs) {
    s = strsplit(f, "_")[[1]]
    
    bg = s[1]
    sal = s[5]
    seed = tail(s, 1)
    recruits_ts = tail(s, 3)[1]
    
    if ("RandomGrowth" %in% s){
    mort = "RandomGrowth"
    }   else {
    mort = "NoGrowth"
    }
  
    t = fread(paste(wd, f, "Population.csv", sep = "/"))
    t = t %>%
        mutate(year = time / 3600 / 24 / 365.25) %>%
        filter(!is.na(growth)) %>% 
        group_by(year) %>%
        mutate(tdens = n() / 30 / 30 * 10000) %>% ungroup() %>%
        group_by(tree) %>%
        mutate(
          recruit = min(year),
          max_age = max(year),
          age = year - recruit
        ) %>% ungroup() %>%
        mutate(
          dbh = 200 * r_stem,
          height = h_stem + 2 * r_crown,
          slenderness = height / dbh
        ) %>%
        filter(year > 1400) %>%
        mutate(seed = seed,
               sal = sal,
               bg = bg, 
               mort = mort,
               recruits_ts = recruits_ts)
    trees = rbind(trees, t)
}

trees$bg_red_factor = round(as.numeric(trees$salinity) * 1000, 0)

save(trees, file = paste(wd, "trees_FS.rda", sep = "/"))


