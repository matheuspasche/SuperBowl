library(dplyr)
library(nycflights13)
library(purrr)
library(lubridate)












summarytable <- function(.data,var){
  .data %>% 
      group_split(month) %>% 
      map_df(~.x %>% 
              select( {{var}}) %>% 
              skimr::skim()
            )
}













teste <- flights %>% 
    filter(!is.na(air_time)) %>% 
    mutate(total_delay = dep_delay + arr_delay,
           time = lubridate::as_date(time_hour),
           yearmonth = floor_date(time, unit= 'month')) %>% 
    group_split(month) %>% 
    map_df(~.x %>% 
           select(month, total_delay) %>% 
           mutate(a = nest(skimr::skim(total_delay)))
          ) %>% 
    view()
  
    summarytable(var = total_delay) %>% 
    view()



percents <- function(.data, variable, probs){
  
  .data %>% 
  mutate("p.{probs}"  := quantile({{variable}}, probs = {{probs}}, na.rm = T))
}

#Gere um dataframe contendo o atraso médio, bem como o desvio-padrão e os percentis p25, p50, p90 e p99 para cada dia do ano.
p99 = flights %>%
        mutate(total_delay = dep_delay + arr_delay,
               time = lubridate::as_date(time_hour)) %>% 
        group_by(time) %>% 
        summarise(total_delay = sum(total_delay, na.rm = T),
                  mean_delay = mean(total_delay, na.rm = T),.groups = 'drop') %>% 
        percents(total_delay, probs = .99)
