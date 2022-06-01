library(tidyverse)
library(nycflights13)
library(lubridate)


flights <- nycflights13::flights %>% 
           mutate(yearmon = ym(paste0(year, month)),
                  time = as_date(time_hour),
                  total_delay = arr_delay + dep_delay,
                  status = case_when(total_delay > 0 ~ 'delay',
                                     total_delay <0~ 'early',
                                     total_delay ==0 ~'in time')
                  )

flights %>% 
  filter(!is.na(status)) %>% 
  with_groups(c(yearmon), ~summarise(.,Count = n())) %>% 
  ggplot(aes(x = yearmon, y = Count, label = Count, vjust = 1))+
  geom_bar(stat = 'identity', fill = pilot::pilot_color(2))+
  pilot::theme_pilot()+
  pilot::geom_text_pilot()
  
    

#
# Interessante que não apenas a % de voos atrasados aumenta, mas a quantidade absoluta! 
# Não esperava por isso
flights %>% 
  with_groups(c(yearmon, status), ~summarise(.,Count = n())) %>% 
  filter(!is.na(status)) %>% 
  ggplot(aes(x = yearmon, y = Count, col = status))+
  geom_line(size = 1 )+
  pilot::theme_pilot()
 

# 0s atrasos nsao maiores nos meses de junho, julho e dezembro. O destino muda? 

flights %>% 
  with_groups(c(yearmon, status), ~summarise(.,Count = n())) %>% 
  filter(!is.na(status)) %>% 
  with_groups(c(yearmon), ~mutate(.,Part = Count/sum(Count))) %>% 
  ggplot(aes(x = yearmon, y = Part, fill = status))+
  geom_bar(stat = 'identity')+
  pilot::theme_pilot()+
  pilot::scale_fill_pilot()




stats <- function(.data, variable) {
  .data %>%
    summarise(
      count = n(),
      mean = mean({{variable}}),
      median = median({{variable}}),
      sd = sd({{variable}}),
      var = var({{variable}}),
      'p.01'=quantile({{variable}}, probs = .1),
      'p.025'=quantile({{variable}}, probs = .25),
      'p.09'=quantile({{variable}}, probs = .9),
      'p.99'=quantile({{variable}}, probs = .99)
    )
  
}

bymonth = flights %>% 
  filter(!is.na(air_time), status == 'delay') %>% 
  mutate(month_year = floor_date(time, 'month'))

bymonth%>% 
  with_groups(month_year, ~stats(., variable = total_delay)) %>% 
  knitr::kable(caption = 'Registros de estatísticas descriticas por mês', digits=1)


bymonth %>% 
  ggplot(aes(x= month_year, y = total_delay, col= carrier))+
  geom_point()
  
  
  