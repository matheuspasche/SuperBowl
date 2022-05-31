library(tidyverse)
library(nycflights13)
library(lubridate)


flights <- nycflights13::flights %>% 
           mutate(yearmon = ym(paste0(year, month)),
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



teste <- flights %>% 
  with_groups(c(yearmon, dest), ~summarise(.,Count = n()))  %>% 
  with_groups(c(yearmon), ~filter(slice_max(., Count, n = 3)))  %>% 
  left_join(airports, c("dest"="faa")) 
  
  map_data("state")

map_data("state") %>% 
ggplot() +
  geom_polygon(mapping=aes(x=long,y=lat),color="white",fill="grey")+
  geom_point(data=teste, mapping = aes(x = lon, y = lat, size = dest, col = dest), 
             position = "jitter",size=2.5) + labs(color = "Average Delay")+
  facet_wrap(~yearmon)




flights2 <- mutate(flights, tot_delay = arr_delay + dep_delay)
flights2 <- flights2 %>% group_by(dest) %>% 
  summarise(avg_delay = mean(tot_delay, na.rm = T)) %>% 
  left_join(airports, c("dest"="faa")) %>%
  arrange(desc(avg_delay))
states = map_data("state")

g <- ggplot(data=states)
g2 <- g + geom_polygon(mapping=aes(x=long,y=lat,group=group),color="white",fill="grey")
g2 <- g2 + ggtitle("Map of the USA minus Alaska")
g2

flights3 <- filter(flights2, lon > -140)
g3 <- g2 + geom_point(data=flights3, mapping = aes(x = lon, y = lat, color = avg_delay), 
                      position = "jitter",size=2.5) + labs(color = "Average Delay") +
  scale_colour_gradient(low = "white",high = "dark red")
g3


flights3 <- filter(flights3, !is.na(avg_delay))
worst_flights <- head(flights3,5)
best_flights <- tail(flights3,5)

g4 <- g3 + geom_text(data=worst_flights, mapping=aes(x = lon, y = lat, label = dest), color="red", nudge_y = -0.5) 

g4 + geom_text(data=best_flights, mapping=aes(x = lon, y = lat , label = dest), angle=20,nudge_y=-0.5)
  



