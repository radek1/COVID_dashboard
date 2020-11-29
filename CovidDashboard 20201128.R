library(tidyverse)
library(COVID19)
library(zoo) #for rolling average

start_date <- "2020-07-31"
x.3 <- covid19(c("US"), level = 3, start = start_date) #countries where we need level 3 data
x.1 <- covid19(c("Czech Republic", "Switzerland"), level = 1, start = start_date) #countries where we need level 1 data
x.2 <- covid19(c("United Kingdom", "India"), level = 2, start = start_date) %>% filter(administrative_area_level_2 %in% c("Scotland", "Telagana", "California")) #countries where we need level 2 data


x.3 <- x.3 %>% filter(administrative_area_level_3 %in% c("Santa Clara", "Alameda", "Contra Costa", "San Francisco", "San Mateo", "Marin", "Napa", "Sonoma", "Santa Cruz")) %>% 
  group_by(date) %>% 
  summarize(sum_confirmed = sum(confirmed), sum_population = sum(population), region = "Bay Area") %>% 
  mutate(daily_rate = 100000*c(NA, diff(sum_confirmed))/sum_population) %>% 
  select(date, daily_rate, sum_population, region)

x.2 %>% 
  group_by(administrative_area_level_2) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(., daily_cases = c(NA, diff(confirmed))))) %>% 
  unnest(data) %>% ungroup(administrative_area_level_2)  %>% 
  mutate(daily_rate = 100000*daily_cases/population, region = administrative_area_level_2) %>% 
  select(date, daily_rate, population, region) -> x.2
  
x.1 %>% 
  group_by(administrative_area_level_1) %>% 
  nest() %>% 
  mutate(data = map(data, ~mutate(., daily_cases = c(NA, diff(confirmed))))) %>% 
  unnest(data) %>% ungroup(administrative_area_level_1)  %>% 
  mutate(daily_rate = 100000*daily_cases/population, region = administrative_area_level_1) %>% 
  select(date, daily_rate, population, region) -> x.1

x.combined <- bind_rows(x.1, x.2, x.3)
x.combined <- x.combined %>% 
  group_by(region) %>% 
  mutate(average.7 = zoo::rollmean(daily_rate, k = 7, fill = NA))

ggplot(x.combined, aes(date, daily_rate, color = region)) +
    geom_line(alpha = 0.5) +
    geom_line(aes(date, average.7, color = region), size = 1) +
    ylab("daily cases per 100,000") +
    ggtitle("7-day rollling average of COVID-19 cases in selected regions") +
    theme_bw()
