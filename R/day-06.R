#Barush Martinez
# 08/11/2020
# create a graph that shows relation of state and covid cases

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
head(covid, 5)

covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(cases, n = 6) %>%
  pull(state) ->
  top_states

covid %>%
  filter(state %in% top_states) %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line(size = 2) +
  facet_wrap(~state) +
  theme(legend.position = "NA")+
  labs(title = "Cummulative Case Count Per State",
       subtitle = "Data Source: NY-Times",
       x = "Date",
       y = "Cases",
       caption = "Daily exercise 06/ Barush Martinez")


covid %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_col(fill = "gray85", color = "darkred", alpha = .25) +
  geom_line(color = "darkred", size = 3) +
  labs(title = "National Case Count",
       x = "Date",
       y = "Cases",
       caption = "Daily Exercise 06/Barush Martinez")





