# load libraries
library(tidyverse)
library(conflicted)

#how to handle overloaded functions
conflict_prefer("pluck","purrr")
conflict_prefer("filter","dplyr")

# loading data 
tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

# Getting winnigest conferences
winnigest<-tournament%>%
  filter(tourney_finish=="Champ")%>%
  count(conference)%>%
  top_n(10) %>%
  pull(conference)

# Getting cummulative sum per conference for each year
ncaaw_cum<-tournament%>%
  filter(tourney_finish=="Champ")%>%
  count(year, conference) %>%
  complete(year, conference, fill = list(n = 0)) %>%
  arrange(year) %>%
  group_by(conference) %>%
  mutate(cum_wins = cumsum(n)) %>%
  ungroup() %>%
  filter(conference %in% winnigest) %>%
  mutate(conference = str_replace(conference, "–", "-"))

#plot 
ggplot(data = ncaaw_cum) +
  geom_line(aes(x = year, y = cum_wins, colour = conference), size = 2) + 
  scale_y_continuous(breaks = seq(0, max(ncaaw_cum$cum_wins), len = 2)) + 
  theme_minimal()+  
  labs(title = "Winningest conferences in NCAA Women's Basketball Tournament", 
       subtitle = "From 1982 to 2018",
       x = "Tournament year",
       y = "Cumulative wins",
       caption = "Viz: @xavbarbier - Data: FiveThirtyEight",
       color = "Conferences")+
   theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10, face = "bold"),
        plot.margin = margin(2, 2, 2, 2, "cm"))
  
