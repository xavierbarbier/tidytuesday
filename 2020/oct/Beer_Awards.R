# load libraries
library(tidyverse)
library(conflicted)
library(usmap)
library(patchwork)
library(cowplot)
library(here)
library(magick)


#how to handle overloaded functions
conflict_prefer("pluck","purrr")
conflict_prefer("filter","dplyr")

# load data
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

# Quick look at data
glimpse(beer_awards)

# Converting to uppercase
beer_awards$state<-str_to_upper(beer_awards$state)

# Counting for gold medal per states
beer_awards_gold<- beer_awards%>%
  filter(medal == "Gold")%>%
  mutate(nb = 1)%>%
  select(nb,state)%>%
  group_by(state)%>%
  summarise_all(funs(sum))

# us map gold medals plot
goldplot<-plot_usmap(data = beer_awards_gold, values = "nb") + 
  scale_fill_gradient(low = "white", high = "gold")+
  theme(legend.position = "none")+
  ggtitle("Gold medals")+ 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Counting for silver medal per states
beer_awards_silver<- beer_awards%>%
  filter(medal == "Silver")%>%
  mutate(nb = 1)%>%
  select(nb,state)%>%
  group_by(state)%>%
  summarise_all(funs(sum))

# us map silver medals plot
silverplot<-plot_usmap(data = beer_awards_silver, values = "nb") + 
  scale_fill_gradient(low = "white", high = "slategrey")+
  theme(legend.position = "none")+
  ggtitle("Silver medals")+ 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Counting for bronze medal per states
beer_awards_bronze<- beer_awards%>%
  filter(medal == "Bronze")%>%
  mutate(nb = 1)%>%
  select(nb,state)%>%
  group_by(state)%>%
  summarise_all(funs(sum))

# us map bronze medals plot
bronzeplot<-plot_usmap(data = beer_awards_bronze, values = "nb") + 
  scale_fill_gradient(low = "white", high = "chocolate4")+
  theme(legend.position = "none")+
  ggtitle("Bronze medals")+ 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Put the plots together
plot <-goldplot + silverplot + bronzeplot + plot_annotation(
  title = "Great American Beer Festival", 
  subtitle =  "Most medals by states (1987-2020)", 
  caption = "Created by @xavbarbier, Data from Great American Beer Festival, #TidyTuesday",
  theme = theme(plot.title = element_text(size = 32),
                plot.caption = element_text(size=12))
)
## Add Logo
logo_file <- here("gabf.jpeg")
ggdraw() +
  draw_plot(plot) +
  draw_image(logo_file,  x = 0.4, y = 0.3, scale = .2)
