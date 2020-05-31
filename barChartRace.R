# barChartRace.R
# About - This script uses data from covid19india.org to plot number of COVID-19 case for States. And then it animates these graphs across dates to create a bar chart race.
# Author - @repulsivestud
# Date - 31/05/2020

library(tidyverse)
library(gganimate)
library(ggthemes)
library(RColorBrewer)

rm(list=ls())

rd1 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data1.csv"))
rd2 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data2.csv"))
rd3 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data3.csv"))
rd4 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data4.csv"))
rd5 <- data.frame(read_csv("https://api.covid19india.org/csv/latest/raw_data5.csv"))

rd1 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases = Num.cases) -> covidcases

covidcases %>% 
  bind_rows(
    rd2 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.cases)
  ) %>% 
  bind_rows(
    rd3 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.Cases)
  )%>% 
  bind_rows(
    rd4 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.Cases) 
  ) %>% 
  bind_rows(
    rd5 %>% select(Date.Announced, Detected.City, Detected.District, Detected.State, State.code, Current.Status, Status.Change.Date, Cases =Num.Cases)
  ) -> covidcases

covidcases %>% 
  rename(
    Date = Date.Announced,
    City = Detected.City,
    District = Detected.District,
    State = Detected.State,
    UpdatedOn = Status.Change.Date,
  ) -> covidcases

covidcases %>% 
  mutate(
    Date = as.Date(Date, '%d/%m/%Y'),
    City = as.factor(City),
    District = as.factor(District),
    State = as.factor(State),
    State.code = as.factor(State.code),
    Current.Status = as.factor(Current.Status),
    UpdatedOn = as.Date(UpdatedOn, '%d/%m/%Y')
  ) -> covidcases

covidcases %>%
  filter(Current.Status == "Hospitalized") %>%
  filter(!is.na(Cases)) %>% 
  group_by(Date, State) %>%
  summarise(Cases=sum(`Cases`)) %>% 
  filter(State != "State Unassigned") -> covidcases

covidcases %>% 
  arrange(Date, State) %>% 
  group_by(State) %>%
  mutate(
    Cumulative= cumsum(Cases)
  ) %>% 
  ungroup() -> dailyDetectedCases
  
dailyDetectedCases %>% 
  filter(Date < Sys.Date()) %>% 
  group_by(Date) %>% 
    mutate(
      rank = rank(-Cumulative, ties.method= "last"),
    ) %>% 
  group_by(State) %>% 
  filter(rank <=10)-> dailyDetectedCases

dailyDetectedCases %>% 
  ggplot(aes(rank, group = State, fill = factor(State))) +
  geom_tile(aes(y = Cumulative/2,
                height = Cumulative,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(State, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Cumulative,label = Cumulative, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, face="bold", colour="Black", vjust=-1),
        plot.subtitle=element_text(size=20, color="Black"),
        plot.caption =element_text(size=16, face="italic", color="Black"),
        plot.background=element_blank(),
        plot.margin = margin(1,2, 1, 4, "cm")) +
  scale_fill_viridis_d() -> staticPlot

animationPlot = staticPlot + transition_time(Date) + ease_aes('cubic-in-out') +
  view_follow(fixed_x = TRUE) +
  labs(title="COVID Cases for top 10 States",
       subtitle='\n\nAs on Date - {round(frame_time,0)}',
       caption="© Omkar Shukla (@shukla_omkar)\n Data source: covid19india.org")

animate(animationPlot, fps = 20, width = 1000, height = 800, renderer = gifski_renderer("gganim.gif"))