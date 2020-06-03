## ----message=FALSE, warning=FALSE, include=FALSE----------------------------------------------------
library(tidyverse)
library(gganimate)
library(ggplot2)
library(plyr)
library(gifski)
c_data <- read_csv('time_series_covid19_deaths_global.csv')[,-c(1,3,4)] 
colnames(c_data)[1] <- "country"
c_data <- ddply(c_data, "country", numcolwise(sum))
head(c_data)


## ----include=FALSE, warning=FALSE-------------------------------------------------------------------
eu_countries <- c("Austria",
                  "Belgium",
                  "Bulgaria",
                  "Croatia",
                  "Cyprus",
                  "Czech Republic",
                  "Denmark",
                  "Estonia",
                  "Finland",
                  "France",
                  "Germany",
                  "Greece",
                  "Hungary",
                  "Ireland",
                  "Italy",
                  "Latvia",
                  "Lithuania",
                  "Luxembourg",
                  "Malta",
                  "Netherlands",
                  "Poland",
                  "Portugal",
                  "Romania", 
                  "Slovakia",
                  "Slovenia",
                  "Spain",
                  "Sweden")
c_data_2 <- c_data %>% 
  filter(country %in% eu_countries) %>% 
  gather(key = Date, value = Cases, 2:101) %>% 
  arrange(country)

c_data_2$Date <- as.Date(c_data_2$Date, "%m/%d/%Y")
  


## ----include=FALSE, warning=FALSE-------------------------------------------------------------------
line_animation <- c_data_2 %>% 
  ggplot(aes(x = Date, y = Cases, group = country, color = factor(country))) +
  geom_line()+
  scale_color_discrete()+
  labs(x = "Time", y = "Deaths", title = "Number of COVID-19 Deaths", color = "Country") +
  transition_reveal(Date)
  

## ----echo=FALSE, warning=FALSE----------------------------------------------------------------------
animate(line_animation, duration = 10, fps = 20, renderer = gifski_renderer())


## ----include=FALSE, warning=FALSE-------------------------------------------------------------------
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = 'medium', type = 'map_units',returnclass = 'sf')

europe <- world %>% 
  filter(name %in% eu_countries) %>% 
  arrange(name)


## ----include=FALSE, warning=FALSE-------------------------------------------------------------------

today <- c_data[c(1, 40)] 

europe <- map_data("world", region = eu_countries)

c_data_long <- c_data %>% 
  gather(value=deaths, key=dates,2:101)

full <- left_join(europe, c_data_long, by= c("region" = "country")) 
full$dates <- as.Date(full$dates, format = "%m/%d/%y")




## ----include=FALSE, warning=FALSE-------------------------------------------------------------------
map_animation <- ggplot(full, aes(x=long, y=lat, group=group, fill=deaths)) +
  geom_polygon( col="black") + 
  coord_fixed(ratio = 1) +
  labs(title = "COVID-19 Deaths Over Time", subtitle = 'Date: {current_frame}') +
  scale_fill_gradientn(colors = c("black", "red")) +
  transition_manual(dates)



## ---- echo=FALSE, warning=FALSE---------------------------------------------------------------------
animate(map_animation, duration = 10, fps = 20, renderer = gifski_renderer())

