## ----include=FALSE---------------------------------------------------------------------------------------------------
library(openintro)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales)
library(lubridate)

goog_data <- openintro::goog
goog_data$Date <- as_date(goog_data$Date)


## ----warning=FALSE---------------------------------------------------------------------------------------------------
#format goog_data
goog_data_long <- goog_data %>% 
  select(Date, Open, Close, High, Low) %>%
  gather(key = "Attribute", value = "Price", c(Open, Close, High, Low)) 
goog_data_long$Date <- as_date(goog_data$Date)


#plot goog_data
ggplot(goog_data_long, aes(x = Date, y = Price, color=Attribute)) +
  geom_line() +
  labs(title="Google Stock Price Over Time",
       subtitle="2008 - 2014",
       x="Date (Year)",
       y="Price (US Dollars)",
       caption="source: Yahoo! Finance") +
  theme_foundation()



## --------------------------------------------------------------------------------------------------------------------
goog_data %>% 
  select(Date, Volume) %>% 
  ggplot(aes(x = Date, y = Volume/10^5)) +
  geom_bar(stat = "identity") +
  labs(title="Google Stock Trading Volume Over Time",
       subtitle="2008 - 2014",
       x="Date (Year)",
       y="Shares Traded (hundred thousands)",
       caption="source: Yahoo! Finance") +
  theme_solarized()


## --------------------------------------------------------------------------------------------------------------------
goog_data %>% 
  select(Date, Volume, High, Low) %>% 
  mutate(vol_adj = ((High+Low)/2)*Volume/10^8) %>% 
  ggplot(aes(x = Date, y = vol_adj)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title="Google Stock Trading Volume Over Time",
       subtitle="2008 - 2014",
       x="Date (Year)",
       y="Approx. Total Capital (in $100 millions)",
       caption="source: Yahoo! Finance") +
  scale_y_continuous() +
  theme_calc()

