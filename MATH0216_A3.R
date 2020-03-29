## ----include=FALSE---------------------------------------------------------------------------------------
library(tidyverse)
library(ggthemes)


## --------------------------------------------------------------------------------------------------------
#To load directly from `nycflights13` package
# make sure you have ran install.packages("nycflights13")
library(nycflights13)
#tblflights <- tbl_df(flights)

#OR
# to load from local .csv file
tblflights <- read.csv("tblflights.csv")


## --------------------------------------------------------------------------------------------------------
#the airline codes can be found using the following code
tbl_df(airlines)


## ---- results=FALSE--------------------------------------------------------------------------------------
mutate(.data = tblflights, route = paste(origin, dest, sep = "-"))


## --------------------------------------------------------------------------------------------------------
length(table(tblflights$carrier)) 


## --------------------------------------------------------------------------------------------------------
tblflights %>%
  filter(is.na(dep_delay) == 0) %>%
  group_by(carrier) %>% 
  summarize(mean.delay = mean(dep_delay))


## --------------------------------------------------------------------------------------------------------
tblflights %>% 
  arrange(-dep_delay) %>%
  select(year, month, day, sched_dep_time, dep_time, carrier, origin, dest, dep_delay) %>% 
  head(n = 10)


## --------------------------------------------------------------------------------------------------------
tblflights %>% 
  filter(is.na(dep_delay) == 0 & dep_delay >= 0) %>% 
  ggplot(aes(dep_delay)) + 
  theme_wsj() +
  geom_histogram(binwidth = 0.25, na.rm = TRUE) + 
  labs(title = "Departure Delays", 
         x = "Departure Delay (in minutes)", 
         y = "Frequency", 
         caption = "source: tblflights dataset") +
  scale_x_log10() 


tblflights %>% 
  filter(is.na(dep_delay) == 0 & dep_delay >= 0) %>%  
  ggplot(aes(arr_delay)) +
  geom_histogram(binwidth = 0.25, na.rm = TRUE) +
  theme_economist() +
  labs(title = "Arrival Delays",
         x = "Arrival Delay (in minutes)", 
         y = "Frequency", 
         caption = "source: tblflights dataset") +
  scale_x_log10()


## --------------------------------------------------------------------------------------------------------
tblflights %>% 
  filter(origin == "JFK", dest == "LAX", arr_delay > 0, arr_delay <= 400) %>% 
  ggplot(aes(x = carrier, y = arr_delay)) +
  geom_boxplot(fill = "steelblue", outlier.size = 1) +
  labs(title = "Arrival Delays Across Carriers",
         x = "Carrier", 
         y = "Delay (Minutes)", 
         caption = "source: tblflights dataset")
  


## --------------------------------------------------------------------------------------------------------
carrier_flights_over_time <- tblflights %>% 
  group_by(carrier, month) %>% 
  summarize(sum = length(carrier)) %>% 
  spread(key = month, value = sum)

carrier_flights_over_time


## --------------------------------------------------------------------------------------------------------
tblflights %>% 
  filter((origin == "JFK" | origin == "LGA" | origin == "EWR") & (dest == "SFO" | dest == "SJC" | dest == "OAK")) %>% 
  group_by(carrier, month) %>% 
  summarize(sum = length(carrier)) %>% 
  ggplot(aes(x = month, y = sum)) +
  geom_point(aes(col=carrier), size=2) +
  geom_line(aes(col=carrier)) +
  labs(title = "Flights from NYC Area to SF Bay Area by Carrier",
          x = "Month", 
          y = "Number of Flights", 
          caption = "source: tblflights dataset") +
  scale_x_discrete(limits=c("JAN","FEB","MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")) +
  theme_dark()
  

