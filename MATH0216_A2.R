library(tidyverse)

#To load directly from webpage
#UNCdata <- read.csv("http://ryanthornburg.com/wp-content/uploads/2015/05/UNC_Salares_NandO_2015-05-06.csv")

#OR
#to load locally (from file saved in same folder)
UNCdata <- read.csv("UNCdata.csv")

UNCdata %>% 
  select(name, dept, age, totalsal) %>% 
  head()

names(UNCdata)[9] <- "fulltime"

UNCdata %>% 
  filter(dept == "Neurosurgery") %>% 
  summarise(mean.salary = mean(totalsal))

UNCdata %>% 
  filter(dept == "Neurosurgery" & totalsal > 500000)

UNCdata %>% 
  filter(dept == "Dermatology", fulltime == 1) %>% 
  summarize(total.pay = sum(totalsal))

UNCdata %>% 
  group_by(dept) %>% 
  summarize(cnt = n()) %>% 
  filter(cnt >= 10) %>% 
  summarize(ten.plus = n())
  

radio.dept <- UNCdata %>% 
  filter(dept == "Radiology") %>% 
  select(name, position, age, nonstsal, totalsal) %>% 
  arrange(-totalsal)

dept_summary <- UNCdata %>% 
  group_by(dept) %>% 
  summarize(dep.size = n(),
            mean.dep.sal = mean(totalsal),
            median.dep.sal = median(totalsal),
            max.sal = max(totalsal))

dept_summary %>%
  select(dept, mean.dep.sal) %>% 
  arrange(-mean.dep.sal) %>% 
  head(n=10)
  

dept_summary %>% 
  select(dept, median.dep.sal ) %>% 
  arrange(-median.dep.sal) %>% 
  head(n=10)

names <- levels(UNCdata$dept) #already sorted?
names[42]
