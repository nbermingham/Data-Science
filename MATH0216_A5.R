## ----include=FALSE---------------------------------------------------------------------------------------------------
library(tidyverse)
library(rvest)


## --------------------------------------------------------------------------------------------------------------------
products <-read.csv("https://ebmwhite.github.io/MATH0216/assignments/products.csv")
orders <-read.csv("https://ebmwhite.github.io/MATH0216/assignments/orders.csv")
customers <-read.csv("https://ebmwhite.github.io/MATH0216/assignments/customers.csv")


## --------------------------------------------------------------------------------------------------------------------
products %>% 
  filter(price <10)


## --------------------------------------------------------------------------------------------------------------------
orders %>% 
  filter(product_id == 3) %>% 
  summarise(sum = sum(quantity))


## --------------------------------------------------------------------------------------------------------------------
product <- orders$product_id[orders$order_id == 4]

products$description[product]


## --------------------------------------------------------------------------------------------------------------------
customer <- orders$customer_id[orders$order_id == 7]

customers$email[customer]


## --------------------------------------------------------------------------------------------------------------------
orders <- orders%>%
  mutate(price = products$price[product_id])

orders


## --------------------------------------------------------------------------------------------------------------------
orders <- orders %>% 
  mutate(total = price * quantity)

orders


## --------------------------------------------------------------------------------------------------------------------
orders %>% 
  summarise(total_revenue = sum(total))


## --------------------------------------------------------------------------------------------------------------------
people <- orders$customer_id[orders$time_stamp == "3/13/20"]
customers[people, 2:5]


## --------------------------------------------------------------------------------------------------------------------
world_cup_URL <- "https://en.wikipedia.org/wiki/FIFA_World_Cup"

world_cup_URL %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_table(fill = TRUE) %>% 
  head()

