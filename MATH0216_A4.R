## ----include=FALSE---------------------------------------------------------------------------------------------------
library(tidyverse)
library(maps)
library(Hmisc)


## --------------------------------------------------------------------------------------------------------------------
## maps
library(maps)
world_map <- map_data("world")  #to map countries in the world
states_map <- map_data("state") #to map states in the US
counties_map <-map_data("county") #to map counties in the US

## rnaturalearth
library(rnaturalearth)
worldmap <- ne_countries(scale = 'medium', type = 'map_units',returnclass = 'sf')


## --------------------------------------------------------------------------------------------------------------------
vermont <- map_data("county", region = "vermont")
vermont$subregion <- capitalize(vermont$subregion)

ggplot(vermont, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill = subregion), col="grey") +
  aes(color = subregion) +
  geom_point(aes(x = -73.1673, y = 44.0153), color = "black") +
  geom_text(aes(x = -73.1673, y = 44.0153, label = "Middlebury College"), size = 2.5, nudge_y = 0.1, color = "black") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Counties of Vermont", subtitle = "Middlebury College marked", caption = "Source: map_data") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position="right") +
  scale_fill_discrete(name = "County")




## --------------------------------------------------------------------------------------------------------------------
africa <- filter(worldmap, continent == "Africa")
africa$economy <- substr(africa$economy, start = 4, nchar(africa$economy))

ggplot() +
  geom_sf(data = africa, aes(fill = economy)) +
  labs(title = "Economy Types of African Countries", caption = "Source: rnaturalearth") +
  scale_fill_discrete(name = "Economy Type") +
  xlab("Longitude") +
  ylab("Latitude")


## --------------------------------------------------------------------------------------------------------------------
#read in and format dataset
data <- read.csv("https://ebmwhite.github.io/MATH0216/data/worlddata.csv")
data$Country <- as.character(trimws(data$Country))
data$phones <- as.numeric(data$phones)

#format map data for joining
world_map$Country <- as.character(world_map$region)

#join map data with dataset
world_data <- left_join(world_map, data , by = "Country")
world_data$Region <- trimws(world_data$Region)


## --------------------------------------------------------------------------------------------------------------------
sub_saharan_data <- world_data %>% 
  filter(Region == "ASIA (EX. NEAR EAST)")
  

ggplot(sub_saharan_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = phones), color = "black") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colors = c("yellow", "orange", "red"), 
                       name = "Phones per 1000 People") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_dark() +
  labs(title = "Phone Ownership in Asia", caption = "source: worlddata.csv")
  


