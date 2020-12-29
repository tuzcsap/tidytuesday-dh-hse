library(tidyverse)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

big_mac %>% 
  filter(name == "United States") %>% 
  distinct(date, local_price) %>% 
  rename(US_PRICE = local_price) %>% 
  right_join(big_mac) %>% 
  mutate(dollar_ratio = dollar_price / US_PRICE - 1) %>% 
  distinct(date, name, dollar_ratio) ->
  with_values

read_csv("shiny_big_mac_index/countries.csv", col_names = FALSE) %>% 
  mutate(name = "Euro area") %>% 
  full_join(with_values) %>% 
  mutate(name = ifelse(is.na(X1), name, X1)) %>% 
  select(-X1) ->
  with_values
  
map_data("world") %>% 
  filter(region != "Antarctica") ->
  world

world %>%
  fuzzyjoin::regex_full_join(with_values %>% filter(date == "2000-04-01"),
                             by = c("region" = "name")) %>% 
  ggplot(aes(long, lat, map_id = region, fill = dollar_ratio)) +
  geom_map(map = world) +
  coord_quickmap() +
  theme_void() +
  scale_fill_gradient(low = "lightblue", high = "tomato") +
  labs(fill = "")
