library(tidyverse)
theme_set(theme_minimal())

ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

ninja_warrior %>% 
  count(obstacle_name, sort = TRUE)

ninja_warrior %>% 
  count(obstacle_name) %>% 
  top_n(50) %>% 
  ggplot(aes(n, fct_reorder(obstacle_name, n))) +
  geom_col() +
  labs(x="",
       y="")

library(UpSetR)
#upset graph

ninja_warrior %>% 
  distinct(season, location, round_stage) %>% 
  mutate(id = 1:n()) %>% 
  left_join(ninja_warrior) %>% 
  relocate(id) %>% 
  select(id, obstacle_name) %>% 
  mutate(value = 1) %>% 
  pivot_wider(values_from = value, names_from = obstacle_name, values_fill = 0) %>% 
  select(-id) %>% 
  as.data.frame() %>% 
  upset(nsets = 20, order.by = "freq")


library(tidytext)

s_not_ending <-  c("Cross", "Hourglass", "Criss", "Pass")
ninja_warrior %>% 
  separate(col = obstacle_name, 
           sep = " ",
           into = c("name1", "name2", "name3", "name4", "name5"),
           fill = "left") %>% 
  select(-name1, -name2, -name3, -name4) %>% 
  rename(word = name5) %>% 
  mutate(word = str_remove(word, "s$"),
         word = str_remove(word, "'$"),
         word = ifelse(word %in% str_sub(s_not_ending, start = 1, end = -2),
                       str_c(word, "s"),
                       word)) %>% 
  #count(word, sort = TRUE) %>% top_n(16) %>% pull(word) -> fltrd_obstacles
  count(word, season, sort = TRUE) %>% 
  filter(word %in% fltrd_obstacles) %>% 
  #reorder_within for sorting in facets
  mutate(word2 = reorder_within(word, n, within = season)) %>% 
  ggplot(aes(n, word2, fill = word))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~season, scales= "free")+
  scale_y_reordered() +
  labs(title = "Starting from the 6. season Wall, Step, and Ladders are the most popular",
       x = "", y = "",
       caption = "data is from tidytuesday 2020-12-15")
