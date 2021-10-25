library(dplyr)
setwd("~/Desktop/RScripts")
Loki <- read.csv("patents_7.csv")
Loki %>% count(ee_country, sort = TRUE)

df <- Loki %>%
  group_by(ee_country, ptype) %>%
  summarise(count = n()) 

Loki %>% 
  filter(ee_country == "US" | 
           ee_country == "JP" |
           ee_country == "KR")  %>% 
  ggplot(aes(ee_country, fill=ptype)) +
  geom_bar(position="fill",
           alpha = 0.5)

Loki %>% 
  filter(ee_country == "US" | 
           ee_country == "JP" |
           ee_country == "KR") %>% 
  ggplot(aes(ee_country, fill=ptype)) +
  geom_bar(position="dodge",
           alpha = 0.5)

Loki %>% 
  filter(ee_country == "US" | 
           ee_country == "JP" |
           ee_country == "KR",
           ptype == "utility")  %>% 
  ggplot(aes(ee_country, fill=ptype)) +
  geom_bar(position="dodge",
           alpha = 0.5)

Loki %>% 
  filter(ee_country == "US" | 
           ee_country == "JP" |
           ee_country == "KR",
         ptype == "design")  %>% 
  ggplot(aes(ee_country, fill=ptype)) +
  geom_bar(position="dodge",
           alpha = 0.5)

Loki %>% 
  filter(ee_country == "US" | 
           ee_country == "JP" |
           ee_country == "KR",
         ptype == "plant" |
           ptype == "reissue")  %>% 
  ggplot(aes(ee_country, fill=ptype)) +
  geom_bar(position="dodge",
           alpha = 0.5)
