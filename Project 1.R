setwd("~/Desktop/RScripts")
Loki <- read.csv("patents_7.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tigris)
options(tigris_use_cache = TRUE)

count(patents, ee_country)
data %>% 
  arrange(desc(ee_country))

mydata = patents(x=1:100, y=runif(100)rownames(mydata)
                 
Loki <- patents %>%
  group_by(ee_country, ptype) %>%
  summarise(count = n()) 
                 
data1 <- patents %>%
  filter(ptype %in% c("utility")) %>%
  filter(ee_country %in% c("US", "JP", "CN", "CA", "TW", "KR")) %>%
  group_by(ee_country) %>%
  summarize(count = n())

data1 <- factor(data1,
                levels = data1[order(data1, decreasing = TRUE)])

#Run here for bar graph
ggplot(data = data1, aes(x = ee_country, y = count, color = ee_country)) + 
  geom_bar(stat = "identity", fill = "white") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(fill = "aliceblue")) +
  scale_color_discrete(name = "Country") +
  scale_x_discrete(limits = c("CA", "CN", "TW", "KR", "JP", "US")) + 
  labs(x = "") +
  labs(y = "Patent Count", caption = 
         "Illustrating data only within the utility type patents") + 
  ggtitle("Number of Utility type Patents per Country")  

p <- ggplot(data = data1, aes(x = ee_country, y = count)) + 
     geom_bar(stat = "identity")
p
