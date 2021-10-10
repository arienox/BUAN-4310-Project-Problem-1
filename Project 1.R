setwd("~/Desktop/RScripts")
Loki <- read.csv("patents_7.csv")

install.packages("dplyr")
library(dplyr)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)


patents_by_country <- Loki %>%
  group_by(ee_country) %>%
  summarize(count = n())

# Top 10 Regions with the Most Number of Patents
TOPpatents_by_country <- patents_by_country[with(patents_by_country,order(-count)),]
TOPpatents_by_country[1:10,]

# Bottom 20 Regions with the Least Number of Patents (only 1 patent)
BOTTOMpatents_by_country <- patents_by_country[with(patents_by_country,order(count)),]
BOTTOMpatents_by_country[1:20,]

# Subset US 
patents_us <- subset(Loki, ee_country == "US")


# Most Popular Patents Overall
patents_by_type <- Loki %>%
  group_by(ptype) %>%
  summarize(count = n())
patents_by_type <- patents_by_type[with(patents_by_type,order(-count)),]


# Popular patent type by top 5 countries
country_type <- Loki %>% 
  filter(ee_country %in% c("US", "JP", "KR", "DE", "TW")) %>%
  add_count(ee_country, ptype, sort=FALSE, name = "count")
country_type <- select(country_type, ee_country, ptype, count)
country_type <- distinct(country_type)


# Line Graph of Patents Granted over the Years (2011 - 2019)
granted <- Loki %>%
  filter(ee_country %in% c("US", "JP", "KR", "DE", "TW")) %>%
  add_count(ee_country, grantyear, sort=FALSE, name = "count")

granted <- select(granted, ee_country, grantyear, count) 
granted <- distinct(granted)

linegraph <- ggplot(granted, aes(x=grantyear, y=count)) +
  geom_line(aes(color=ee_country)) +
  scale_color_brewer(palette="Set2")
linegraph

# More things to do
# Average wait time to get a patent granted
# Who owns the most patents in the US
# Waffle plot - popular types of patents %
