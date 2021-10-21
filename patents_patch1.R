getwd()
patents= read.csv("patents_7.csv", header = TRUE)

head(patents)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
#patents by country
patents_by_country <- patents %>%
  group_by(ee_country) %>%
  summarize(count = n())

head(patents_by_country)

#us subset
patents_us <- subset(patents, ee_country == "US")

nrow(patents_us)
#How long does a patent take to get granted in the US by state on average?
patents_us$time_to_approval <- patents_us$grantyear - 
  patents_us$applyear

aggregate(time_to_approval ~ ee_state,
          data = patents_us, 
          FUN = mean)

install.packages("patentsview")
library(patentsview)                 
