setwd("~/Desktop/RScripts")
Loki <- read.csv("patents_7.csv")

install.packages("dplyr")
library(dplyr)

patents_by_country <- Loki %>%
  group_by(ee_country) %>%
  summarize(count = n())

patents_by_country <- patents_by_country[with(patents_by_country,order(-count)),]
patents_by_country[1:10,]
