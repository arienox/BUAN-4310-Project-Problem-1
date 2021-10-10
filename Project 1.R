setwd("~/Desktop/RScripts")
Loki <- read.csv("patents_7.csv")
Loki %>% count(ee_country, sort = TRUE)


