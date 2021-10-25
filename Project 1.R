setwd("~/Desktop/RScripts")
Loki <- read.csv("patents_7.csv")

library(dplyr)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(gghighlight)

patentsCountry <- Loki %>%
  group_by(ee_country) %>%
  summarize(patent_count = n())


## Top 10 Countries with the Most Number of Patents
mostPatents <- patentsCountry[with(patentsCountry,order(-patent_count)),]
top10patents <- mostPatents[1:10,]
top10patents
# Only 5 countries had more than 1,000 patents: the US, Japan, South Korea, Germany, and Taiwan.
# The US is the only country with more than 10,000 patents.


## Number of Patents Approved Over Time
approvals <- Loki %>%
  filter(ee_country %in% c("US", "JP", "KR", "DE", "TW")) %>%
  add_count(ee_country, grantyear, sort=FALSE, name = "count")
approvals <- select(approvals, ee_country, grantyear, count) 
approvals <- distinct(approvals)
approvals$grantyear <- as.integer(approvals$grantyear)

linegraph <- ggplot(approvals, aes(x=grantyear, y=count)) +
  geom_line(aes(color=ee_country)) +
  scale_color_brewer(palette="Set2") + 
  theme(panel.background = element_blank()) +
  labs(title="Patents Approved Over Time by Top 5 Countries",
       x = "Year",
       y = "Patents") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  gghighlight(ee_country %in% c("US", "JP", "KR", "DE", "TW"))
linegraph
# The US has shown a large increase in the number of patents approved each year from 2011 to 2019.
# Other countries have mostly stayed stagnant with little growth. 


## Popular Patent Types by Country
countryPtype <- Loki %>% 
  filter(ee_country %in% c("US", "JP", "KR", "DE", "TW")) %>%
  add_count(ee_country, ptype, sort=FALSE, name = "count")
countryPtype <- select(countryPtype, ee_country, ptype, count)
countryPtype <- distinct(countryPtype)

countryPtype$ptype <- factor(countryPtype$ptype,
                             levels=c("utility","design","plant","reissue"))

countryPtype$ee_country <- factor(countryPtype$ee_country,
                                  levels=c("TW","DE","KR","JP","US"))

ptypeBar <- ggplot(countryPtype, aes(fill=ee_country, y=count, x=ptype)) + 
  geom_bar(position="stack", stat="identity") +
  theme(panel.background = element_blank()) +
  labs(title="Popularity of Patent Types by Country",
       x ="Patent Type", y = "Number of Patents") +
  scale_fill_brewer(palette="Set2") 
ptypeBar
# Utility is by far the most common type of patent amongst the top countries.
# The US holds large portions of the total patents for each type. 


# Average Wait Times for Utility Patent Approval 
Loki$time_to_approval <- Loki$grantyear - Loki$applyear
patentsWait <- select(Loki, ee_country, ptype, time_to_approval)
patentsWait <- patentsWait %>%
  filter(ptype == "utility") %>%
  group_by(ee_country) %>% 
  summarize(mean_wait_time = mean(time_to_approval))
patentsWait$mean_wait_time <- round(patentsWait$mean_wait_time ,digit=2)
top5wait <- patentsWait %>%
  filter(ee_country %in% c("US", "JP", "KR", "DE", "TW"))
top5wait

waitBar<-ggplot(data=top5wait, aes(x=reorder(ee_country, -mean_wait_time), y=mean_wait_time, fill=ee_country)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set2") +
  theme(panel.background = element_blank(),
        legend.position = "none") +
  labs(title="Average Wait Time for Utility Patents Approval",
       x = "Country",
       y = "Years") +
  geom_text(aes(label=mean_wait_time), position=position_dodge(width=0.9), vjust=-0.25)
waitBar
# While the US has far more patents than other countries, their wait time for approval is not short in comparison to the other Top 5 countries. 


## Top 5 States with the Most Patents
patents_us <- subset(Loki, ee_country == "US")
patents_states <- patents_us %>%
  add_count(ee_state, sort=FALSE, name = "count")
patents_states <- select(patents_states, ee_state, count)
patents_states <- distinct(patents_states)
TOPpatents_states <- patents_states[with(patents_states,order(-count)),]
topStates <- TOPpatents_states[1:5,]
topStates
# California has the most patents by far. 


## Entities that Own US Patents
usEntities <- patents_us %>% 
  add_count(ee_role_desc, sort=FALSE, name = "count")
usEntities <- select(usEntities, ee_role_desc, count)
usEntities <- distinct(usEntities)
usEntities <- usEntities[with(usEntities,order(-count)),]
usEntities
# Vast majority of US patents are owned by US companies / corporations. 


## US Companies with the Most Patents 
usInc <- patents_us %>%
  add_count(ee_name, sort=FALSE, name = "count")
usInc <- select(usInc, ee_name, count)
usInc <- distinct(usInc)
topUSinc <- usInc[with(usInc,order(-count)),]
topUSinc <- topUSinc[1:5,]
topUSinc
# International Business Machines Corporation (IBM) own far more patents than other US companies. 

