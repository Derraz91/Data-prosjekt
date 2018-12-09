library(readr)

library(dplyr)

library(tidyverse)

install.packages("zoo")

library(zoo)

short_term_rate <- DP_LIVE_08122018162404242

View(short_term_rate)

#FJERNER URELEVANT DATA
short_term_rate$INDICATOR = NULL
short_term_rate$SUBJECT = NULL
short_term_rate$FREQUENCY = NULL
short_term_rate$Flag.Codes = NULL
short_term_rate$MEASURE = NULL

rente_usa <- short_term_rate[107:250, ]
View(rente_usa)

rente_eu <- short_term_rate[5519:5662, ]
View(rente_eu)

#FINNER GJENNOMSNITT PR M??NED FOR USA
meanUS <- rente_usa %>% 
  group_by(TIME) %>% 
  summarise(average = mean(Value))

meanUS$TIME <- as.Date(as.yearmon(meanUS$TIME))


View(meanUS)

#FINNER GJENNOMSNITT PR M??NED FOR EU

meanEU <- rente_eu %>% 
  group_by(TIME) %>% 
  summarise(average = mean(Value))

meanEU$TIME <- as.Date(as.yearmon(meanEU$TIME))

View(meanEU)


#LAGER GRAF AV RENTE FOR EU OG USA

ggplot() +
  geom_line(data = meanUS, aes(x = TIME, y = average, color = "US")) +
  geom_line(data = meanEU, aes(x = TIME, y = average, color = "EU",)) +
  labs(title = "Short-term intrest-rate US and EU", x = "Date/Time", y = "Rate")

ggplot() +
  geom_line(data = meanEU, aes(x = TIME, y = average))

ggplot() +
  geom_line(data = meanUS, aes(x = TIME, y = average))









