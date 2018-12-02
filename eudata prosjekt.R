library(readr)

library(dplyr)

library(tidyverse)

library(knitr)

#SKIFTER NAVN P?? DATA
eudata <- DP_LIVE_06112018221043065

#SETTER "TIME" AS.NUMERIC
eudata$TIME <- as.numeric(eudata$TIME)

# FINNER GJENNOMSNITT PR ??R FOR EU
meaneu <- eudata %>% 
  group_by(TIME) %>% 
  summarise(average = mean(Value))

#FJERNER URELEVANT IFORMASJON
eudata$INDICATOR = NULL
eudata$MEASURE = NULL
eudata$FREQUENCY = NULL
eudata$SUBJECT = NULL
eudata$Flag.Codes = NULL

#SJEKKER REVIDERT DATA
print(eudata)

#ENDRER NAVN P?? KOLONNE 1 TIL "COUNTRY"
names(eudata)[1] <- paste("Country")

#SEPARER DATA ETTER LAND
france <- filter(eudata, Country == "FRA")
ggplot() +
  geom_line(data=france, aes(x = TIME, y = Value))

germany <- filter(eudata, Country == "DEU")
ggplot() +
  geom_line(data=germany, aes(x = TIME, y = Value))

grecce <- filter(eudata, Country == "GRC")
ggplot() +
  geom_line(data=grecce, aes(x = TIME, y = Value))

ireland <- filter(eudata, Country == "IRL")
ggplot() +
  geom_line(data=ireland, aes(x = TIME, y = Value))

italia <- filter(eudata, Country == "ITA")
ggplot() +
  geom_line(data=italia, aes(x = TIME, y = Value))

spain <- filter(eudata, Country == "ESP")
ggplot() +
  geom_line(data=spain, aes(x = TIME, y = Value))

uk <- filter(eudata, Country == "GBR")
ggplot() +
  geom_line(data=uk, aes(x = TIME, y = Value))

#LEGGER ALLE LANDENE SAMME I EN GRAF
ggplot() + 
  geom_line(data= france, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  geom_line(data= germany, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  geom_line(data= grecce, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  geom_line(data= ireland, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  geom_line(data= italia, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  geom_line(data= spain, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  geom_line(data= uk, aes(x = TIME, y = Value, color = Country, size = 0.5)) +
  labs(title = "Government debt (EU) 2008 - 2017", x = "Date/Time", y = "% Debt")

  
              
              
              
              
  





