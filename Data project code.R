library(readr)
library(tidyr)
library(splitstackshape)
library(tidyverse)
library(lubridate)

Unemp_OECD <- read_csv("BED-2056/Unemployment2000-2018.csv")

#Removing quotation marks, does not remove it from colnames
Unemp_OECD <- as.data.frame(sapply(Unemp_OECD, function(x) gsub("\"", "", x)))

#Split up colnames
Unemp_OECD <- splitstackshape::cSplit(Unemp_OECD, names(Unemp_OECD))

#Changing colnames
names(Unemp_OECD)[1] <- paste("Location")
names(Unemp_OECD)[2] <- paste("Indicator")  
names(Unemp_OECD)[3] <- paste("Subject")
names(Unemp_OECD)[4] <- paste("Measure")  
names(Unemp_OECD)[5] <- paste("Frequency")  
names(Unemp_OECD)[6] <- paste("Time")
names(Unemp_OECD)[7] <- paste("Value")
names(Unemp_OECD)[8] <- paste("Flag_Codes")

#Removing uninteresting columns
Unemp_OECD$Flag_Codes = NULL
Unemp_OECD$Indicator = NULL
Unemp_OECD$Subject = NULL
Unemp_OECD$Measure = NULL
Unemp_OECD$Frequency = NULL

#Setting column Location as.character
Unemp_OECD$Location <- as.character(Unemp_OECD$Location)

#Seperating Year and Quartile
Unemp_OECD <- separate(Unemp_OECD, Time, into = c("Year", "Quartile"), sep= "-")

#Changing Qartile to month and day
Unemp_OECD$Quartile <- str_replace(Unemp_OECD$Quartile, "Q1", "01-01")
Unemp_OECD$Quartile <- str_replace(Unemp_OECD$Quartile, "Q2", "04-01")
Unemp_OECD$Quartile <- str_replace(Unemp_OECD$Quartile, "Q3", "07-01")
Unemp_OECD$Quartile <- str_replace(Unemp_OECD$Quartile, "Q4", "10-01")

#Uniting year with month and day
Unemp_OECD <- unite(Unemp_OECD, "Time", c("Year","Quartile"), sep = "-")

#Setting it as.Date
Unemp_OECD$Time <- as.Date(Unemp_OECD$Time)


#Filtering for every country of interest
#Average for 28 EU countries
Unemp_EU28 <- filter(Unemp_OECD, Location == "EU28")
#Only data since 2005

#We'll be filtering the previous years out in the countries of interest

#Irland
Unemp_IRL <- filter(Unemp_OECD, Location == "IRL")
Unemp_IRL <- filter(Unemp_IRL, Time >= as.Date("2005-01-01"))
#France
Unemp_FRA <- filter(Unemp_OECD, Location == "FRA")
Unemp_FRA <- filter(Unemp_FRA, Time >= as.Date("2005-01-01"))
#Germany
Unemp_DEU <- filter(Unemp_OECD, Location == "DEU")
Unemp_DEU <- filter(Unemp_DEU, Time >= as.Date("2005-01-01"))
#Italy
Unemp_ITA <- filter(Unemp_OECD, Location == "ITA")
Unemp_ITA <- filter(Unemp_ITA, Time >= as.Date("2005-01-01"))
#Great Britain
Unemp_GBR <- filter(Unemp_OECD, Location == "GBR")
Unemp_GBR <- filter(Unemp_GBR, Time >= as.Date("2005-01-01"))
#Spain
Unemp_ESP <- filter(Unemp_OECD, Location == "ESP")
Unemp_ESP <- filter(Unemp_ESP, Time >= as.Date("2005-01-01"))
#Russia
Unemp_RUS <- filter(Unemp_OECD, Location == "RUS")
Unemp_RUS <- filter(Unemp_RUS, Time >= as.Date("2005-01-01"))
#United states of America
Unemp_USA <- filter(Unemp_OECD, Location == "USA")
Unemp_USA <- filter(Unemp_USA, Time >= as.Date("2005-01-01"))
#Canada
Unemp_CAN <- filter(Unemp_OECD, Location == "CAN")
Unemp_CAN <- filter(Unemp_CAN, Time >= as.Date("2005-01-01"))
#Greece
Unemp_GRC <- filter(Unemp_OECD, Location == "GRC")
Unemp_GRC <- filter(Unemp_GRC, Time >= as.Date("2005-01-01"))
#Norway
Unemp_NOR <- filter(Unemp_OECD, Location == "NOR")
Unemp_NOR <- filter(Unemp_NOR, Time >= as.Date("2005-01-01"))




#Plot of every country
ggplot(data = Unemp_OECD, aes(x = Time, y = Value, color = Location))+
  geom_line()




#Unelpyment rates between 2005 and 2018 for every country of interest
ggplot()+
  geom_line(data = Unemp_CAN, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_EU28, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_NOR, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_IRL, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_FRA, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_DEU, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_ITA, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_GBR, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_ESP, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_RUS, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_USA, aes(x = Time, y = Value, color = Location), size = 2)+
  geom_line(data = Unemp_GRC, aes(x = Time, y = Value, color = Location), size = 2)+
  labs(title = "Unemployment rates between 2005 and 2018",
       x = "Year", y = "% Unemployed")+
  scale_color_manual(values=c("Black", "Blue", "Brown", "Red", "Yellow", "Pink",
                              "Orange", "Grey", "Purple","Navy", "Cyan",
                              "Mediumvioletred"))

