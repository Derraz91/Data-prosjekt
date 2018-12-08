#install.packages("pacman")
library(pacman)
p_load(readr, tidyr, splitstackshape, tidyverse, lubridate, plyr, dplyr,
       scales, ggvis, stringr, zoo)


Unemp_OECD <- read_csv("https://raw.githubusercontent.com/Derraz91/Data-prosjekt/master/Unemployment2000-2018.csv")

#Removing quotation marks, does not remove it from colnames
Unemp_OECD <- as.data.frame(sapply(Unemp_OECD, function(x) gsub("\"", "", x)))

#Split up colnames
Unemp_OECD <- splitstackshape::cSplit(Unemp_OECD, names(Unemp_OECD))


#Removing uninteresting columns
Unemp_OECD <- Unemp_OECD[, -c(2:5, 8)]

#Changing colnames
colnames(Unemp_OECD) <- c("Location", "Time", "Value")

#Setting column Location as.character
Unemp_OECD$Location <- as.character(Unemp_OECD$Location)

#Changing Qartile to month and day
Unemp_OECD$Time <- as.Date(as.yearqtr(Unemp_OECD$Time, format = "%Y-Q%q"))


#Filtering for every country of interest
#Only data since 2005 for EU28, so doing the same for other countries
Countries <- c("IRL", "FRA", "DEU", "ITA", "GBR", "ESP", "RUS", "USA", "CAN", "GRC",
               "NOR", "EU28")

Unemp_filtered <- Unemp_OECD %>%
  filter(Location %in% Countries & Time >= as.Date("2005-01-01"))

#Displaying all countries of interest in a ggplot
ggplot(data = Unemp_filtered, aes(Time, Value, color = Location))+
  geom_line(size = 2)+
  theme_classic()+
  scale_y_continuous(breaks=seq(0,30,5))+
  scale_x_date(date_breaks = "2 year", labels=date_format("%Y"))+
  labs(title = "Unemployment rates between 2005 and 2018",
       x = "Year", y = "% Unemployed")+
  scale_color_manual(values=c("Black", "Blue", "Brown", "Red", "Yellow", "Pink",
                              "Orange", "Grey", "Purple","Navy", "Cyan",
                              "Mediumvioletred"))



#Displaying in a checkbox graph

Unemp_filtered %>%
  group_by(Location) %>%
  ggvis(~Time, ~Value) %>%
  filter(Location %in% eval(input_checkboxgroup(
    choices = unique(Unemp_filtered$Location),
    label = "Location"))) %>%
  layer_paths(stroke = ~Location)

