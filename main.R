#install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)
#install.packages(c("scales"))
library(scales)

datCC <- read.csv("/cloud/project/climate-change.csv")
datCO2 <- read.csv("/cloud/project/annual-co-emissions-by-region.csv")

colnames(datCO2)[4] <- "CO2"
colnames(datCO2)[1] <- "Country"

#IN-CLASS PROMPTS

#P1
#plot(datCO2$Year, datCO2$CO2, type = "b", xlab = "Year", ylab = "CO2 Emissions")
#ggplot(datCO2, aes(Year, CO2)) + geom_point()

colnames(datCC)[1] <- "Region"

datCC$ptime <- ymd(datCC$Day, tz="EST")

hemispheres <- datCC %>% 
  filter(Region == "Northern Hemisphere" | Region == "Southern Hemisphere")

north <- hemispheres %>%
  filter(Region == "Northern Hemisphere")

south <- hemispheres %>%
  filter(Region == "Southern Hemisphere")

plot(north$ptime, north$temperature_anomaly,
     type = "l", 
     xlab = "Date", ylab = "Temperature Anomaly (°C)")
points(south$ptime, south$temperature_anomaly,
       type = "l",
       col="red"
)

ggplot(hemispheres, aes(x = ptime, y = temperature_anomaly, color = Region)) + 
  geom_point() + labs(x="Date", y = "Temperature Anomaly (°C)")

#P2

NA_CO <- datCO2 %>% 
  filter(Country == "United States" | 
           Country == "Mexico" | 
           Country == "Canada")

totals <- NA_CO %>%
  group_by(Country) %>%
  summarize(Total_Emissions = sum(CO2))

ggplot(totals, aes(x = Country, y = Total_Emissions)) + geom_col() + 
  labs(x="Country", y = "Total CO2 Emissions (tons)")

#HOMEWORK PROBLEMS

#Q1

#extracting data for nordics as countries of my choosing
nordics <- datCO2 %>%
  filter(Country == "Sweden" | Country == "Finland" | Country == "Norway")

#cite: https://forum.posit.co/t/how-to-turn-off-scientific-notation-like-1e-09-in-r/71575
#used to figure out how to switch the axis off of scientific notation 
ggplot(nordics, aes(x = Year, y = CO2, color = Country)) + geom_line() + 
  labs(x="Year", y = "Total CO2 Emissions (tons)") + xlim(1850, 2020) + 
  scale_y_continuous(labels = comma) + ggtitle("Annual CO2 Emissions of Nordic Countries")

