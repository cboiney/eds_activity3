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

nordics$mtons <- nordics$CO2/1000000

#cite: https://forum.posit.co/t/how-to-turn-off-scientific-notation-like-1e-09-in-r/71575
#used to figure out how to switch the axis off of scientific notation 
ggplot(nordics, aes(x = Year, y = mtons, color = Country)) + geom_line() + 
  labs(x="Year", y = "Carbon Emissions (megatons CO2)") + xlim(1850, 2020) + 
  scale_y_continuous(labels = comma) + ggtitle("Annual CO2 Emissions of Nordic Countries")

#Q2

world_data <- datCC %>%
  filter(Region == "World")

#Note: check what exactly the anomaly thing measures
ggplot(world_data, aes(x = ptime, y = temperature_anomaly)) + geom_line(color = 'red') + 
  labs(x = "Year", y = "Temperature Increase (°C)") + ggtitle("Global Temperature Change from 1890 Average")

grouped_emissions <- datCO2 %>%
  group_by(Year) %>%
  summarize(Global_Emissions = sum(CO2))

grouped_emissions$gigatonCO2 <- grouped_emissions$Global_Emissions/1000000000

ggplot(grouped_emissions, aes(x = Year, y = gigatonCO2)) + geom_area(color = 'blue', fill = 'blue') + 
  labs(x = "Year", y = "Carbon Emissions (gigatons CO2)") + ggtitle("Annual Global CO2 Emissions") + 
  scale_y_continuous(labels = comma) + xlim(1800, 2020)

#Q3

#CITE: https://www.statology.org/plot-multiple-columns-in-r/
#Instructions on plotting multiple columns on same ggplot

#install.packages("reshape2")
library(reshape2)

energy_data <- read.csv("/cloud/project/energy-efficiency-of-meat-and-dairy-production.csv")
energy_gdp_data <- read.csv("/cloud/project/energy-use-gdp-decoupling.csv")

us_partition <- energy_gdp_data %>%
  filter(Entity == "United States")

us_partition <- us_partition[ , -c(1,2)]

colnames(us_partition)[1] <- "Year"
colnames(us_partition)[2] <- "Consumption-Based Energy"
colnames(us_partition)[3] <- "Domestic Energy"
colnames(us_partition)[4] <- "GDP"

#Converting data into percentages to match graph
us_partition$`Consumption-Based Energy` <- 100*(us_partition$`Consumption-Based Energy`-us_partition$`Consumption-Based Energy`[1])/us_partition$`Consumption-Based Energy`[1]
us_partition$`Domestic Energy` <- 100*(us_partition$`Domestic Energy`-us_partition$`Domestic Energy`[1])/us_partition$`Domestic Energy`[1]
us_partition$`GDP` <- 100*(us_partition$`GDP`-us_partition$`GDP`[1])/us_partition$`GDP`[1]


#(from above citation)
us_partition <- melt(us_partition ,  id.vars = 'Year', variable.name = 'Quantity')

#create line plot for each column in data frame (also from above citation)
#Cite: https://www.statology.org/ggplot-horizontal-line/
#Looked up how to add dotted lines at y values to make chart more interpretable
ggplot(us_partition, aes(Year, value)) +
  geom_line(aes(colour = Quantity)) + labs(x = "Year", y = "Increase Since 1995 (%)") + 
  theme_classic() + ggtitle("Changes in Energy Use vs. Changes in GDP, United States") + 
  geom_point(aes(color = Quantity)) + geom_hline(yintercept = seq(0, 100, by = 25), linetype = "dotted", color = "gray")

#ggplot(us_partition, aes(x = Year, y = consumption_based_energy)) + geom_line()
