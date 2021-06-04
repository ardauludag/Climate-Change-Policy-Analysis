install.packages("ggrepel") 
library(tidyverse)
library(ggrepel)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(cowplot)
install.packages("extrafont")
library(extrafont)
library(forcats)
library(lubridate)
install.packages("plotly")
library(plotly)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(devtools)
install_github 
library(patchwork) # We use patchwork to create multiple plots easily.

#### Loading Data

getwd()
setwd("Downloads/")
getwd()
EUAData <- read_csv("Carbon Emissions Futures Historical Data(2).csv") # Loading our EUA Data
view(EUAData)
summary(EUAData)
EUAData <- EUAData %>%
  drop_na()
EUAData[0:2] # For the beginning we are just analyzing price movement, thus we select date and price columns from dataset.

#### Editing Date Format to Convert it into Y-M-D

EUAData$Date #Date format is currently M-D, Y.
date <- as.character(EUAData$Date)
date <- mdy(EUAData$Date)
EUAData$Date <- as.Date(date, format = "%Y-%m-%d")
str(EUAData$Date)

#### Visualizing EUA Data

plot(EUAData$Date, EUAData$Price) 
view(EUAData)

par("mar") # For more complicated graph we need to fix the shape of x-axis. Otherwise legend of x-axis might be seen unreadable.
par(mar=c(1,1,1,1))

graph1 <- ggplot(EUAData, aes(x=EUAData$Date, y=EUAData$Price, color = "red")) + geom_line(aes()) + labs(title="Historical Levels in EUA Prices", x = "", y="Euro (per CO2e)", caption = "Source: ICE") +theme_economist() + theme(legend.position = "none") + scale_x_date(date_labels = "%m-%Y")
graph1 


#### Volume Analysis

volume <- EUAData$Vol.
volume

df <- data.frame(EUAData)
df1 <- select(df, Date , Vol.)
view(df1)

df1$Vol. <- as.numeric(sub("K", "e3", df1$Vol., fixed = TRUE)) # Since Volume information appears with K instead of numerical characters, we need to correct it. 
df1

graphvol <- ggplot(df1, aes(x=Date, y=Vol., color="red"))+geom_area() + labs(title="Market Volume Started to Increase in December 2020", x = "", y="Volume", caption = "Source: ICE") +theme_economist()
graphvol1 <- graphvol + scale_x_date(date_labels = "%m-%Y") + theme(legend.position = "none") # Since ggtheme does not allow to remove legend problem with a single line code, we fix it with a second line.
graphvol1


#### Integrating Other Variables into Analysis

EUAData <- read_csv("Carbon Emissions Futures Historical Data(2).csv")
CoalData <- read_csv("Rotterdam Coal Futures Historical Data.csv")
GasData <- read_csv("Natural Gas Futures Historical Data.csv")
OilData <- read_csv("Brent Oil Futures Historical Data.csv")
StoxxData <- read_csv("STOXX600 Oil&Gas Futures Historical Data.csv")

summary(CoalData)
summary(GasData)
summary(OilData)
summary(StoxxData)
summary(EUAData)


#### Merging All Data

dfmix1 <- left_join(CoalData, EUAData, by = c("Date"="Date"))
dfmix1 <- subset(dfmix1, select = c("Date", "Price.x", "Price.y"))
dfmix1 %>%
  rename(EUA = Price.x, 
         Coal = Price.y)

  names(dfmix1)[names(dfmix1) == "Price.x"] <- "EUA"
  names(dfmix1)[names(dfmix1) == "Price.y"] <- "Coal"

dfmix2 <- left_join(GasData, dfmix1, by = c("Date"="Date"))
dfmix2 <- subset(dfmix2, select = c("Date", "EUA", "Coal", "Price"))
names(dfmix2)[names(dfmix2) == "Price"] <- "Gas"

dfmix3 <- left_join(StoxxData, dfmix2, by = c("Date"="Date"))
dfmix3 <- subset(dfmix3, select = c("Date", "EUA", "Coal", "Gas", "Price"))
names(dfmix3)[names(dfmix3)=="Price"] <- 'Stoxx600'

dfmix4 <- left_join(OilData, dfmix3, by = c("Date"="Date"))
dfmix4 <- subset(dfmix4, select = c("Date", "EUA", "Coal", "Price", "Gas", "Stoxx600"))
names(dfmix4)[names(dfmix4)=="Price"] <- 'Oil'


#### Editing Date Format

datemix <- mdy(dfmix4$Date)
str(datemix)
date2 <- as.Date(datemix, format = "%Y-%m-%d")

dfmix4$Date <- date2
view(dfmix4)

#### Quick Data Cleaning

summary(dfmix4)

dfmix4 <- dfmix4 %>% # We need to clean missing data, and since the ratio of those cells are less than 20% of the data, we may delete the whole row of missing values.
  drop_na()

summary(dfmix4)
dfmix4

#### Visualizing All Data
graphoil <- ggplot(dfmix4, aes(x=Date, y=dfmix4$Oil, color="red")) + geom_line() + theme_economist() + scale_x_date(date_labels = "%m-%Y") + theme(legend.position = "none") + labs(title="Oil", x = "", y="USD")
graphcoal <- ggplot(dfmix4, aes(x=Date, y=dfmix4$Coal, color="red")) + geom_line() + theme_economist() + scale_x_date(date_labels = "%m-%Y") + theme(legend.position = "none") + labs(title="Coal", x = "", y="USD")
graphstoxx600 <- ggplot(dfmix4, aes(x=Date, y=dfmix4$Stoxx600, color="red")) + geom_line() + theme_economist() + scale_x_date(date_labels = "%m-%Y") + theme(legend.position = "none") + labs(title="STOXX600 Oil & Gas", x = "", y="Euro")
graphgas <- ggplot(dfmix4, aes(x=Date, y=dfmix4$Gas, color="red")) + geom_line()+theme_economist() + scale_x_date(date_labels = "%m-%Y") + theme(legend.position = "none") + labs(title="Natural Gas", x = "", y="Pound", caption = "Source: ICE, investing.com")
grapheua <- ggplot(dfmix4, aes(x=Date, y=dfmix4$EUA, color="red")) + geom_line()+theme_economist() + scale_x_date(date_labels = "%m-%Y") + theme(legend.position = "none") + labs(title="EUA", x = "", y="Euro")

grapheua / (graphgas | graphoil) / (graphcoal | graphstoxx600)

#### Correlation Analysis

dfmixcor <- dfmix4[2:6]
dfmixcor

dfmixcortable <- cor(dfmixcor)

view(round(dfmixcortable, 2))

chart.Correlation(dfmixcor, histogram=TRUE, pch=19)


