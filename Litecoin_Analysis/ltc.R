library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
#Reading in the files that we are going to use for the analysis
ltc <- read.csv("/home/sam/R/LitecoinAn/LTC-USD.csv")

#Formatting the dates
ltc$Date <- as.Date(ltc$Date)

#Removing duplicates
ltc %>% 
  distinct(.keep_all = TRUE)

#Confirming that our data has been sufficiently cleaned
glimpse(ltc)
str(ltc)
colnames(ltc)

#Plotting out and printing the daily closing price
closing_price <- ggplot(data = ltc) + geom_line(mapping = aes(x = Date, y = Close)) +
  labs(title = "Closing price of LTC", subtitle = "From April 2013 - August 2022", caption = "This is not financial advice.")

closing_price

#Plotting the daily volume
volume <- ggplot(data = ltc) + geom_line(mapping = aes(x = Date, y = Volume)) +
  labs(title = "Volume of LTC", subtitle = "From April 2013 - August 2022", caption = "This is not financial advice.")

volume

#Plotting Volume vs closing

volume_vs_closing <- ggplot(data = ltc) + geom_smooth(mapping = aes(y = Volume, x = Close)) +
  labs(title = "Volume vs Closing of LTC", subtitle = "From April 2013 - August 2022", caption = "This is not financial advice.")

volume_vs_closing

