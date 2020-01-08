library(tidyverse)
library(readxl)
library(lubridate)
library(wbstats)
library(plotly)
library(scales)

df <- read_delim("https://raw.githubusercontent.com/calvindw/datasets/master/UN_Budget_2001_2020.csv", 
                 ";", escape_double = FALSE, col_types = cols(Period = col_date(format = "%m/%d/%Y")), 
                 trim_ws = TRUE)

#remove dummy date columns
df <- df %>% dplyr::select(-Month, -Day)

#convert period column into a proper date format
df <- df %>% mutate(Period = as.Date(Period))

#convert member state into factor
df <- df %>% mutate(`Member State` = as_factor(`Member State`),
                    iso3c = as_factor(iso3c),
                    Key = paste0(Year,"-",iso3c))

#pull data from WB
#use countries_only to pull all data from country parameter

#use this to prevent scientific notation
options(scipen=999)

gdp <- wb(country="countries_only",
          indicator = 'NY.GDP.MKTP.CD',
          startdate = 2001,
          enddate = 2020)

#make key column on budget
gdp <- gdp %>% mutate(Key = paste0(date,"-",iso3c))

#left_join gdp and budget dataframe 
df_budget_gdp <- left_join(x=gdp,
                           y=df,
                           by=c("Key"="Key")) 

#make a percentage
#df_budget_gdp <- df_budget_gdp %>% mutate(budget_to_gdp = `Net contributions`/value)

#plot it
v<- df_budget_gdp %>% ggplot(.,aes(x=value,y=`Net contributions`,color=country)) + 
  geom_point()+
  ggtitle("Member State's GDP vs UN Budget")+
  xlab("GDP (current US$)")+
  ylab("Budget (current US$)")+
  scale_y_continuous(label=comma)+
  scale_x_continuous(label=comma)


ggplotly(v)