#this script will transform UNWTO's data from UNstats, which is particularly useful if you want to get arrival and departure statistics from certain countries


library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(scales)
library(lubridate)
library(egg)

temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("http://data.un.org/Handlers/DocumentDownloadHandler.ashx?id=401&t=bin", temp.file, mode="wb")

df <- read_excel(temp.file, skip = 5)

df <- df %>% select(-NOTES,-2) 

#make a copy of COUNTRY column 
df <- df %>% mutate(variables = COUNTRY)


#reorder the dataframe, use ncol(df) to count the total number of index, 
#substract with one as the new column was moved to second column
#so it will retain the number of columns incase the year columns are updated
df <- df %>% select(COUNTRY,variables,3:ncol(df)-1)

#create a vector of variables to be replaced with NA
vars_to_replace <- c("Arrivals - Thousands", 
                     "Inbound tourism",
                     "Outbound tourism",
                     "Travel - US$ Mn",
                     "Tourism expenditure in the country - US$ Mn",
                     "Passenger transport - US$ Mn",
                     "Departures - Thousands",
                     "Tourism expenditure in the country - US$ Mn",
                     "Tourism expenditure in other countries - US$ Mn",
                     "Source: World Tourism Organization (UNWTO)")

#clean the rows of COUNTRY before using lacf
df <- df %>% mutate(COUNTRY = case_when(COUNTRY %in% vars_to_replace ~ NA_character_, 
                                        TRUE ~ COUNTRY))

#use na.locf to fill the blanks at COUNTRY column using na.locf
df <- df %>% mutate_at(vars(COUNTRY), funs(na.locf))

#use distinct() to make a filter
mask <- df %>% distinct(COUNTRY) %>% as_vector()

#filter country names
df <- df %>% filter(!str_detect(variables, paste(mask, collapse = "|")))

#unpivot
df<- df %>% gather(-COUNTRY, -Series,-variables, key="years", value="value")

#change value column as numeric
df <- df %>% mutate(value = as.numeric(value))

#add dummy day and month column 
df <- df %>% mutate(Month = "1", Day = "1")

#use paste0 with "-" to create separator on the year column
df <- df %>% mutate(Years = paste0(years,"-", Month,"-",Day))

#impute missing values
# solution source: https://stackoverflow.com/questions/30637522/impute-variables-within-a-data-frame-group-by-factor-column
# mutate_each is depreceated 

df <- df %>%
  group_by(COUNTRY,years) %>%
  mutate_each(funs(replace(., which(is.na(.)), 
                    median(., na.rm=TRUE))), 
              starts_with('value'))


#change it into date format
df <- df %>% mutate(Years = as.Date(Years))

#deselect the dummy columns
df <- df %>% select(-years,-Month,-Day)

#convert misc characters into  to zeros
df <- df %>%  replace(., is.na(.), "0")

#convert value column into numeric
df <- df %>% mutate_at(vars(value), as.numeric)


#change country, variables, and series into factor
df <- df %>% mutate_at(vars(COUNTRY,variables,Series), as_factor)


#filter
asean <- c("INDONESIA", "MALAYSIA","THAILAND",
           "PHILIPPINES", "VIET NAM", 
           "LAO PEOPLE'S DEMOCRATIC REPUBLIC", 
           "MYANMAR","BRUNEI DARUSSALAM", 
           "SINGAPORE", "CAMBODIA")

#filter arrival data
df_arrival <- df %>% dplyr::filter(str_detect(variables, "Arrivals - Thousands")
                                   )

#filter asean countries
df_arrival_asean <- df_arrival %>%  dplyr::filter(str_detect(COUNTRY,paste(asean, collapse = "|")))

#filter the series just use "TF"
df_arrival_asean <- df_arrival_asean %>%  dplyr::filter(str_detect(Series, "TF"))

#plot it

#make a label for last values
label <- df_arrival_asean %>% filter(Years == "2017-01-01")


v <- df_arrival_asean %>%  ggplot(., aes(x=Years, y=value, color=COUNTRY))+
  geom_line(stat="identity") + 
  ylab("Arrivals - Thousands") + 
  xlab("Years") + 
  ggtitle("Tourist Arrivals in Southeast Asia")+
  scale_y_continuous(labels = comma) +
  geom_point()+
  geom_label(aes(label=COUNTRY),stat="identity", data=label)
  theme_minimal()
v
v2 <- df_arrival_asean %>%  ggplot(., aes(x=Years, y=value, color=COUNTRY))+
  geom_line(stat="identity") + 
  ylab("Arrivals - Thousands") + 
  xlab("Years") + 
  ggtitle("Tourist Arrivals in Southeast Asia")+
  scale_y_continuous(labels = comma) +
  geom_point()+
  geom_label(aes(label=comma(value)),stat="identity", data=label)
theme_minimal()
v2

ggarrange(v,v2)


