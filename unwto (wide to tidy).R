#this script will transform UNWTO's data from UNstats, which is particularly useful if you want to get arrival and departure statistics from certain countries


library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(scales)
library(lubridate)
library(egg)
library(ggflags) #devtools::install_github("rensa/ggflags")
library(gganimate) 

#check http://data.un.org/Search.aspx?q=tourism for the latest data
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("http://data.un.org/Handlers/DocumentDownloadHandler.ashx?id=409&t=bin", temp.file, mode="wb")

#read excel file
df <- read_excel(temp.file)

#deselect columns we don't need
df <- df %>% select(-NOTES,-3) 

#make a copy of COUNTRY column 
df <- df %>% mutate(Variables = COUNTRY)


#reorder the dataframe, use ncol(df) to count the total number of index, 
#substract with one as the new column was moved to second column
#so it will retain the number of columns incase the year columns are updated
df <- df %>% select(Country=COUNTRY,Variables,3:ncol(df)-1)

#create a vector of Variables to be replaced with NA
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

#clean the rows of Country before using lacf
df <- df %>% mutate(Country = case_when(Country %in% vars_to_replace ~ NA_character_, 
                                        TRUE ~ Country))

#use na.locf to fill the blanks at Country column using na.locf
df <- df %>% mutate_at(vars(Country), funs(na.locf))

#use distinct() to make a filter
mask <- df %>% distinct(Country) %>% as_vector()

#filter Country names
df <- df %>% filter(!str_detect(Variables, paste(mask, collapse = "|")))

#unpivot
df<- df %>% gather(-Country, -Series,-Variables, key="Year", value="Value")

#change Value column as numeric
df <- df %>% mutate(Value = as.numeric(Value))

#add dummy day and month column 
df <- df %>% mutate(Month = "12", Day = "31")

df <- df %>% mutate(Year = as.integer(Year))

#convert misc characters into  to zeros
df <- df %>%  replace(., is.na(.), "0")

#convert Value column into numeric
df <- df %>% mutate_at(vars(Value), as.numeric)

#change Country, Variables, and series into factor
df <- df %>% mutate_at(vars(Country,Variables,Series), as_factor)

#filter arrival data
df_arrival <- df %>% dplyr::filter(str_detect(Variables, "Arrivals - Thousands"))

#impute missing values by selecting the maximum value from Series (VF or TF)
df_arrival <- df_arrival %>%
  group_by(Country,Year,Variables) %>% 
  filter(Value == max(Value))

#create a rank
df_arrival <- df_arrival %>% group_by(Year, Country) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value)) %>% 
  ungroup() 

#add flag
df_formatted_flag <- df_arrival %>% 
  mutate(iso2c = countrycode(Country, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")


#flag animation
static_plot <- df_formatted_flag %>%  
  filter(rank <= 15) %>% 
  ggplot(.) +
  aes(x=rank, group=Country, fill=Country)+
  geom_bar(aes(y=Value, fill=Country, group=Country), stat="identity")+
  geom_text(aes(y = 0, label = Country, hjust = 1), size=5)+
  geom_text(aes(y = max(Value)/2, label = scales::comma(Value)), size = 5)+
  geom_flag(aes(y = max(Value)/10, country=iso2c))+ 
  scale_x_reverse()+
  xlab("Country") + 
  ggtitle("International Tourist Arrivals")+
  theme_minimal()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 2, 0, 5, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold",
                                  colour = "black", vjust = 0))+
  coord_flip(clip="off")

animation <- static_plot + transition_time(as.integer(Year))  +
  labs(title = "International Tourist Arrivals. Year: {frame_time}")

animate(animation,fps = 10,end_pause = 60, duration=30)

