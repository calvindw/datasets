#this code will transform UNWTO data hosted on data.un.org, this code will focus on tourist arrivals and tourist departures data

#Load Libraries----
library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(scales)
library(lubridate)
library(egg)
library(gganimate)
library(wbstats)
library(countrycode)
library(ggflags)
library(ggrepel)



# Extract Data ------------------------------------------------------------

temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("http://data.un.org/Handlers/DocumentDownloadHandler.ashx?id=409&t=bin", temp.file, mode="wb")


df <- read_excel(temp.file)



# Data Cleaning ====
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
#df <- df %>% mutate(Month = "12", Day = "31")

df <- df %>% mutate(Year = as.integer(Year))

#convert misc characters into  to zeros
df <- df %>%  replace(., is.na(.), "0")

#convert Value column into numeric
df <- df %>% mutate_at(vars(Value), as.numeric)

#change Country, Variables, and series into factor
df <- df %>% mutate_at(vars(Country,Variables,Series), as_factor)


#impute missing values by selecting the maximum value from group 
df <- df %>%
  group_by(Country,Year,Variables) %>% 
  filter(Value == max(Value))

#filter arrival data
df_arrival <- df %>% dplyr::filter(str_detect(Variables, "Arrivals - Thousands"))

# Copy data frames ====

#create a copy for bar animation
df_formatted_flag <- df_arrival

#create a copy for geom_line visualization
df_rank_by_region <- df_arrival

#create a copy for departures data
df_departures <- df %>% dplyr::filter(str_detect(Variables, "Departures - Thousands"))


# Get World Bank data ====
#use wbstats to pull countries data, let's just name the object x
#x <- wbcache()

#this will create a list, subset countries from this list and just overwrite the object
#x <- x$countries

#alternatively just use a copy of the data that I uploaded to my github
x <- read.csv2(url("https://raw.githubusercontent.com/calvindw/datasets/master/countries.csv"), stringsAsFactors =FALSE)

#transform the iso2c column from x to lowercase so it can be used for left_join with country_code
x <- x %>% mutate(iso2c = tolower(iso2c))


# Data cleaning for reverse bar animation with geom_bar ====

#create a rank
df_formatted_flag <- df_formatted_flag %>% group_by(Year, Country) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value, ties.method = "first")) %>% 
  ungroup() 

#add flag
df_formatted_flag <- df_formatted_flag %>% 
  mutate(iso2c = countrycode(Country, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")

#left join with formatted data
df_formatted_flag <- left_join(x=df_formatted_flag,
                               y=x[,c('iso2c','region','income')],
                               by='iso2c')

#remove NA regions
df_formatted_flag  <- df_formatted_flag %>% filter(!(region %in% NA))

#convert into factor
df_formatted_flag <- df_formatted_flag %>% 
  mutate(Region = as_factor(region), Country = as_factor(Country)) %>% 
  select(-region)


# Data cleaning for geom_line facet_wrap plot ====

#group by year, region, country (for geom_line facet_wrap)
df_rank_by_region <- df_rank_by_region %>% ungroup()

#country code
df_rank_by_region <- df_rank_by_region %>% 
  mutate(iso2c = countrycode(Country, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")

#left join with world bank data
df_rank_by_region <- left_join(x=df_rank_by_region,
                                      y=x[,c('iso2c','region','income')],
                                      by='iso2c')
#filter out NA values
df_rank_by_region <- df_rank_by_region %>% filter(region != "NA")

#rename region
df_rank_by_region <- df_rank_by_region %>% rename(Region = region)

#filter out zero values
df_rank_by_region <- df_rank_by_region %>% 
  filter((!Value %in% 0))

#make a rank based on year, region, country
df_rank_by_region <- df_rank_by_region %>% group_by(Year, Region,Country) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value, ties.method = "first")) %>% 
  ungroup() 

#as the country code was removed due to summarsation, make another 
df_rank_by_region <- df_rank_by_region %>% 
  mutate(iso2c = countrycode(Country, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")

#left join to get income factor
df_rank_by_region <- left_join(x=df_rank_by_region,
                               y=x[,c('iso2c','income')],
                               by='iso2c')



# Visualization -----------------------------------------------------------
# Animation with flag (arrival data)====
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

animate(animation, renderer=av_renderer("unwto.mp4"), fps = 20,end_pause = 120, duration=60, rewind=FALSE)




# Facet wrap with bars (tourist arrivals)====
static_plot <- df_formatted_flag %>% 
  ggplot(.) +
  geom_bar(aes(x=Year, y=Value, fill=Region), stat="identity")+
  ggtitle("International Tourist Arrivals 1995-2018 (Thousands)")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(vars(income, Country), ncol=10, shrink=FALSE)


ggsave(plot=static_plot, filename="plt.png", width=100, height=100, units = "cm", limitsize=FALSE)



# Facet wrap with line (tourist arrivals) ====
options(scipen=999)
library(directlabels)
df_rank_by_region <- df_rank_by_region %>% 
  filter((!Value %in% 0))


static_plot <- df_rank_by_region %>%
  ggplot(.) +
  geom_line(aes(x=Year, y=rank, color=Country), stat="identity", linetype="dashed")+
  ggtitle("International Tourist Arrivals 1995-2018 (Thousands)")+
  #geom_dl(aes(x=Year, y=rank, label = Country), method = list(dl.combine("last.points", "first.points"), cex = 0.8))+
  geom_flag(data=subset(df_rank_by_region, Year %in% c(2018,1995)), aes(x=Year, y = rank, country=iso2c, size=6))+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(Region~., scales="free")+
  scale_y_reverse()



ggsave(plot=static_plot, filename="plt.png", width =30, height=30, units = "cm", limitsize=FALSE)



# Data Cleaning for departures====

#create a rank
df_departures <- df_departures %>% group_by(Year, Country) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value)) %>% 
  ungroup() 

#add flag
df_formatted_flag <- df_departures %>% 
  mutate(iso2c = countrycode(Country, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")

# Animation with flag (departures data)====

static_plot <- df_formatted_flag %>%
  filter(rank <= 15, Value != 0) %>% 
  ggplot(.) +
  aes(x=rank, group=Country, fill=Country)+
  geom_bar(aes(y=Value, fill=Country, group=Country), stat="identity")+
  geom_text(aes(y = 0, label = Country, hjust = 1), size=2.5)+
  geom_text(aes(y = max(Value)/2, label = scales::comma(Value)), size = 5)+
  geom_flag(aes(y = max(Value)/10, country=iso2c))+ 
  scale_x_reverse()+
  xlab("Country") + 
  ggtitle("International Tourist Departures 2017 (Thousands)")+
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



ggsave(plot=static_plot, filename="plt.png", width = 20, height = 80, units = "cm")


animation <- static_plot + transition_time(as.integer(Year))  +
  labs(title = "International Tourist Departures (Thousands). Year: {frame_time}")

animate(animation, renderer=av_renderer("unwto.mp4"), fps = 20,end_pause = 120, duration=60, rewind=FALSE)


# Compare GDP per capita vs departure====

library(tidyverse)
library(lubridate)
library(wbstats)
library(plotly)
library(scales)
library(gganimate)
library(egg)
library(ggflags)
options(scipen=999)

#search gdp from world bank stats and put into a dataframe (optional)
#gdp_search <- wbsearch(pattern = "gdp")

#pull gdp and gdp per capita data
gdp <- wb(country="countries_only",
          indicator = c('NY.GDP.PCAP.CD'),
          startdate = 2017,
          enddate = 2020)
gdp <- gdp %>% mutate(iso2c = tolower(iso2c))
gdp_2017 <- gdp %>% filter(date %in% 2017) %>% select(iso2c, GDP_per_capita=value)


#filter departure data
df_departures <- df %>% dplyr::filter(str_detect(Variables, "Departures - Thousands"))

#create a rank
df_departures <- df_departures %>% group_by(Year, Country) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value)) %>% 
  ungroup() 

#add flag
df_formatted_flag <- df_departures %>% 
  mutate(iso2c = countrycode(Country, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")


df_2017 <- df_formatted_flag %>%
  filter(Year %in% 2017, Value != 0)

df_gdp_outbound <- left_join(x=df_2017,
                             y=gdp_2017,
                             by="iso2c")

df_gdp_outbound <- df_gdp_outbound %>% rename(outbound_tourism=Value)
df_gdp_outbound <- df_gdp_outbound %>% filter(!is.na(GDP_per_capita))
df_gdp_outbound <- df_gdp_outbound %>% mutate(median_GDP_pc = median(GDP_per_capita, na.rm=TRUE))

#regression line
reg<-lm(outbound_tourism~GDP_per_capita, data = df_gdp_outbound)
coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

static_plot <- df_gdp_outbound %>%
  mutate(log_x= log10(GDP_per_capita), log_y=log10(outbound_tourism)) %>% 
  ggplot(.) +
  aes(x=log_x, y=log_y, color=Country)+
  geom_point(alpha=0.7) +
  geom_abline(intercept=log10(8225), slope=0.2)+
  theme_minimal()+
  #scale_x_log10()+
  #scale_y_log10()+
  theme(legend.position = "none")+
  scale_color_viridis_d()+
  xlab("GDP per Capita")+
  ylab("Outbound Tourism (Thousands)")
static_plot

ggplotly(static_plot)
