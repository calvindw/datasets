#this code will transform Indonesia's statistics of annual tourist arrival in 2000-2017

library(tidyverse)
library(htmltab)
library(lubridate)
library(rvest)
library(readxl)
library(plotly)
library(scales)
library(egg) #for ggarrange
library(gganimate) #install.packages('pkg') and install.packages('gifski') if you are getting errors

#first time install for ggflags: you need to install devtools to install the package from github as it's not available on CRAN
#install.packages('devtools')
#library('devtools')
#install_github("ellisp/ggflags")
library(ggflags)

df <- htmltab("https://www.bps.go.id/statictable/2014/09/08/1394/wisatawan-mancanegara-yang-datang-ke-indonesia-menurut-kebangsaan-2000-2017.html", rm_nodata_cols = F, which=4)


#get column names from the second row
names(df) <- lapply(df[2, ], as.character)

#reset  index
rownames(df) <- 1:nrow(df)

#remove rows we don't need
df <- df[-c(1:3,22,26,44:56),]

#change column language
df <- df %>% rename(Nationality = Kebangsaan)

#clean country names from encoding problem
df <- df %>% mutate_at(vars(Nationality), str_replace_all,"Â", "")

#change names into english
df <- df %>% mutate_at(vars(Nationality), str_replace_all,
                 pattern = c("Brunei  Darussalam"="Brunei Darussalam",             
                             "Filipina" = "The Philippines",               
                             "Singapura" = "Singapore",                     
                             "Jepang" = "Japan",
                             "Korea   Selatan" = "South Korea",
                             "Srilanka"  = "Sri Lanka",
                              "Tiongkok   / Cina" = "China",
                             "Selandia   Baru"= "New Zealand",
                             "Asia   Pasifik Lainnya" = "Others (Asia Pacific)",
                             "Amerika   Serikat"="United States of America",      
                             "Kanada" = "Canada",                        
                             "Amerika   Lainnya" = "Other (Americas)",
                             "Belgia" = "Belgium",
                             "Perancis" = "France",
                             "Jerman" = "Germany",
                             "Italia" = "Italy",
                             "Belanda" = "Netherlands",
                             "Spanyol"= "Spain",                       
                             "Swedia" = "Sweden",                        
                             "Norwegia" = "Norway",                        
                             "Finlandia" ="Finland",
                             "Swiss" = "Switzerland",
                             "Inggris"= "The United Kingdom",
                             "Rusia" = "Russia",
                             "Eropa   Lainnya" = "Other Europe",
                             "Lainnya   [(]Timur Tengah dan Afrika[)]" = "Other (Middle East and Africa)"))



#unpivot
df <- df %>% gather(-Nationality, key=Years, value=Value)

#clean value column
df <- df %>% mutate_at(vars(Value), str_replace_all,"Â", "")
df <- df %>% mutate_at(vars(Value), str_replace_all,"-", "0")
df <- df %>% mutate_at(vars(Value), str_replace_all," ", "")
df$Value <- str_trim(df$Value, side="both")
df$Nationality <- str_trim(df$Nationality, side="both")


#change coutry names into factor
df <- df %>% mutate(Nationality = as_factor(Nationality))


#create helper columns to create a year column
df <- df %>% mutate(Year= paste0(Years,"-", "1","-","1")) 

#change into year
df <- df %>% mutate (Year = as.Date(Year))
df <- df %>% select(-Years)

#change Value column into numeric
df <- df %>% mutate_at(vars(Value), funs(as.numeric))

#aggregate to see who contributed the biggest tourists
x <- df %>% group_by(Nationality) %>% summarise(Value = sum(Value)) %>% arrange(desc(Value))

#reorder nationality based on the biggest contributor
fct_data <- as_vector(x$Nationality)

#apply this to the dataframe
df$Nationality <- factor(df$Nationality, levels=fct_data)

df$Year  <- year(df$Year)

df %>% mutate(Value = as.integer(Value))

#create a rank
df_formatted <- df %>% group_by(Year, Nationality) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value)) %>% 
  ungroup() %>% 
  filter(rank <= 20) 

#add iso2c column (used for flag animation with geom_flag)
df_formatted_flag <- df_formatted %>% 
  mutate(iso2c = countrycode(Nationality, origin='country.name.en', destination='iso2c'))  %>% 
  mutate(iso2c = tolower(iso2c)) %>%
  filter(iso2c != "NA")

#if no error message, plot it:
v <- df %>%  
  ggplot(., aes(x=Year, y=Value, fill=Nationality)) + 
  geom_bar(stat="identity")+
  xlab("Years") + 
  ggtitle("Tourist Arrivals in Indonesia based on Country of Origin")+
  theme_minimal()+
  facet_wrap(.~Nationality,scales = "free")+
  scale_y_continuous(labels = comma)+
  theme(legend.position = "none") 
 
ggplotly(v) 

#to fix the scale in facet_wrap
ggarrange(v)

##animation (without flag)
static_plot <- df_formatted %>% 
  ggplot(.)+
  aes(x=rank, group=Nationality, fill=Nationality)+
  geom_bar(aes(y=Value, fill=Nationality, group=Nationality), stat="identity")+
  geom_text(aes(y = 0, label = Nationality, hjust = 1), size=5)+
  geom_text(aes(y = Value, label = scales::comma(Value), hjust = 0), size = 5)+
  scale_x_reverse()+
  xlab("Country") + 
  ggtitle("Tourist Arrivals in Indonesia based on Country of Origin (2000-2017)")+
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
  labs(title = "Tourist Arrivals in Indonesia. Year: {frame_time}")

animate(animation,fps = 10,end_pause = 60, duration=30)


##animation with flag
#the key is to ensure each data has a corresponding country name variable in iso2c and in lowercase format

static_plot <- df_formatted_flag %>%
  ggplot(.) +
  aes(x=rank, group=Nationality, fill=Nationality)+
    geom_bar(aes(y=Value, fill=Nationality, group=Nationality), stat="identity")+
    geom_text(aes(y = 0, label = Nationality, hjust = 1), size=5)+
    geom_text(aes(y = max(Value)/2, label = scales::comma(Value)), size = 5)+
    geom_flag(aes(y = max(Value)/10, country=iso2c))+ 
  scale_x_reverse()+
  xlab("Country") + 
  ggtitle("Tourist Arrivals in Indonesia based on Country of Origin (2000-2017)")+
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
  labs(title = "Tourist Arrivals in Indonesia. Year: {frame_time}")

animate(animation,fps = 10,end_pause = 60, duration=30)
