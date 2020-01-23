#this code will transform Indonesia's statistics of annual tourist arrival in 2000-2017

library(tidyverse)
library(htmltab)
library(lubridate)
library(rvest)
library(readxl)
library(plotly)
library(scales)

#get the data from the website
df <- htmltab("https://www.bps.go.id/statictable/2014/09/08/1394/wisatawan-mancanegara-yang-datang-ke-indonesia-menurut-kebangsaan-2000-2017.html", rm_nodata_cols = F, which=4)

#get column names from the second row
names(df) <- lapply(df[2, ], as.character)

#reset  index
rownames(df) <- 1:nrow(df)

#remove rows we don't need
df <- df[-c(1:3,22,26,44:56),]

#unpivot
df <- df %>% gather(-Kebangsaan, key=Years, value=Value)

#clean value column
df <- df %>% mutate_at(vars(Kebangsaan), str_replace_all,"Â", "") #double check whether the character is really "Â", as it seems an encoding problem.
df <- df %>% mutate_at(vars(Value), str_replace_all,"Â", "")
df <- df %>% mutate_at(vars(Value), str_replace_all,"-", "0")
df <- df %>% mutate_at(vars(Value), str_replace_all," ", "")
df$Value <- str_trim(df$Value, side="both")
df$Kebangsaan <- str_trim(df$Kebangsaan, side="both")

#change country column into factor
df <- df %>% mutate(Nationality = as_factor(Kebangsaan))

df <- df %>% select(-Kebangsaan)

#create helper columns to create a year column
df <- df %>% mutate(Year= paste0(Years,"-", "1","-","1")) 

#change into year
df <- df %>% mutate (Year = as.Date(Year))

#change Value column into numeric
df <- df %>% mutate_at(vars(Value), funs(as.numeric))

#create a rank (will be used for animation)
df_formatted <- df %>% group_by(Year, Nationality) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(rank = rank(-Value)) %>% 
  ungroup() %>% 
  filter(rank <= 20) 

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

##animation
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
  #transition_states(Nationality, transition_length = 3, state_length = 1, wrap = FALSE)+
  labs(title = "Tourist Arrivals in Indonesia. Year: {frame_time}")

animate(animation,fps = 3, end_pause = 60)
