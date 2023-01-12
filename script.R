library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(corrplot)

# Revenue of video games per year 1980-2016
vg <- vgsales[!(vgsales$Year %in% c("N/A", "2017", "2020")),]

revenue_per_year <- vg %>% 
  group_by(Year) %>%
  summarize(Revenue = sum(Global_Sales))

ggplot(revenue_per_year, aes(Year, Revenue)) + 
  geom_bar(fill="Sky blue",stat = "identity") +
  ggtitle("Video Game Revenue by Year") +
  guides(x =  guide_axis(angle = 90))

# Revenue of video games per publisher 1980-2016
revenue_per_publisher <- vg %>% 
  group_by(Publisher) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(-Revenue)
# Selectionner les top 10
revenue_per_publisher_10 <- revenue_per_publisher[1:10,]

ggplot(revenue_per_publisher_10, aes(x = reorder(Publisher, -Revenue), Revenue)) + 
  geom_bar(fill="Sky blue",stat = "identity") +
  ggtitle("Video Game Revenue per Publisher 1980-2016 ") +
  guides(x =  guide_axis(angle = 90))


# Revenue of video games per Game 1980-2016
revenue_per_game <- vg %>% 
  group_by(Name) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(-Revenue)
# Selectionner les top 10
revenue_per_game_10 <- revenue_per_game[1:10,]

ggplot(revenue_per_game_10, aes(x = reorder(Name, -Revenue), Revenue)) + 
  geom_bar(fill="Sky blue",stat = "identity") +
  ggtitle("Video Game Revenue per Game 1980-2016 ") +
  guides(x =  guide_axis(angle = 90))

# Revenue of video games per Platform 1980-2016
revenue_per_platform <- vg %>% 
  group_by(Platform) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(-Revenue)
# Selectionner les top 10
revenue_per_platform_10 <- revenue_per_platform[1:10,]

ggplot(revenue_per_platform_10, aes(x = reorder(Platform, -Revenue), Revenue)) + 
  geom_bar(fill="Sky blue",stat = "identity") +
  ggtitle("Video Game Revenue per Platform 1980-2016 ") +
  guides(x =  guide_axis(angle = 90))

# Sales of different publishers over the years

# top_publishers_year contient les revenues de chaque publisher par année
top_publishers_year <- vg %>% 
  subset(Publisher %in% revenue_per_publisher_10$Publisher) %>% 
  group_by(Year,Publisher) %>%
  summarise(Revenue = sum(Global_Sales))

ggplot(top_publishers_year,aes(x=Year, y=Revenue,color=Publisher, group=Publisher))+geom_line() + geom_point() + facet_wrap('Publisher') + guides(x =  guide_axis(angle = 90)) +
  theme (axis.text.x=element_text(angle=90, hjust=0.01,color=c("black","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","black","transparent","transparent","transparent","transparent","black","transparent")))

# Top 5 Genres par Région

#NA
#Pour chaque région, on somme les ventes de la région en question en les regroupant par les top 5 genres selon le nombre de ventes
top_5_genres_NA = cop %>%
  group_by(Genre) %>%
  summarize(total_NA=sum(NA_Sales))%>%
  mutate(Index=rank(-total_NA)) %>%
  filter(Index <= 5) %>%
  arrange(-total_NA)
#Visualisation
pie(top_5_genres_NA$total_NA, top_5_genres_NA$Genre, main = "Top Genres in North America")

#EU
top_5_genres_EU = cop %>%
  group_by(Genre) %>%
  summarize(total_EU=sum(EU_Sales))%>%
  mutate(Index=rank(-total_EU)) %>%
  filter(Index <= 5) %>%
  arrange(-total_EU)
pie(top_5_genres_EU$total_EU, top_5_genres_EU$Genre, main = "Top Genres in Europe")

#JP
top_5_genres_JP = cop %>%
  group_by(Genre) %>%
  summarize(total_JP=sum(JP_Sales))%>%
  mutate(Index=rank(-total_JP)) %>%
  filter(Index <= 5) %>%
  arrange(-total_JP)
pie(top_5_genres_EU$total_EU, top_5_genres_JP$Genre, main = "Top Genres in Japan")

#Other
top_5_genres_Other = cop %>%
  group_by(Genre) %>%
  summarize(total_Other=sum(Other_Sales))%>%
  mutate(Index=rank(-total_Other)) %>%
  filter(Index <= 5) %>%
  arrange(-total_Other)
pie(top_5_genres_Other$total_Other, top_5_genres_Other$Genre, main = "Top Genres in the rest of the World")


# Sales of different platforms over the years
# top_platforms_year contient les revenues de chaque platform par année
top_platforms_year <- vg%>% 
  group_by(Year, Platform) %>% 
  summarise(Number_of_games = n())

ggplot(top_platforms_year,aes(x = Year, y = Platform, fill = Number_of_games)) +
  geom_tile() +
  labs(title = "Platform heatmap over years", fill = "Number of games")+
  theme (axis.text.x=element_text(angle=90))

#1. Matrice de correlation entre les sales de différentes régions

#Sélection du sub dataset constitué des colonnes qui nous intéressent
sales <- c("Global_Sales","NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
correl <- vg[sales]
#Visualisation de la matrice
corrplot(cor(correl))




