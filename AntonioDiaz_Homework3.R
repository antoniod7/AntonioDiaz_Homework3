graphics.off()
rm(list = ls());
gc()
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(coronavirus)


#Question  1
library(coronavirus)


head(coronavirus, 100)

# The first column is the data numbered, column 2 is the date, column 3 is the country
# Column 4 and 5 are the latitude and longitude for the country, column 6 is the type of cases 
# in this case confirmed, recovered of deaths from the virus and the last column is the amount of cases for that particular date

#Question 2

#Top 20 cases

top_twenty <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

top_twenty %>% head(20)



top_5 = top_twenty %>% head(5, sort = TRUE)
data.frame(top_5)

#Vertical Barplot

ggplot(top_5, aes (reorder(country, total_cases), total_cases)) + #reordered them so they appear from greatest to least 
  geom_bar(stat = "identity", color="black", fill=rgb(0.1,0.4,0.5,0.7), width= .8 )+ # outlined and changed the color of the bars to blue 
  #Decreased the bar width to .8
  theme_classic() + #added the theme classic which changed the look of the background
  labs(title="Top 5 countries by total cases",
     x ="Country", y = "Total Number of Cases")
  
#Horizontal Barplot

ggplot(top_5, aes (reorder(country, total_cases), total_cases, fill = country)) +  
  geom_bar(stat = "identity", width = .6) + #decreased the width of the bars
  theme_fivethirtyeight()+ #added the theme 538 which changes the overall look of the plot
  scale_fill_brewer(palette = "Set1") + # Set each country to be a different color
  theme(legend.position="none")+ #removed legend associated with the color theme
  coord_flip()+
  labs(title="Top 5 countries by total cases",
       x ="Country", y = "Total Number of Cases")


#Question 3

date_total = coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date)

recent_cases = data.frame(date_total)

ggplot(recent_cases, aes(date, confirmed)) +
  geom_line(color ="#9398a3", size = 1.2)+
  theme_gray()

#changed the line color to grey using hexcode
#Made the line slightly thicker
#Changed the overall theme of the graph to grey
