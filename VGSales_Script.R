install.packages("tidyverse")
library (tidyverse)

#Loading the three datasets
ps4_sales<-read.csv("PS4_GamesSales.csv")
View(ps4_sales)
xbox_sales<-read.csv("XboxOne_GameSales.csv")
View(xbox_sales)
vg_sales<-read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
View(vg_sales)
str(vg_sales)

#Data Cleaning packages
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
install.packages("dplyr")
library(dplyr)

#Taking a peak at the different datasets and some basic cleaning checks
skim_without_charts(ps4_sales)
glimpse(xbox_sales)
head(vg_sales)
clean_names(ps4_sales)

ps4_sales %>% 
  arrange(Year)

vg_sales %>%
  group_by(Publisher) %>%
  drop_na()

clean_ps4 <- ps4_sales %>% filter(Global != 0, Year != "N/A")
View(clean_ps4)

Yearly_ps4_Sales<-clean_ps4 %>% group_by(Year)%>%summarize(sum(Global))
View(Yearly_ps4_Sales)

Yearly_xbox_sales<-xbox_sales %>% group_by(Year)%>%summarize(sum(Global))
View(Yearly_xbox_sales)

Yearly_nintendo_sales <- vg_sales %>% group_by(Year_of_Release) %>%  filter(Publisher == "Nintendo")%>%summarize(sum(Global_Sales))
View(Yearly_nintendo_sales)

#Visualizations
install.packages("ggplot2")
library(ggplot2)

#Visualizing what game sales look like corresponding to their critical scores
ggplot(data = vg_sales) + geom_point(mapping = aes(x=Year_of_Release, y = Global_Sales))

#Visualizing yearly sales
colnames(Yearly_ps4_Sales)[2] = "sales"
ggplot(data=Yearly_ps4_Sales) + geom_col(mapping = aes(x=Year, y=sales, fill = Year))

colnames(Yearly_xbox_sales)[2] = "sales"
ggplot(data=Yearly_xbox_sales) +
  geom_col(mapping = aes(x=Year, y=sales, fill = Year))+
  labs(title = "Yearly Xbox game sales")

#Creating a table to compare sales
sales_comp <- merge(x = Yearly_ps4_Sales, y = Yearly_xbox_sales, by = "Year")
colnames(sales_comp)[2] = "Ps4_Sales"
colnames(sales_comp)[3] = "XBox_Sales"
View(sales_comp)

ggplot(data = sales_comp) +
  geom_col(mapping = aes(x = Year, y = Ps4_Sales, fill = "ps4_Sales"))+
  geom_col(mapping = aes(x = Year, y = XBox_Sales, fill = "xBox_Sales") )+
  labs(y = "Sales", title = "Ps4 vs Xbox Game Sales")+
  theme(legend.position = "bottom")

#Best Genres
genre_by_p <- clean_ps4 %>% group_by(Genre) %>% summarize(sum(Global))
View(genre_by_p)
install.packages("rmarkdown")
