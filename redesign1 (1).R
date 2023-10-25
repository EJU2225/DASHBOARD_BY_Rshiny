
library(ggplot2)
library(tidyverse)
read.csv(file = 'F:/george mason/Stat 515/homework/project/canadian.csv')
canada<- read.csv(file = 'F:/george mason/Stat 515/homework/project/canadian.csv')
canada
head((canada)[2])
str(canada)
levels(canada)
data <- canada %>% subset(select = -c(Total))
head(data)
tail(data)
data <- data %>% 
  rename("1980" = 5, "1981" = 6, "1982" = 7, "1983" = 8, "1984" = 9, "1985" = 10, "1986" = 11, "1987" = 12, "1988" = 13, "1989" = 14,
         "1990" = 15, "1991" = 16, "1992" = 17, "1993" = 18, "1994" = 19, "1995" = 20, "1996" = 21, "1997" = 22,
         "1998" = 23, "1999" = 24, "2000" = 25, "2001" = 26, "2002" = 27, "2003" = 28, "2004" = 29, "2005" = 30,
         "2006" = 31, "2007" = 32, "2008" = 33, "2009" = 34, "2010" = 35, "2011" = 36, "2012" = 37, "2013" = 38)
head(data) 
tail(data)
data_2 <- data %>% 
  gather(Year, Immigrants, - c(Country, Continent, Region, DevName)) 
head(data_2) 
tail(data_2)
type <- data_2 %>% 
  group_by(DevName,Year) %>%  
  summarise( Total_Immigrants = sum(Immigrants),.groups = "drop_last")
  ggplot(type, aes(x = Year, y = Total_Immigrants, color = DevName, group = DevName  ))+ geom_line()+
  scale_color_manual(values = c("#FFC300","#581845"), name = "Regions")+geom_point()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(vars(DevName))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.spacing = unit(2, "cm", data = NULL))+labs(x="Year", y="Immigrants",title="Immigration to canada")+ 
  theme(plot.title=element_text(hjust=0.5))+
  theme(text = element_text(size = 16))+
  theme(plot.title = element_text(color = "brown"))+
  theme(axis.title.x=element_text(colour="brown"))+
  theme(axis.title.y = element_text(colour="brown"))+
  theme(strip.background = element_rect(fill = "Grey"),strip.text = element_text(color = "blue"))+
  scale_x_discrete(breaks = c(1980,1985,1990,1995,2000,2005,2010,2013))