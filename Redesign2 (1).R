df3<-read.csv("C:/Users/singh/Downloads/canadian_immegration_data.csv")
head(df3)
library(ggplot2)
library(tidyverse)
library(dplyr)
data_2 <- df3 %>% 
  gather(Year, Immigrants, - c(Country, Continent, Region, DevName, Total))
head(data_2)
tail(data_2)
continent <-  data_2  %>%
  count(Continent, wt = Immigrants) %>% 
  arrange(desc(n)) 
continent

ggplot(continent,aes(Continent, n,fill=Continent))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n, vjust = -0.3)) + 
  labs(y = "Total Number of Immigrants",title="Total No of Immigrants") +
  scale_y_continuous( label = scales::comma)+
  theme(axis.text.x = element_text(angle=30, hjust=1),legend.position = "none")