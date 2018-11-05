#First load all libraries

library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(tidyverse)
library(ggsn)
library(GISTools)
# import U.S. Data 
states <- map_data("state")
states
ggplot(data=states, mapping=aes (x=long, y=lat, group=group))+
  geom_polygon()
coord_map()

# get Spatial-Data from california  
Californiamap<- map_data (map="county",region="California")
ggplot(Californiamap)+  
  geom_polygon(mapping=aes (x=long, y=lat, group=group), color= "blue", fill="orange")+
  coord_map()+ 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

#Now I'll load the data
choose.files()
#chooooose your files

crimecali <- read.csv()
crimecali <- crimecali %>%  
  mutate(subregion = as.factor(subregion), numerator = as.factor(numerator))

#Load the Data
Californiamap <- map_data(map = "county", region= "California")


crime_summary <- crimecali %>%
  group_by(subregion) %>%
  summarise(numerator=n())

crime_summary
Californiamap

mapdatacali <- merge(Californiamap, crime_summary, by ="subregion")

C <- ggplot(mapdatacali)+
  geom_polygon(mapping=aes(x=long, y=lat, group=group, fill=numerator))+
  coord_map()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  scale_fill_gradient(low="ivory", high="navyblue") +
  theme(legend.key = element_blank())+
  ggtitle("NUMBER OF VIOLENT CRIMES IN CALIFORNIA IN 2017",
          subtitle = "Source:Department of Justice in California")+
  labs(fill='1000 per population ') +
  scalebar(data= mapdatacali,location="bottomleft",y.min=25.3755, y.max=26.3715, 
           x.min=-111.865, x.max=-110.869, dist=200, st.size = 2.5, dd2km= TRUE, model='WGS84',
           st.dist=.02)
north2(C, x = .20, y = .25, scale = .1, symbol = 9) 



#########















