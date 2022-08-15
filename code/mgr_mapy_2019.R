library(rgdal)
library(ggplot2)
library(ggmap)

# geographical centre of Poland
zachpom <- get_stamenmap(
  bbox = c(left = 14.041, bottom = 52.503, right = 17.369, top = 54.591), 
  maptype = "watercolor",
  zoom = terraom)

zachpomMap <- ggmap(zachpom) 

#Wczytanie mapy przygotowanej wojewodztwa w shp
gminy <-readOGR("zachpom_gminy.shp")
gminy <- spTransform(gminy, CRS("+proj=longlat +datum=WGS84"))
gminy <- fortify(gminy)
str(gminy)
#wyrysowanie mapy ggplotem
zachpom_plot <- ggplot(data = gminy, aes(x=long, y=lat, group=group)) +
  geom_path() +
  theme_map()



zachpomMap <- zachpomMap + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = 'grey', 
               size = 1,
               color = 'red',
               data = gminy,
               alpha = 0.3)
zachpomMap



library(sf)
#proba inaczej
gminy2 <- read_sf('zachpom_gminy.shp')
plot(gminy2)
str(gminy2)
head(gminy2)
gminy2 <- select(gminy2, 2,5) #usunalem tez inne niepotrzebne


data_map2019 <- select(Gus_2019, 1:2)
data_map2019$ID <- seq.int(nrow(data_map2019)) #dodaje ID zebv potem ³atwo po³¹czyæ dane

data_map2019_temp <- select(gus2019_base, 1:7)
data_map2019_temp$ID <- seq.int(nrow(data_map2019_temp))

data_map2019 <- right_join(data_map2019,data_map2019_temp, by = "ID")

names(gminy2)[1] <- "TERYT" #zmiana nazwy zeby po³¹czyæ

gminy2 <- right_join(gminy2,data_map2019 )
gminy2 <- select(gminy2, -"ID") #usuwam niepotrzebne kolumny - ID, JPT_nazwa
plot(select(gminy2, 4:10)) #mapka prosta z wskaznikami syntetycznymi
gminy2 <- fortify(gminy2) #dataframe

#OGARN¥Æ 
mapa_2019 <- ggplot()+
  geom_sf(aes(fill=Poziom_Rozwoju),color='transparent',data=gminy2)+
  geom_sf(fill='transparent',color='black',data=gminy2)+
  scale_fill_viridis_d(name='Poziom Rozwoju',
                       guide=guide_legend(
                         direction='horizontal',
                         title.position='top',
                         title.hjust = .5,
                         label.hjust = .5,
                         label.position = 'bottom',
                         keywidth = 3,
                         keyheight = .5
                       ))+
  labs(caption=c('ród³o: Opracowanie w³asne'))+
  theme_gray()+
  theme(title=element_text(face='bold'),
        legend.position = 'bottom')

sciagawka <- select(sciagawka, -1)
library(dplyr)
