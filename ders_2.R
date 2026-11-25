library(dplyr)
library(palmerpenguins)
library(tidyverse)
library(nycflights13)
#filtrelemeler
jan1<-filter(flights,month==1,day==1)
nov_dec<filter(flights,month%in%c(11,12))
filter(flights,arr_delay<=120,dep_delay<=120)

arrange(flights,year,month,day)#satirlari siralamak icin
#azalan icin ozelligin basina desc yaziliyor

select(flights,year,month,day)#secmek icin 
#starts_with("abc"),end_with("xyz"),contains("ijk")
flights_sml<-select(flights,year:day,
                    ends_with("delay"),
                    distance,
                    air_time)
mutate(flights_sml,
gain=dep_delay-arr_delay,
speed=distance/air_time*60)#yeni degiskenler olusturmak icin
#sadece yeni olusturdugumuz degisrknleri saklaak icin 
transmute(flights_sml,
          gain=dep_delay-arr_delay,
          speed=distance/air_time*60)
#mutate()

#summarise()

by_day<-group_by(flights,year,month,day)
summarise(by_day,
          delay=mean(dep_delay))
by_dest<-group_by(flights,dest)
delay<-summarise(by_dest,
                 count=n(),
                 dist=mean(distance,na.rm=TRUE,
                           delay=mean(arr_delay,na.rm=TRUE)))
delay2<-filter(delay,count>20,dest!="HNL")
summary(delay2$count)
#pipe opertorunun kullanımı;bu opertor tekrar tekrar degisken oluturup hafizaya kadetmemize gerek kalmadan islem yapmamişzi saglar bellek yonetimi
delay2_yeni<-flights%>%group_by(dest)%>%
  summarise(
    count=n(),
    dist=mean(distance,na.rm=TRUE,
              delay=mean(arr_delay,na.rm=TRUE)))
not_cancelled<-flights%>%filter(!is.na(dep_delay),!is.na(arr_delay))
not_cancelled%>%
  group_by(year,month,day)%>%
  summarise(n_early=sum(dep_time<500))
#mpg veri kumesi kullanaraka plot olusturalim
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=class))