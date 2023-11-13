library("tidyverse")

df<-read.csv("finaldata(1).csv")

df2<-df %>%
  filter(intersects_with_road==FALSE) %>%
  group_by(Name, date) %>%
  summarise(total_violent_crime=sum(total_violent_crime), 
            total_property_crime=sum(total_property_crime)) %>%
  ungroup() %>%
  mutate(date=as.Date(date)) 

n_noh<-unique(df2$Name)
d<-unique(df2$date)

library("terra")
ne<-vect("Neighborhood/Neighborhood.shp")
ndf<-as.data.frame(ne)
ndf2<-ndf %>%
  mutate(Name=toupper(Name)) %>%
  select(-OBJECTID) %>%
  filter(Name %in% n_noh)

st <- as.Date("2014-01-01")
en <- as.Date("2020-12-31")
date<-seq(st + 1, en + 1, by = "1 day") - 1

panel<-crossing(ndf2, date)

panel2<-merge(panel, df2, by=c("Name", "date"), all.x=TRUE)

panel3<-panel2 %>%
  mutate(total_violent_crime=ifelse(is.na(total_violent_crime),0, total_violent_crime)) %>%
  mutate(total_property_crime=ifelse(is.na(total_property_crime),0, total_property_crime)) %>%
  ungroup() %>%
  mutate(date=as.Date(date)) %>%
  mutate(day=day(date), month=month(date), year=year(date), dayofweek=wday(date, label=TRUE))

m<-read.csv("merra_wheelabrator.csv") %>%
  mutate(year=as.numeric(substr(file,1,4))) %>%
  mutate(month=as.numeric(substr(file,5,6))) %>%
  mutate(day=as.numeric(substr(file,7,8)))

panel4<-merge(panel3, m, by=c("year", "month", "day"), all.x=TRUE) %>%
  mutate(wind_dir=atan2(ULML, VLML)*(180/pi))
         
df<-read.csv("neighborhood_angle.csv")

w_y<- 39.270347862575214
w_x<- -76.62909892334027

x_axis <- w_y
y_axis <- w_x

df2<-df %>%
  mutate(n1x=xmin-y_axis, #y componet
         n2x=xmax-y_axis, 
         n1y=ymin-x_axis, #x component
         n2y=ymax-x_axis) %>%
  mutate(ang1=atan2(n1x,n2y)*(180/pi), ang2=atan2(n2x,n1y)*(180/pi)) %>%
  mutate(Name=toupper(Name)) %>%
  mutate(min_ang = pmin(ang1, ang2), max_ang = pmax(ang1, ang2))

all<-merge(panel4, df2, by="Name") %>%
  mutate(treatment = ifelse(wind_dir > min_ang & wind_dir< max_ang, 1, 0)) %>%
  filter(!is.na(treatment)) %>%
  select(-file, -ULML, -VLML, -X, -xmin, -xmax, -ymin, -ymax, -n1x, -n2x, -n1y, -n2y, -ang1,
         -ang2, -min_ang, -max_ang, -day, -AmInd_AkNa, -NatHaw_Pac, -Other_Race, 
         -TwoOrMore, -Shape__Len, -wind_dir)

all2<-all %>%
  group_by(Name, treatment) %>%
  tally() %>%
  filter(n>100 & treatment==1)

nt<-unique(all2$Name)

all3<-all %>%
  filter(Name %in% nt)

write.csv(all3, "paneldata.csv", row.names = F)

         