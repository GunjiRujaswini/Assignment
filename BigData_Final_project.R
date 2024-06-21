# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load

library(tidyverse) # metapackage of all tidyverse packages
library(ggplot2)
library(plotly)

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

data <- read.csv('C:/Users/gruja/Downloads/air_pol_delhi.csv')

head(data)

count(data)

data = subset(data,select=-c(query_name,state,to_date,to_time,from_time,id,city))

data

da <- data[!(is.na(data$pm25)) | !(is.na(data$pm10)) | !(is.na(data$SO2)) | !(is.na(data$CO)) | !(is.na(data$Ozone)),] 

count(da)

head(da)

month_to_numeric <- function(a){
  if(substring(a,4,6)=="Jan"){
    return (paste(substring(a,8,12),"01",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Feb"){
    return (paste(substring(a,8,12),"02",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Mar"){
    return (paste(substring(a,8,12),"03",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Apr"){
    return (paste(substring(a,8,12),"04",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="May"){
    return (paste(substring(a,8,12),"05",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Jun"){
    return (paste(substring(a,8,12),"06",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Jul"){
    return (paste(substring(a,8,12),"07",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Aug"){
    return (paste(substring(a,8,12),"08",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Sep"){
    return (paste(substring(a,8,12),"09",substring(a,1,2),sep="-"))
  }
  
  else if(substring(a,4,6)=="Oct"){
    return (paste(substring(a,8,12),"10",substring(a,1,2),sep="-"))
  }
  
  else if(substring(a,4,6)=="Nov"){
    return (paste(substring(a,8,12),"11",substring(a,1,2),sep="-"))
  }
  else if(substring(a,4,6)=="Dec"){
    return (paste(substring(a,8,12),"12",substring(a,1,2),sep="-"))
  }
  
}

da$from_date= sapply(da$from_date,month_to_numeric)

da$year<-substring(da$from_date,1,4)
da$month<-substring(da$from_date,6,7)
da$year = as.numeric(da$year)
da$date = substring(da$from_date,9,10)

head(da)

summary(table(da$year,da$month))

quantile(da$pm25,na.rm=TRUE)

quantile(da$pm10,na.rm=TRUE)

cor.test(da$pm25,da$pm10)

cor.test(da$pm25,da$pm10,method="spearman")

head(scale(da$pm25))

mean(da$pm25,na.rm=TRUE)

t.test(da$pm25,mu=119)

wilcox.test(da$pm25,conf.inf=TRUE)

shapiro.test(da$pm10)

shapiro.test(da$pm25)

head(da)

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(data=da[da$pm25>500,],mapping=aes(x=from_date,y=pm25)) + geom_point() + geom_text(label=da[da$pm25>500,]$month,nudge_x=0.25,nudge_y=0.25,check_overlap=TRUE,angle=45) + theme_gray(base_size=16)

ggplot(data=da[da$pm10>800,],mapping=aes(x=from_date,y=pm10)) + geom_point() + geom_text(label=da[da$pm10>800,]$month,nudge_x=0.25,nudge_y=0.25,check_overlap=TRUE,angle=45) +theme_gray(base_size=16)

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(data=da[da$pm10>500,],mapping=aes(x=year,y=pm10)) + geom_point() + geom_text(label=da[da$pm10>500,]$date,nudge_x=0.25,nudge_y=0.25,check_overlap=TRUE,angle=45)

ggplot(data=da[!is.na(da$pm25),],mapping=aes(x=pm10,y=pm25)) + geom_smooth(method="loess")

m = lm(da$pm25 ~da$pm10)
# ggplot(data=da,mapping=aes(x=day,y=pm10)) + geom_point() + facet_wrap(facets=vars(months_s))
plot(da$pm25 ~ da$ pm10)
abline(m)

ggplot(data=da,mapping=aes(x=pm10)) + geom_histogram(bins=40) + theme_bw() 

ggplot(data=da,mapping=aes(x=pm25)) + geom_histogram(bins=40) + theme_bw()

bad_d = da[da$pm25>60,]

table(bad_d$month,bad_d$year,bad_d$site)

unique(da$site_name)

da<- arrange(da,year)

av <- plot_ly(da[da$site_name=="Anand Vihar, Delhi - DPCC",],name="Anand Vihar",x=~reorder(from_date,year),y=~pm25,type="scatter")
dtu <- plot_ly(da[da$site_name=="DTU, New Delhi - CPCB",],name="DTU",x=~reorder(from_date,year),y=~pm25,type="scatter")
ds8 <- plot_ly(da[da$site_name=="Dwarka-Sector 8, Delhi - DPCC",],name="Dwarka",x=~reorder(from_date,year),y=~pm25,type="scatter")
igi <- plot_ly(da[da$site_name=="IGI Airport (T3), New Delhi - IMD",],name="IGI",x=~reorder(from_date,year),y=~pm25,type="scatter")
ncdu <-plot_ly(da[da$site_name=="North Campus, DU, New Delhi - IMD",],name="North Campus",x=~reorder(from_date,year),y=~pm25,type="scatter")
fig <- subplot(av,dtu,ds8,igi,ncdu)
fig

av <- plot_ly(da[da$site_name=="Anand Vihar, Delhi - DPCC",],name="Anand Vihar",x=~reorder(from_date,year),y=~pm10,type="scatter")
dtu <- plot_ly(da[da$site_name=="DTU, New Delhi - CPCB",],name="DTU",x=~reorder(from_date,year),y=~pm10,type="scatter")
ds8 <- plot_ly(da[da$site_name=="Dwarka-Sector 8, Delhi - DPCC",],name="Dwarka",x=~reorder(from_date,year),y=~pm10,type="scatter")
igi <- plot_ly(da[da$site_name=="IGI Airport (T3), New Delhi - IMD",],name="IGI",x=~reorder(from_date,year),y=~pm10,type="scatter")
ncdu <-plot_ly(da[da$site_name=="North Campus, DU, New Delhi - IMD",],name="North Campus",x=~reorder(from_date,year),y=~pm10,type="scatter")
fig <- subplot(av,dtu,ds8,igi,ncdu)
fig

da$from_date=as.Date(da$from_date)

colnames(da)

head(da[!is.na(da$pm25),])

classifypm25 <- function(val){
  if(val<=30){
    return(1)}
  else if(val>30 & val<=60){
    return(2)}
  else if (val>60 & val <=90){
    return(3)}
  else if (val>90 & val <=120){
    return(4)}
  else if (val >120 & val <=250){
    return(5)}
  else if (val>250){
    return(6)}
}

classifypm10 <- function(val){
  if(val<=50){
    return(1)}
  else if(val>50 & val<=100){
    return(2)}
  else if (val>100 & val <=250){
    return(3)}
  else if (val>250 & val <=350){
    return(4)}
  else if (val>350 & val <=430){
    return(5)}
  else if (val>430){
    return(6)}
}

da[is.na(da)] <- 0

da$aqipm25 <- sapply(da$pm25,classifypm25)
da$aqipm10 <- sapply(da$pm10,classifypm10)

plot_ly(da,x=~aqipm25,type="histogram",color=~site_name)

plot_ly(da,x=~aqipm10,type="histogram",color=~site_name)

da %>% count(site_name,sort=TRUE)

head(da)

bd=0
gd=0
for (x in da[!is.na(da$pm25),4]){
  
  if(x<=60){
    gd=gd+1;    
  } else if (x>60){
    bd=bd+1;
  }
}
sprintf("Bad AQI number of days- %i",bd)
sprintf("Good AQI number of days- %i",gd)

bd=0
gd=0
for (x in da[!is.na(da$pm10),5]){
  
  if(x<=100){
    gd=gd+1;    
  } else if (x>100){
    bd=bd+1;
  }
}
sprintf("Bad AQI number of days- %i",bd)
sprintf("Good AQI number of days- %i",gd)

bd=0
gd=0
for (x in da[!is.na(da$Ozone),8]){
  
  if(x<=100){
    gd=gd+1;    
  } else if (x>100){
    bd=bd+1;
  }
}
sprintf("Bad AQI number of days- %i",bd)
sprintf("Good AQI number of days- %i",gd)

bd=0
gd=0
for (x in da[!is.na(da$SO2),6]){
  
  if(x<=80){
    gd=gd+1;    
  } else if (x>80){
    bd=bd+1;
  }
}
sprintf("Bad AQI number of days- %i",bd)
sprintf("Good AQI number of days- %i",gd)

bd=0
gd=0
for (x in da[!is.na(da$CO),7]){
  
  if(x<=2.0){
    gd=gd+1;    
  } else if (x>2.0){
    bd=bd+1;
  }
}
sprintf("Bad AQI number of days- %i",bd)
sprintf("Good AQI number of days- %i",gd)

plot_ly(da[da$year>2017,],name="The effect of the pandemic",x=~from_date,y=~pm25,type="scatter")

plot_ly(da[da$year>2017,],name="The effect of the pandemic",x=~from_date,y=~pm10,type="scatter")

plot_ly(da[da$year>2017,],name="The effect of the pandemic",x=~from_date,y=~SO2,type="scatter")

plot_ly(da[da$year>2017,],name="The effect of the pandemic",x=~from_date,y=~Ozone,type="scatter")

plot_ly(da[da$year>2017,],name="The effect of the pandemic",x=~from_date,y=~CO,type="scatter")

plot_ly(da[(da$year>2017 & da$CO<10),],name="The effect of the pandemic",x=~from_date,y=~CO,type="scatter")

