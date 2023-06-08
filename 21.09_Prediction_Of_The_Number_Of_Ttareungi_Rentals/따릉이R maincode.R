library(gridExtra)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(corrplot)
library(data.table)
#2017년 4월 1일부터, 5월 31일까지 시간별로 서울시 따릉이 대여수와 기상상황 데이터가 주어집니다.

#각 날짜의 1시간 전의 기상상황을 가지고 1시간 후의 따릉이 대여수를 예측

setwd("C:/Users/배정민/Desktop/2021 9월 프로젝트/data/제공데이터")
train<-read.csv("train.csv")
test<-read.csv("test.csv")

str(train) # 1459행 11열
head(train)
# id                     날짜와 시간별 id
# hour                  
# hour_bef_temperature   1시간 전 기온
# hour_bef_precipitation 1시간 전 강수여부 : 비가 오지 않았으면 0, 비가 오면 1
# hour_bef_windspeed     1시간 전 풍속
# hour_bef_humidity      1시간 전 습도
# hour_bef_visibility    1시간 전 시정(특정 기상 상태에 따른 가시성)
# hour_bef_ozone         1시간 전 오존
# hour_bef_pm10          1시간 전 미세먼지(머리카락 굵기의 1/5에서 1/7 크기의 미세먼지)
# hour_bef_pm2.5         1시간 전 미세먼지(머리카락 굵기의 1/20에서 1/30 크기의 미세먼지)
# count(타겟변수)        시간에 따른 따릉이 대여 수 

############################################################
#여기부터 외부데이터
setwd("C:/Users/배정민/Desktop/2021 9월 프로젝트/data/가공데이터")
weather<-read.csv("기상데이터.csv")
sum(is.na(match(train$hour_bef_temperature,weather$기온..C.))) #2

which(is.na(match(train$hour_bef_temperature,weather$기온..C.))) #935 1036번째 행
train[c(935,1036),] #온도 값이 없어서 애초에 대응이 안되는 행

##변수 추가로 생성 : 일시->날짜,시간,요일로 만듬
weather$일시<-as.POSIXct(weather$일시)
weather$일시<-weather$일시+hours(1) #1시간 뒤 따릉이 대여 수에 대한 일시로 바꿔줌
weather <- weather %>% mutate(day=as.factor(weekdays(weather$일시)) )
weather <- weather %>% mutate(hour=ifelse(substr(일시,12,12)=='0', substr(일시,13,13),substr(일시,12,13)))
weather <- weather %>% mutate(date = as.Date(substr(일시,1,10)))

head(weather,30)
str(weather)
write.csv(weather,"weather.csv")
##데이터 합치기
train <- data.table(train)
weather <- data.table(weather)

colnames(weather) <- c('지점','지점명','일시','hour_bef_temperature',
                       'hour_bef_precipitation','hour_bef_windspeed',
                       'hour_bef_humidity','hour_bef_visibility','day','hour',
                       'date')

setkeyv(train, c('hour_bef_temperature','hour_bef_windspeed',
                 'hour_bef_humidity','hour_bef_visibility'))
setkeyv(weather, c('hour_bef_temperature','hour_bef_windspeed',
                   'hour_bef_humidity','hour_bef_visibility'))
newtrain <- merge(train,weather,all.x=T)
colnames(newtrain)
newtrain2<-newtrain%>%select(c("id","hour.x","hour.y", "date","day",
                               "hour_bef_temperature","hour_bef_windspeed","hour_bef_humidity",
                               "hour_bef_visibility","hour_bef_precipitation.x",
                               "hour_bef_precipitation.y","hour_bef_ozone","hour_bef_pm10",
                               "hour_bef_pm2.5","count","일시"))
##복사
newtrain3<-newtrain2

##시간 변수 na를 99로 임시 변환 후 정수타입으로 설정
newtrain3[which(is.na(newtrain3$hour.y)),"hour.y"]<-99
newtrain3$hour.y<-as.integer(newtrain3$hour.y)

##일시로 오름차순 정렬
newtrain3<-newtrain3[order(newtrain3$일시),]

##합친 데이터 검토과정
table(newtrain3$date) #4/1 : 23  #4/5 : 20  #4/6 : 23 #  

##table차이 고려해서 추가적으로 아예 비는 na유추해 채우기 
table(newtrain3$hour.x) 
table(newtrain3$hour.y) #4/5 18시 #4/6 0시(유력) 또는 4/1 0시 0시임을 확인

##hour.x는 na가 없지만 실제로 비는 시간대가 존재함 
#-> (4/5 : 16,17,21시)  (4/17 : 16시)
aaaa<-newtrain3%>%filter(hour.x==0)
unique(aaaa$date) #4/6 0시 또는 4/1 0시 : NA행
length(unique(aaaa$date))

aaaa2<-newtrain3%>%filter(hour.x==13)
unique(aaaa2$date) #4/17 13시

aaaa3<-newtrain3%>%filter(hour.x==16)
unique(aaaa3$date) #4/5 16시

aaaa4<-newtrain3%>%filter(hour.x==17)
unique(aaaa4$date) #4/5 17시

aaaa5<-newtrain3%>%filter(hour.x==18)
unique(aaaa5$date) #4/5 18시 : NA행

aaaa6<-newtrain3%>%filter(hour.x==21)
unique(aaaa6$date) #4/5 21시

##강수량 검토
#weather에서 가져올 때, 강수량이 0보다 크면 train에서 1로 표기
#   "         "      ,  강수량 NA는 train에서 0으로 표기 되어있음
k<-newtrain3%>%filter(hour_bef_precipitation.x==1)
unique(k$hour_bef_precipitation.y)
sum(is.na(k$hour_bef_precipitation.y))
dim(k)

k2<-newtrain3%>%filter(hour_bef_precipitation.x==0)
unique(k2$hour_bef_precipitation.y)
sum(is.na(k2$hour_bef_precipitation.y))
dim(k2)

##따라서 weather에서 가져온 강수량 값의 NA 만 0으로 입력하고 0초과 값 활용
vec<-which(is.na(newtrain3$hour_bef_precipitation.y))
length(vec)
vec2<-vec[-c(1374,1375)]
#sum(is.na(newtrain3[vec2,]$일시))
newtrain3[vec,]
newtrain3[vec2,]
newtrain3[vec2,"hour_bef_precipitation.y"]<-0
sum(is.na(newtrain3$hour_bef_precipitation.y)) #애초에 모든 변수들이 NA인 행은 2개

###########NA인 행의 일자 찾기##############
############################################
##0시의 전 후 count로 4월 1일인지 4월 6일인지 유추
newtrain3%>%filter(일시<="2017-04-01 03:00:00 KST")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 39,col='red',size=2,linetype='dashed')+
  xlab("")+
  ggtitle("4월 1일 0시의 따릉이 대여 수")
#x축순서 수정필요
newtrain3%>%filter(일시<="2017-04-06 04:00:00 KST"&일시>="2017-04-05 21:00:00 KST")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 39,col='red',size=2,linetype='dashed')+
  xlab("")+
  ggtitle("4월 6일 0시의 따릉이 대여 수")
#해석:0시인 행은 4월 1일 0시이다. 4월 6일 0시는 (4/5 : 16,17,21시)  (4/17 : 16시)처럼 주어지지 않은 데이터이다.

##18시 전 후 count로 4월 5일의 18시가 맞는지 확인
newtrain3%>%filter(일시<="2017-04-05 21:00:00 KST"&일시>="2017-04-05 15:00:00 KST")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 1,col='red',size=2,linetype='dashed')+
  xlab("")+
  scale_y_continuous(breaks=seq(0, 2, 1))+
  ggtitle("4월 5일 18시의 따릉이 대여 수")
#해석: 16시 17시는 원래 데이터가 주어지지 않은 시간이고, 18시의 count가 1일 가능성은 높아보인다.
#4월 5일 비가 왔기때문에 count가 특히 낮은 것.

##결론 : id 1420은 4/1 0시, id 1553은 4/5 18시의 기상데이터로 대체한다. 
newtrain4<-newtrain3
str(newtrain4)
newtrain4[which(id==1420),"hour.y"] <- 0
newtrain4[which(id==1420),"date"] <- as.Date("2017-04-01")
newtrain4[which(id==1420),"day"] <- "토요일"
newtrain4[which(id==1420),"hour_bef_temperature"] <- 5.3
newtrain4[which(id==1420),"hour_bef_windspeed"] <- 2.2
newtrain4[which(id==1420),"hour_bef_humidity"] <- 79
newtrain4[which(id==1420),"hour_bef_visibility"] <- 2000
newtrain4[which(id==1420),"hour_bef_precipitation.x"] <- 0
newtrain4[which(id==1420),"hour_bef_precipitation.y"] <- 0
newtrain4[which(id==1420),"일시"] <- as.POSIXct("2017-04-01 0:00")
newtrain4[which(id==1420),]

newtrain4[which(id==1553),"hour.y"] <- 18
newtrain4[which(id==1553),"date"] <- as.Date("2017-04-05")
newtrain4[which(id==1553),"day"] <- "수요일"
newtrain4[which(id==1553),"hour_bef_temperature"] <- 10.8
newtrain4[which(id==1553),"hour_bef_windspeed"] <- 4.4
newtrain4[which(id==1553),"hour_bef_humidity"] <- 91
newtrain4[which(id==1553),"hour_bef_visibility"] <- 267
newtrain4[which(id==1553),"hour_bef_precipitation.x"] <- 1
newtrain4[which(id==1553),"hour_bef_precipitation.y"] <- 4.5
newtrain4[which(id==1553),"일시"] <- as.POSIXct("2017-04-05 18:00")
newtrain4[which(id==1553),]

##일시로 한 번 더 오름차순 정렬
newtrain4<-newtrain4[order(newtrain4$일시),]

###################################################################
##############요일 별 특징 파악 후 dummy변수화하기#################
##요일 별 시간에 따른 count 분포
a1<-newtrain4%>%filter(day=="월요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='pink')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("월요일")
a2<-newtrain4%>%filter(day=="화요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='pink')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("화요일")
a3<-newtrain4%>%filter(day=="수요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='pink')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("수요일")
a4<-newtrain4%>%filter(day=="목요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='pink')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("목요일")
a5<-newtrain4%>%filter(day=="금요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='pink')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("금요일")
a6<-newtrain4%>%filter(day=="토요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("토요일")
a7<-newtrain4%>%filter(day=="일요일")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  scale_y_continuous(limits=c(0,2800),breaks = seq(0, 2800, 400))+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  xlab("")+
  ggtitle("일요일")
grid.arrange(a1,a2,a3,a4,a5,a6,a7,ncol=5)
#해석:평일은 출근 시간대(8시) 와 퇴근 시간대(18시)에 대여 수가 많으며 

##요일 별 시간대에 따른 count 박스플랏
##평일
newtrain4%>%filter(day!="토요일" & day!="일요일")%>%
  ggplot()+
  geom_boxplot(aes(x=hour.x,y=count,fill=as.factor(hour.x)))+
  facet_wrap(~day,ncol = 5)+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  theme(legend.position = 'none')

##주말
newtrain4%>%filter(day=="토요일" | day=="일요일")%>%
  ggplot()+
  geom_boxplot(aes(x=hour.x,y=count,fill=as.factor(hour.x)))+
  facet_wrap(~day,ncol = 5)+
  scale_x_continuous(breaks=c(seq(0, 24, 6)))+
  theme(legend.position = 'none')

newtrain4%>%
  ggplot()+
  geom_boxplot(aes(x=hour.x,y=count,fill=as.factor(hour.x)))+
  facet_wrap(~day,ncol = 5)+
  scale_x_continuous(breaks=c(seq(0, 24, 6),8))+
  theme(legend.position = 'none')

##평일이면 0, 주말이면  1인 변수 weekend 생성
newtrain5<-newtrain4
newtrain5<-newtrain5%>%mutate(weekend=ifelse( (day=="토요일"|day=="일요일"),1,0 ))
table(newtrain5$weekend)
table(newtrain5$day)

##변수 쳐내고 ozone,pm2.5,pm10의 NA 처리만을 남긴 데이터 저장
colnames(newtrain5)
head(newtrain5)
finaltest<-newtrain5%>%select(-c("hour.y","일시"))
colnames(finalltrain)[2]<-'hour'
write.csv(finaltest,"newtrain.csv")
###################################################################
###################################################################
###################################################################
##test셋에도 동일처리
test <- data.table(test)
setkeyv(test, c('hour_bef_temperature','hour_bef_windspeed',
                 'hour_bef_humidity','hour_bef_visibility'))
newtest <- merge(test,weather,all.x=T)
colnames(newtest)

newtest2<-newtest%>%select(c("id","hour.x","hour.y", "date","day",
                             "hour_bef_temperature","hour_bef_windspeed","hour_bef_humidity",
                             "hour_bef_visibility","hour_bef_precipitation.x",
                             "hour_bef_precipitation.y","hour_bef_ozone","hour_bef_pm10",
                             "hour_bef_pm2.5","일시"))

newtest2[which(is.na(newtest2$hour.y)),3]<-99
newtest2$hour.y<-as.integer(newtest2$hour.y)
##오름차순
newtest2<-newtest2[order(newtest2$일시),]

##합친 데이터 검토과정
table(newtest2$date) #6/2 : 20  #6/6 : 23  #6/7 : 23  

##table차이 고려해서 추가적으로 아예 비는 na유추해 채우기 
table(newtest2$hour.x) 
table(newtest2$hour.y) 

##hour.x는 na가 없지만 실제로 비는 시간대가 존재함 
#-> (6/2 : 16,17,18,19시)  (6/6 : 21시)  (6/7 : 5시)
bbbb<-newtest2%>%filter(hour.x==16)
unique(bbbb$date) #6/2 16시

bbbb2<-newtest2%>%filter(hour.x==17)
unique(bbbb2$date) #6/2 17시

bbbb3<-newtest2%>%filter(hour.x==18)
unique(bbbb3$date) #6/2 18시

bbbb4<-newtest2%>%filter(hour.x==19)
unique(bbbb4$date) #6/2 19시 : NA행

bbbb5<-newtest2%>%filter(hour.x==21)
unique(bbbb5$date) #6/6 21시

bbbb6<-newtest2%>%filter(hour.x==5)
unique(bbbb6$date) #6/7 5시

##강수량 검토
j<-newtest2%>%filter(hour_bef_precipitation.x==1)
unique(j$hour_bef_precipitation.y)
sum(is.na(j$hour_bef_precipitation.y))
dim(j)

j2<-newtest2%>%filter(hour_bef_precipitation.x==0)
unique(j2$hour_bef_precipitation.y)
sum(is.na(j2$hour_bef_precipitation.y))
dim(j2)

##따라서 weather에서 가져온 강수량 값의 NA 만 0으로 입력하고 0초과 값 활용
vecvec<-which(is.na(newtest2$hour_bef_precipitation.y))
length(vecvec)
vecvec2<-vecvec[-c(635)]
#sum(is.na(newtrain3[vec2,]$일시))
newtest2[vecvec,]
newtest2[vecvec2,]
newtest2[vecvec2,"hour_bef_precipitation.y"]<-0

sum(is.na(newtest2$hour_bef_precipitation.y)) #애초에 모든 변수들이 NA인 행은 1개

###########NA인 행의 일자 찾기##############
############################################
##19시의 전 후 count로 6월 2일의 19시가 맞는지 확인할 수는 없으나 유추 가능.

##결론 : id 1943은 6/2 19시의 기상데이터로 대체한다. 
newtest3<-newtest2
str(newtest3)
newtest3[which(id==1943),"hour.y"] <- 19
newtest3[which(id==1943),"date"] <- as.Date("2017-06-02")
newtest3[which(id==1943),"day"] <- "금요일"
newtest3[which(id==1943),"hour_bef_temperature"] <- 23.5
newtest3[which(id==1943),"hour_bef_windspeed"] <- 4.4
newtest3[which(id==1943),"hour_bef_humidity"] <- 32
newtest3[which(id==1943),"hour_bef_visibility"] <- 2000
newtest3[which(id==1943),"hour_bef_precipitation.x"] <- 0
newtest3[which(id==1943),"hour_bef_precipitation.y"] <- 0
newtest3[which(id==1943),"일시"] <- as.POSIXct("2017-04-01 0:00")
newtest3[which(id==1943),]

##한 번 더 오름차순
newtest3<-newtest3[order(newtest3$일시),]

##평일이면 0, 주말이면  1인 변수 weekend 생성
newtest4<-newtest3
newtest4<-newtest4%>%mutate(weekend=ifelse( (day=="토요일"|day=="일요일"),1,0 ))
table(newtest4$weekend)
table(newtest4$day)

##변수 쳐내고 ozone,pm2.5,pm10의 NA 처리만을 남긴 데이터 저장
colnames(newtest4)
head(newtest4)
finaltest<-newtest4%>%select(-c("hour.y","일시"))
colnames(finaltest)[2]<-'hour'
write.csv(finaltest,"newtest.csv")
