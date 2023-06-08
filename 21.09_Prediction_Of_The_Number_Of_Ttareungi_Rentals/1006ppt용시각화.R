colnames(train)

a1<-train[which(is.na(train$id)),id]
a2<-train[which(is.na(train$hour)),id]
a3<-train[which(is.na(train$hour_bef_temperature)),id]
a4<-train[which(is.na(train$hour_bef_precipitation)),id]
a5<-train[which(is.na(train$hour_bef_windspeed)),id]
a6<-train[which(is.na(train$hour_bef_humidity)),id]
a7<-train[which(is.na(train$hour_bef_visibility)),id]
a8<-train[which(is.na(train$hour_bef_ozone)),id]
a9<-train[which(is.na(train$hour_bef_pm10)),id]
a10<-train[which(is.na(train$hour_bef_pm2.5)),id]
a11<-train[which(is.na(train$count)),id]

aaa<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
length(unique(aaa) )


b1<-test[which(is.na(test$id)),id]
b2<-test[which(is.na(test$hour)),id]
b3<-test[which(is.na(test$hour_bef_temperature)),id]
b4<-test[which(is.na(test$hour_bef_precipitation)),id]
b5<-test[which(is.na(test$hour_bef_windspeed)),id]
b6<-test[which(is.na(test$hour_bef_humidity)),id]
b7<-test[which(is.na(test$hour_bef_visibility)),id]
b8<-test[which(is.na(test$hour_bef_ozone)),id]
b9<-test[which(is.na(test$hour_bef_pm10)),id]
b10<-test[which(is.na(test$hour_bef_pm2.5)),id]
b11<-test[which(is.na(test$count)),id]
bbb<-c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)
length(unique(bbb) )

#train NA 파이차트
rwnum<-c(1328,131)
pct<-round(rwnum/sum(rwnum)*100,1)
lab<-paste(pct,"%")
pie(rwnum,init.angle = 90,col=c("yellow","pink"),label=lab,cex=2)
legend("bottom",c("not NA","NA"),cex = 0.9,fill=c("yellow","pink"))
?pie
#test NA 파이차트
rwnum2<-c(674,41)
pct2<-round(rwnum2/sum(rwnum2)*100,1)
lab2<-paste(pct2,"%")
pie(rwnum2,init.angle = 90,col=c("yellow","red"),label=lab2,cex=2)
legend("bottom",c("not NA","NA"),cex = 0.9,fill=c("yellow","red"))
#
use_count<-c(2,1,0,6,8,3,2)
use_hour<-factor(c(22,23,0,1,2,3,4),levels=c(22,23,0,1,2,3,4))
use<-data.frame(use_hour,use_count)
use%>%ggplot()+
  geom_bar(aes(x=use_hour,y=use_count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 39,col='red',size=2,linetype='dashed')+
  xlab("hour")+
  ylab("count")+
  ggtitle("4월 6일 0시의 따릉이 대여 수")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))
#
use_count<-c(0,34,29,21,6)
use_hour<-c(0,1,2,3,4)
use<-data.frame(use_hour,use_count)
use%>%ggplot()+
  geom_bar(aes(x=use_hour,y=use_count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 39,col='red',size=2,linetype='dashed')+
  xlab("hour")+
  ylab("count")+
  ggtitle("4월 1일 0시의 따릉이 대여 수")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))


table(train$hour_bef_precipitation)
rain<-factor(c(0,1))
num<-c(1411,46)
precipp<-data.frame(rain,num)
precipp%>%ggplot()+
  geom_bar(aes(x=rain,y=num,fill=rain),stat = 'identity')+
  scale_y_continuous(limits=c(0,1500),breaks = seq(0, 1500, 500))+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20))+
  theme(legend.position = "none")

table(newtrain3$hour_bef_precipitation.y)
table(newtrain3$hour_bef_precipitation.x)

newtrain3%>%filter(일시<="2017-04-05 21:00:00 KST"&일시>="2017-04-05 15:00:00 KST")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 1,col='red',size=2,linetype='dashed')+
  xlab("hour")+
  scale_x_continuous(breaks=seq(15, 20, 1))+
  scale_y_continuous(breaks=seq(0, 2, 1))+
  ggtitle("4월 5일 18시의 따릉이 대여 수")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))
########################################################################
supertrain<-read.csv("finaltrain.csv")
sum(is.na(supertrain))
supertrain$hour<-as.factor(supertrain$hour)

#시간에 대한 풍속 분포
supertrain%>%ggplot()+
  geom_boxplot(aes(x=hour,y=hour_bef_windspeed,col=hour))+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))

#온도에 대한 오존 분포
supertrain%>%ggplot()+
  geom_point(aes(x=hour_bef_temperature ,y=hour_bef_ozone ),col="blue")+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))

#시정에 대한 pm2.5 분포
supertrain%>%ggplot()+
  geom_point(aes(x=hour_bef_visibility ,y=hour_bef_pm2.5  ),col="red")+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))

#pm2.5에 대한 pm10 분포
supertrain%>%ggplot()+
  geom_point(aes(x=hour_bef_pm2.5 ,y=hour_bef_pm10  ),col="brown")+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))
##########################
#단일변수
#train2$hour<-as.factor(train2$hour)

#hours
i=2
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] ) )
assign(paste("hist",i,sep="_"),
       train%>%
         ggplot(aes(x=train[,i]))+
         geom_bar(fill = "pink", colour = "black")+
         scale_y_continuous(limits=c(0,65),breaks = seq(0, 65, 10))+
         xlab( colnames(train)[i] ) )
grid.arrange(box_2,hist_2)
hist_2
table(train2$hour)
table(train$hour)

#hour_bef_temperature
i=3
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 1, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_3,hist_3)
#hour_bef_precipitation

#hour_bef_windspeed
i=5
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 1, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_5,hist_5)
hist_5
#hour_bef_humidity
i=6
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 1, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_6,hist_6)
#hour_bef_visibility
i=7
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 1, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_7,hist_7)

nrow(train2) #1358
length(which(train2$hour_bef_visibility==2000)) #305
length(which(train2$hour_bef_visibility<2000)) #1023

train3<-train2%>%filter(hour_bef_visibility<2000)
a1<-train3%>%
  ggplot(aes(x=1,y=train3[,i]))+
  geom_boxplot(fill = "green", colour = "black")+
  ylab( colnames(train3)[i] )
a2<-train3%>%
  ggplot(aes(x=train3[,i],y=..density..))+
  geom_histogram(binwidth = 5, fill = "pink", colour = "black")+
  geom_density(alpha=0.3, fill="yellow")+
  xlab( colnames(train3)[i] )+
  theme(axis.title=element_text(size=20),axis.text.x = element_text(size=30))
grid.arrange(a1,a2)

hist_7
a2
#hour_bef_ozone
i=8
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 0.005, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_8,hist_8)
#hour_bef_pm10
i=9
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 2, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_9,hist_9)
#hour_bef_pm2.5
i=10
assign(paste("box",i,sep="_"),
       train2%>%
         ggplot(aes(x=1,y=train2[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train2)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train2%>%
         ggplot(aes(x=train2[,i],y=..density..))+
         geom_histogram(binwidth = 1, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train2)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_10,hist_10)
#count
i=11
assign(paste("box",i,sep="_"),
       train%>%
         ggplot(aes(x=1,y=train[,i]))+
         geom_boxplot(fill = "green", colour = "black")+
         ylab( colnames(train)[i] )+
         scale_x_continuous(breaks = NULL)+
         xlab("")+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
assign(paste("hist",i,sep="_"),
       train%>%
         ggplot(aes(x=train[,i],y=..density..))+
         geom_histogram(binwidth = 5, fill = "pink", colour = "black")+
         geom_density(alpha=0.3, fill="yellow")+
         xlab( colnames(train)[i] )+
         theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20)))
grid.arrange(box_11,hist_11)

#
#시간에 대한 count 분포1
train2%>%ggplot()+
  geom_boxplot(aes(x=hour,y=count,col=hour))+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))
#시간에 대한 count 분포2
train2%>%ggplot()+
  geom_point(aes(x=hour ,y=count ),col="orange")+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))


train$hour<-as.factor(train$hour)




#시간에 대한 count 분포1
train%>%ggplot()+
  geom_boxplot(aes(x=hour,y=count,col=hour))+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))
#시간에 대한 count 분포2
train%>%ggplot()+
  geom_point(aes(x=hour ,y=count ),col="orange")+
  theme(legend.position = "none")+
  theme(axis.title=element_text(size=15),axis.text.x = element_text(size=20),
        ,axis.text.y = element_text(size=20))
