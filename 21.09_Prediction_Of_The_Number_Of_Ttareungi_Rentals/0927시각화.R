##0시의 전 후 count로 4월 1일인지 4월 6일인지 유추
newtrain4%>%filter(일시<="2017-04-01 03:00:00 KST")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 39,col='red',size=2,linetype='dashed')+
  xlab("")+
  ggtitle("4월 1일 0시의 따릉이 대여 수")
#x축순서 수정필요
newtrain4%>%filter(일시<="2017-04-06 04:00:00 KST"&일시>="2017-04-05 21:00:00 KST")%>%
  ggplot()+
  geom_bar(aes(x=hour.x,y=count),stat = 'identity',fill='skyblue')+
  geom_hline(yintercept = 39,col='red',size=2,linetype='dashed')+
  xlab("")+
  ggtitle("4월 6일 0시의 따릉이 대여 수")

##모든 일시에 대한 온도 분포 
newtrain4%>%
  ggplot()+
  geom_bar(aes(x=일시,y=hour_bef_temperature),stat = 'identity')

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




