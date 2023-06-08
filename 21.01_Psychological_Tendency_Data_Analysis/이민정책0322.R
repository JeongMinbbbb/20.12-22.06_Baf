library(dplyr);library(ggplot2);library(lubridate);library(grid);library(gridExtra);library(scales);library(ggpmisc)


###데이터 11개 CSV파일 불러오기#######################
Case<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/Case.csv",header=TRUE,sep=",")
Patientinfo<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/Patientinfo.csv",header=TRUE,sep=",")
Policy<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/Policy.csv",header=TRUE,sep=",")
Region<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/Region.csv",header=TRUE,sep=",")
SearchTrend<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/SearchTrend.csv",header=TRUE,sep=",")
Seoulfloating<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/SeoulFloating.csv",header=TRUE,sep=",")

Time<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/Time.csv",header=TRUE,sep=",")
TimeAge<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/TimeAge.csv",header=TRUE,sep=",")
TimeGender<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/TimeGender.csv",header=TRUE,sep=",")
TimeProvince<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/TimeProvince.csv",header=TRUE,sep=",")
Weather<-read.csv(file="C:/Users/JMBAE/Desktop/3월 프로젝트/raw data/Weather.csv",header=TRUE,sep=",")
########################################################
head(Policy)
table(Policy$type)

immig<-policy %>% filter(type=="Immigration")
head(immig)
str(immig)
table(immig$gov_policy)

immig %>%
  group_by(gov_policy) %>%
  summarize(count = n()) 
### plot1 #####
plot1<-immig%>%
  ggplot(aes(x="",fill=gov_policy))+
  geom_bar(position="fill",width = 0.2)+
  annotate("text",x=1,y=0.45,label = "13",size=6)+
  annotate("text",x=1,y=0.9,label = "1",size=6)+
  annotate("text",x=1,y=0.97,label = "1",size=6)+
  labs(x="",y="누적 비율")+
  scale_fill_discrete(labels=c("입국 후 14일 자가격리","미국발 입국자 검역강화","특별 출입국 제한"))+
  ggtitle('출입국 정책 구성')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(axis.title.y = element_text(hjust=0.5,vjust = 0.5))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(family="serif",size = 20,hjust=0.5,face = "bold"))

#### Time###############################################
head(Time)
str(Time)
table(Time$time)
class(Time$date)
Time$date<-as.Date(Time$date)

### Time변수생성 : 일일 확진자 수 Today_confirmed ###########
Today_confirmed<-c(1:nrow(Time))
for (i in 2:nrow(Time)) {
  Today_confirmed[i]<-Time[i,5]-Time[i-1,5]
}
class(Today_confirmed)


### New_Time 데이터프레임생성 ######################################
New_Time<-Time[,c(1,5)]
New_Time$Today_confirmed<-Today_confirmed
head(New_Time)
### plot2 출입국 정책 전후 확진자 수#####
plot2<-New_Time %>%
  filter( date>=as.Date("2020-01-25") & date<=as.Date("2020-05-10") )%>%
  ggplot()+
  geom_line(aes(x=date, y=confirmed),size=0.8,col="blue")+
  geom_line(aes(x=date, y=Today_confirmed),size=1,col="red")+
  labs(x="날짜(월-일)",y="확진자 수")+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9) +
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-01-25"),as.Date("2020-05-10"),by="2 week") )+
  ggtitle('출입국 정책 전후 확진자 수')+
  annotate("rect", xmin = as.Date("2020-04-20"), xmax = as.Date("2020-05-10"), ymin = 6800, ymax = 8800, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-05-02") , y=8600, label = "특별 출입국 절차차", size=3)+
  annotate("text", x=as.Date("2020-05-02") , y=8200, label = "입국 후 14일 자가격리", size=3)+
  annotate("text", x=as.Date("2020-05-02") , y=7800, label = "미국발 입국자 검역 강화", size=3)+
  annotate("text", x=as.Date("2020-05-02") , y=7400, label = "누적 확진자 수", size=3)+
  annotate("text", x=as.Date("2020-05-02") , y=7000, label = "일일 확진자 수", size=3)+
  
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-24"), y = 8600, yend = 8600, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-24"), y = 8200, yend = 8200, linetype = 2, colour = "green", size=1)+
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-24"), y = 7800, yend = 7800, linetype = 2, colour = "brown", size=1)+
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-24"), y = 7400, yend = 7400, colour = "blue", size=1)+
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-24"), y = 7000, yend = 7000, colour = "red", size=1)+
  
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))

####################################################################################
#### 정책별 확진자 수 ##############################################################
#1:특별 출입국 절차 정책시행 전후 확진자 수
plot3<-New_Time %>% 
  filter( date>=as.Date("2020-01-25") & date<=as.Date("2020-03-26") )%>%
  ggplot(aes(date, Today_confirmed)) +
  geom_line(size=1.2)+
  labs(x = "날짜(월-일)", y = "일일 확진자 수") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,830))+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+ 
  stat_peaks(data=New_Time %>% 
               filter( date>=as.Date("2020-02-16") & date<=as.Date("2020-03-26") ),aes(date, Today_confirmed),colour = "red") + #극대점 빨간점으로 표시
  stat_peaks(data=New_Time %>% 
               filter( date>=as.Date("2020-02-16") & date<=as.Date("2020-03-26") ),aes(date, Today_confirmed),geom = "text", colour = "red", #극대점 날짜("%Y-%m") 표시 
             vjust = -0.5, x.label.fmt = "%m-%d") +
  stat_valleys(data=New_Time %>% 
                 filter( date>=as.Date("2020-02-16") & date<=as.Date("2020-03-26") ),aes(date, Today_confirmed),colour = "blue") +  #극소점 파란점으로 표시
  stat_valleys(data=New_Time %>% 
                 filter( date>=as.Date("2020-02-16") & date<=as.Date("2020-03-26") ),aes(date, Today_confirmed),geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d")+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-01-25"),as.Date("2020-03-26"),by="2 week") )+
  ggtitle('특별 출입국 절차')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))



#2:입국 후 14일 자가격리 정책시행 전후 확진자 수
plot4<-New_Time %>% 
  filter( date>=as.Date("2020-03-22") & date<=as.Date("2020-04-11") )%>%
  ggplot(aes(date, Today_confirmed)) +
  geom_line(size=1.2)+
  labs(x = "날짜(월-일)", y = "일일 확진자 수") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,450))+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9)+ 
  stat_peaks(colour = "red") + #극대점 빨간점으로 표시
  stat_peaks(geom = "text", colour = "red", #극대점 날짜("%Y-%m") 표시 
             vjust = -0.5, x.label.fmt = "%m-%d") +
  stat_valleys(colour = "blue") +  #극소점 파란점으로 표시
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d")+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-03-22"),as.Date("2020-04-11"),by="1 week") )+
  annotate("text", x=as.Date("2020-04-02") , y=350, label = "4월 1일", size=4, color="darkgreen")+
  ggtitle('입국 후 14일 자가격리')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))


#3:미국발 입국자 검역강화 정책시행 전후 확진자 수
plot5<-New_Time %>% 
  filter( date>=as.Date("2020-04-03") & date<=as.Date("2020-04-23") )%>%
  ggplot(aes(date, Today_confirmed)) +
  geom_line(size=1.2)+
  labs(x = "날짜(월-일)", y = "일일 확진자 수") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,450))+
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+ 
  stat_peaks(colour = "red") + #극대점 빨간점으로 표시
  stat_peaks(geom = "text", colour = "red", #극대점 날짜("%Y-%m") 표시 
             vjust = -0.5, x.label.fmt = "%m-%d") +
  stat_valleys(colour = "blue") +  #극소,점 파란점으로 표시
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d")+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-04-03"),as.Date("2020-04-23"),by="1 week") )+
  ggtitle('미국발 입국자 검역강화')+
  annotate("text", x=as.Date("2020-04-14") , y=350, label = "4월 13일", size=4, color="brown")+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))


plot45<-grid.arrange(plot4,plot5,nrow=1)
plot345<-grid.arrange(plot4,plot5,plot6,nrow=1,
                      widths=c(1.5,1,1),top=textGrob("출입국 정책 전후 확진자 수",gp=gpar(fontsize=18,face="bold")))

#########################################################
##### TimeProvince ######################################
str(TimeProvince)
head(TimeProvince)
class(TimeProvince$date)
TimeProvince$date<-as.Date(TimeProvince$date)

table(TimeProvince$time)
table(TimeProvince$province)
# Busan Chungcheongbuk-do Chungcheongnam-do             Daegu           Daejeon        Gangwon-do           Gwangju       Gyeonggi-do  Gyeongsangbuk-do  Gyeongsangnam-do 
#163               163               163               163               163               163               163               163               163               163 
#Incheon           Jeju-do      Jeollabuk-do      Jeollanam-do            Sejong             Seoul             Ulsan 
#163               163               163               163               163               163               163 

TimeProvince<-TimeProvince[order(TimeProvince$province),]
str(TimeProvince)
head(TimeProvince)
### TimeProvince변수생성 : 일일 확진자 수 Today_confirmedP ###########
Today_confirmedP<-c(1:nrow(TimeProvince))
for (i in 2:nrow(TimeProvince)) {
  Today_confirmedP[i]<-TimeProvince[i,4]-TimeProvince[i-1,4]
}
#1~163 164~326 327~489 490~652 653~815 816~978 979~1141 1142~1304 1305~1467 
#1468~1630 1631~1793 1794~1956 1957~2119 2120~2282 2283~2445 2446~2608 2609~2771
Today_confirmedP[1]<-TimeProvince[1,4]
Today_confirmedP[164]<-TimeProvince[164,4]
Today_confirmedP[327]<-TimeProvince[327,4]
Today_confirmedP[490]<-TimeProvince[490,4]
Today_confirmedP[653]<-TimeProvince[653,4]
Today_confirmedP[816]<-TimeProvince[816,4]
Today_confirmedP[979]<-TimeProvince[979,4]
Today_confirmedP[1142]<-TimeProvince[1142,4]
Today_confirmedP[1305]<-TimeProvince[1305,4]
Today_confirmedP[1468]<-TimeProvince[1468,4]
Today_confirmedP[1631]<-TimeProvince[1631,4]
Today_confirmedP[1794]<-TimeProvince[1794,4]
Today_confirmedP[1957]<-TimeProvince[1957,4]
Today_confirmedP[2120]<-TimeProvince[2120,4]
Today_confirmedP[2283]<-TimeProvince[2283,4]
Today_confirmedP[2446]<-TimeProvince[2446,4]
Today_confirmedP[2609]<-TimeProvince[2609,4]
class(Today_confirmedP)

######일일 확진자수 음수 : 누적 확진자수 감소?? #######################
which(Today_confirmedP<0) 
TimeProvince[c(740,741,742,743),] #741행
TimeProvince[c(2161,2162,2163,2164,2165),] #2162 2163행
#TimeProvince$confirmed 누적확진자 수가 줄어들 수 없다고 판단
#다시 줄어든 값으로 이전 행들 값 대체
TimeProvince[741,4]<-39
TimeProvince[c(2162,2163),4]<-4

### New_TimeProvince 데이터프레임생성 ######################################
New_TimeProvince<-TimeProvince[,c(1,3,4)] #date, province, confirmed 선택
New_TimeProvince$Today_confirmedP<-Today_confirmedP
head(New_TimeProvince)

### Patientinfo ##################
str(Patientinfo)
head(Patientinfo)

class(Patientinfo$patient_id)
Patientinfo$patient_id<-as.character(Patientinfo$patient_id)

#해외유입 확진자 데이터프레임 생성 : overseas_PI
table(Patientinfo$infection_case) #"": 919개, "etc":763 
overseas_PI<-Patientinfo %>% filter(infection_case=="overseas inflow")

str(overseas_PI) # 840행
head(overseas_PI)
table(overseas_PI$province)
table(overseas_PI$contact_number)

#overseas_PI$contact_number 이상치 처리
#결측치 처리한 데이터프레임 생성 : overseas_PI2
overseas_PI2<-overseas_PI %>% filter(contact_number!="" & contact_number!='-' )
table(overseas_PI2$contact_number)
str(overseas_PI2) #161행
head(overseas_PI2)

table(overseas_PI2$country)


overseas_PI2$contact_number<-as.numeric(overseas_PI2$contact_number)

#
overseas_groupby<-overseas_PI2 %>%
  group_by(confirmed_date) %>%
  summarize(date_count = n(),
            date_contact_number=sum(contact_number)) %>%
  mutate(contact_mean = date_contact_number/date_count)
overseas_groupby

overseas_groupby$confirmed_date<-as.Date(overseas_groupby$confirmed_date)

plot6<-overseas_groupby %>%
  filter(contact_mean!=450) %>%
  ggplot(aes(confirmed_date,contact_mean))+
  geom_line(size=1.2)+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9) +
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+ 
  stat_peaks(colour = "red",ignore_threshold = 0.05) + 
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%m-%d",ignore_threshold = 0.05,size=3) +
  stat_valleys(colour = "blue",ignore_threshold = 0.999) + 
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d",ignore_threshold = 0.999,size=2.5)+
  labs(x="날짜(월-일)",y="접촉자 수")+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-02-01"),as.Date("2020-06-30"),by="1 month") )+
  ggtitle('해외유입 확진자들의 일자별 1인당 접촉자 평균')+
  annotate("rect", xmin = as.Date("2020-05-28"), xmax = as.Date("2020-06-30"), ymin = 230, ymax = 290, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-06-18") , y=280, label = "특별 출입국 절차", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=260, label = "입국 후 14일 자가격리", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=240, label = "미국발 입국자 검역강화", size=2.7)+
  
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 280, yend = 280, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 260, yend = 260, linetype = 2, colour = "green", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 240, yend = 240, linetype = 2, colour = "brown", size=1)+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))

plot6_1<-overseas_groupby %>%
  ggplot(aes(confirmed_date,contact_mean))+
  geom_line(size=1.2)+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9) +
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+ 
  stat_peaks(colour = "red",ignore_threshold = 0.05) + 
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%m-%d",ignore_threshold = 0.05,size=3) +
  stat_valleys(colour = "blue",ignore_threshold = 0.999) + 
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d",ignore_threshold = 0.999,size=2.5)+
  labs(x="날짜(월-일)",y="접촉자 수")+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-02-01"),as.Date("2020-06-30"),by="1 month") )+
  ggtitle('해외유입 확진자들의 일자별 1인당 접촉자 평균')+
  annotate("rect", xmin = as.Date("2020-05-28"), xmax = as.Date("2020-06-30"), ymin = 230, ymax = 290, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-06-18") , y=280, label = "특별 출입국 절차", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=260, label = "입국 후 14일 자가격리", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=240, label = "미국발 입국자 검역강화", size=2.7)+
  
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 280, yend = 280, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 260, yend = 260, linetype = 2, colour = "green", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 240, yend = 240, linetype = 2, colour = "brown", size=1)+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))

overseas_PI2$confirmed_date<-as.Date(overseas_PI2$confirmed_date)

plot7<-overseas_PI2 %>%
  filter(contact_number!=450) %>%
  ggplot(aes(x=confirmed_date,y=contact_number,group=confirmed_date))+
  geom_boxplot()+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9) +
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-02-01"),as.Date("2020-06-30"),by="1 month") )+
  labs(x="날짜(월-일)",y="접촉자 수")+
  ggtitle('해외유입 확진자들의 일자별 접촉자 수')+
  annotate("rect", xmin = as.Date("2020-05-28"), xmax = as.Date("2020-06-30"), ymin = 230, ymax = 290, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-06-18") , y=280, label = "특별 출입국 절차", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=260, label = "입국 후 14일 자가격리", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=240, label = "미국발 입국자 검역강화", size=2.7)+
  
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 280, yend = 280, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 260, yend = 260, linetype = 2, colour = "green", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 240, yend = 240, linetype = 2, colour = "brown", size=1)+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))

plot7_1<-overseas_PI2 %>%
  ggplot(aes(x=confirmed_date,y=contact_number,group=confirmed_date))+
  geom_boxplot()+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9) +
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-02-01"),as.Date("2020-06-30"),by="1 month") )+
  labs(x="날짜(월-일)",y="접촉자 수")+
  ggtitle('해외유입 확진자들의 일자별 접촉자 수')+
  annotate("rect", xmin = as.Date("2020-05-28"), xmax = as.Date("2020-06-30"), ymin = 230, ymax = 290, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-06-18") , y=280, label = "특별 출입국 절차", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=260, label = "입국 후 14일 자가격리", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=240, label = "미국발 입국자 검역강화", size=2.7)+
  
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 280, yend = 280, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 260, yend = 260, linetype = 2, colour = "green", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 240, yend = 240, linetype = 2, colour = "brown", size=1)+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))

grid.arrange(plot6,plot7)
grid.arrange(plot6_1,plot7_1)

### plot8 해외유입 감염 확진자 수
plot8<-overseas_PI %>%
  ggplot()+
  geom_bar(aes(x=as.Date(confirmed_date),y=..count..))+
  labs(x = "날짜(월-일)", y="확진자 수" ) +
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-01-20"),as.Date("2020-06-30"),by="2 week") )+
  geom_vline(xintercept = as.numeric(New_Time$date[16]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[24]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[50]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[53]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[56]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[57]),color = "orange", linetype = 2, size=0.9)+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "green", linetype = 2, size=0.9) +
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "brown", linetype = 2, size=0.9)+
  
  annotate("rect", xmin = as.Date("2020-05-28"), xmax = as.Date("2020-06-30"), ymin = 40, ymax = 48, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-06-18") , y=46, label = "특별 출입국 절차", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=44, label = "입국 후 14일 자가격리", size=2.7)+
  annotate("text", x=as.Date("2020-06-18") , y=42, label = "미국발 입국자 검역강화", size=2.7)+
  
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 46, yend = 46, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 44, yend = 44, linetype = 2, colour = "green", size=1)+
  annotate("segment", x = as.Date("2020-05-30"), xend = as.Date("2020-06-05"), y = 42, yend = 42, linetype = 2, colour = "brown", size=1)+
  ggtitle('해외유입 감염 확진자 수')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(legend.position = "top")+
  theme(title = element_text(size = 20,face = "bold"))


###########################################################################################
#종교시설 감염 확진자 수
Patientinfo1<-Patientinfo
Patientinfo1[which(Patientinfo1$infection_case=="Shincheonji Church"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="Dongan Church"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="Gangnam Dongin Church"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="Korea Campus Crusade of Christ"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="SMR Newly Planted Churches Group"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="Anyang Gunpo Pastors Group"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="Onchun Church"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="River of Grace Community Church"),"infection_case"]<-"religion"
Patientinfo1[which(Patientinfo1$infection_case=="Geochang Church"),"infection_case"]<-"religion"
str(Patientinfo1)
#infection_case 
k<-as.data.frame(table(Patientinfo1$infection_case))
k<-k[order(-k$Freq),]
k<- k %>% filter(Var1 != "")
k #감염경로 종교시설 기타 제외 4번째 순위로 높음

### plot9 지역별 확진자 수 #############################
plot9<-New_TimeProvince %>%
  ggplot(aes(date, Today_confirmedP, group = province, color = factor(province))) +
  geom_line(size=1)+
  labs(x = "날짜(월)", y = "확진자 수") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,700))+
  facet_wrap(~province, ncol=4)+
  ggtitle('지역별 확진자 수')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))
#대구,경북 합치기
head(New_TimeProvince)
aaa<-New_TimeProvince %>%  filter(province=="Daegu") %>% rename(confirmed1=confirmed,Today_confirmedP1=Today_confirmedP)
bbb<-New_TimeProvince %>%  filter(province=="Gyeongsangbuk-do") %>% rename(confirmed2=confirmed,Today_confirmedP2=Today_confirmedP)
ddd<-merge(aaa,bbb,by="date")
head(ddd)
nrow(aaa);nrow(bbb);nrow(ddd)
ddd2<-ddd %>% mutate(confirmed=confirmed1+confirmed2, Today_confirmedP=Today_confirmedP1+Today_confirmedP2) %>% select(date,confirmed,Today_confirmedP)
head(ddd2)

### plot10 대구,경북 확진자 수 ########################
plot10<-ggplot()+
  geom_line(data=ddd2,aes(x=date, y=Today_confirmedP),size=0.8,col="black")+
  geom_line(data=ddd2 %>%
              filter(date>=as.Date("2020-02-18") & date<=as.Date("2020-03-03")),
            aes(x=date, y=Today_confirmedP),size=0.8,col="red")+
  labs(x="날짜(월-일)",y="확진자 수")+
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-01-20"),as.Date("2020-06-30"),by="2 week") )+
  ggtitle('대구,경북 확진자 수')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))+
  annotate("rect", xmin = as.Date("2020-01-20"), xmax = as.Date("2020-02-10"), ymin = 600, ymax = 680, alpha = 0.9, fill="white") +
  annotate("segment", x = as.Date("2020-01-22"), xend = as.Date("2020-01-28"), y = 640, yend = 640, linetype = 1, colour = "red", size=1.5)+
  annotate("text", x=as.Date("2020-02-04") , y=640, label = "신천지", size=6,color="black")



a<-Patientinfo %>% filter(infection_case=="Shincheonji Church"|
                            infection_case=="Dongan Church"|
                            infection_case=="Gangnam Dongin Church"|
                            infection_case=="Korea Campus Crusade of Christ"|
                            infection_case=="SMR Newly Planted Churches Group"|
                            infection_case=="Anyang Gunpo Pastors Group"|
                            infection_case=="Wangsung Church"|
                            infection_case=="Onchun Church"|
                            infection_case=="River of Grace Community Church"|
                            infection_case=="Geochang Church")

str(a)
head(a)
table(a$infection_case)

### plot11 종교시설 감염 확진자 수###############################
plot11<-a %>%
  ggplot()+
  geom_bar(aes(x=as.Date(confirmed_date),y=..count..,fill=infection_case))+
  scale_fill_discrete(labels=c("안양군포목회단 ","동안 교회",
                               "강남 동진 교회","고창 교회",
                               "한국기독교총연합회","온천 교회",
                               "은혜의 강 교회","신천지",
                               "SMR 새로 심은 교회","왕성교회" ))+
  labs(x = "날짜(월-일)", y="확진자 수" ) +
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-01-20"),as.Date("2020-06-30"),by="2 week") )+
  #검은 세로 선
  annotate("segment", x = as.Date("2020-02-28"), xend = as.Date("2020-02-28"), y = 18, yend = 23, linetype = 1, colour = "black", size=1)+
  annotate("segment", x = as.Date("2020-03-22"), xend = as.Date("2020-03-22"), y = 18, yend = 23, linetype = 1, colour = "black", size=1)+
  annotate("segment", x = as.Date("2020-04-20"), xend = as.Date("2020-04-20"), y = 18, yend = 23, linetype = 1, colour = "black", size=1)+
  annotate("segment", x = as.Date("2020-05-06"), xend = as.Date("2020-05-06"), y = 18, yend = 23, linetype = 1, colour = "black", size=1)+
  
  #가로 선
  annotate("segment", x = as.Date("2020-02-28"), xend = as.Date("2020-03-22"), y = 21, yend = 21, linetype = 1, colour = "red", size=2)+
  annotate("segment", x = as.Date("2020-03-22"), xend = as.Date("2020-04-20"), y = 21, yend = 21, linetype = 1, colour = "blue", size=2)+
  annotate("segment", x = as.Date("2020-04-20"), xend = as.Date("2020-05-06"), y = 21, yend = 21, linetype = 1, colour = "green", size=2)+
  annotate("segment", x = as.Date("2020-05-06"), xend = as.Date("2020-06-30"), y = 21, yend = 21, linetype = 1, colour = "brown", size=2)+
  #텍스트
  annotate("text", x=as.Date("2020-03-09") , y=20, label = "첫 사회적 거리두기", size=4,color="red")+
  annotate("text", x=as.Date("2020-04-06") , y=20, label = "고강도 사회적 거리두기", size=4,color="blue")+
  annotate("text", x=as.Date("2020-04-28") , y=20, label = "완화된 사회적 거리두기", size=4,color="green")+
  annotate("text", x=as.Date("2020-06-01") , y=20, label = "생활 속 거리두기", size=4,color="brown")+
  ggtitle('종교시설 감염 확진자 수')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(legend.position = "top")+
  theme(title = element_text(size = 20,face = "bold"))

#################################################################
### 한계 ##########
str(Policy)
head(Policy)
table(Policy$type)
Policy$start_date<-as.Date(Policy$start_date)

#일자별 정책 개수:정책이 몰린 정도
Policy_count<-Policy %>%
  group_by(start_date) %>%
  summarise(date_policy_count=n()) 

table(Policy$type)

### plot12 정책종류별 시행일 분포
plot12<-Policy %>%
  ggplot(aes(start_date,fill=type))+
  geom_bar(stat = 'count',width=0.8)+
  facet_wrap(~type)+
  scale_fill_discrete(labels=c("행정","경보",
                               "교육","건강",
                               "출입국","사회적 거리두기",
                               "기술","정책변용"))+
  labs(x="시행일",y="계")+
  ggtitle('정책종류별 시행일 분포')+
  theme(legend.title = element_blank())+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(title = element_text(size = 20,face = "bold"))

#plot겹치기
tail(New_Time$date)
tail(Policy$start_date)
Policy$start_date<-as.Date(Policy$start_date)

### plot13 정책 시행분포

plot13<-ggplot()+ geom_line(data=New_Time,aes(x=date, y=Today_confirmed),size=1)+
  geom_bar(data=Policy
           ,aes(x=as.Date(start_date),y=..count..*800 /6,fill=type),stat = 'count',width=0.8,color="black")+
  scale_fill_discrete(labels=c("행정","경보",
                               "교육","건강",
                               "출입국","사회적 거리두기",
                               "기술","정책변용"))+
  labs(x = "날짜(월-일)") +
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  scale_x_date(date_labels = "%m-%d",breaks =seq(as.Date("2020-01-20"),as.Date("2020-06-30"),by="2 week") )+
  scale_y_continuous(name="확진자 수" ,sec.axis = sec_axis(~ . *6/800,name = "정책 수"))+
  ggtitle('정책 시행분포')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(legend.position = "top")+
  theme(title = element_text(size = 20,face = "bold"))

###########################################################################################
