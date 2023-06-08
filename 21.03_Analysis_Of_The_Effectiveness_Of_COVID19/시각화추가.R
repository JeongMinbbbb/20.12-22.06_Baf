
add1<-ggplot() +
  geom_bar(data=overseas_PI%>% 
             filter( confirmed_date>=as.Date("2020-01-21") & confirmed_date<=as.Date("2020-03-30")),
           aes(x=as.Date(confirmed_date),y=..count..*600/50,fill=infection_case),color="black")+
  geom_line(data=New_Time %>% 
              filter( date>=as.Date("2020-01-21") & date<=as.Date("2020-03-30") ),
            aes(date, Today_confirmed),size=1.2)+
  
  labs(x = "날짜(월-일)", y = "일일 확진자 수") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
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
  scale_x_date(date_labels = "%m-%d",breaks =c(seq(as.Date("2020-01-21"),as.Date("2020-03-30"),by="2 week"),as.Date("2020-03-30")) )+
  scale_y_continuous(limit=c(0,900),name="일일 확진자 수" ,sec.axis = sec_axis(~ . *50/600,name = "해외유입 표본 확진자 수"))+
  
  annotate("rect", xmin = as.Date("2020-03-19"), xmax = as.Date("2020-03-30"), ymin = 700, ymax = 810, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-03-26") , y=780, label = "정책 시행일", size=3)+
  annotate("text", x=as.Date("2020-03-26") , y=750, label = "해외유입 표본", size=3)+
  annotate("text", x=as.Date("2020-03-26") , y=720, label = "일일 확진자 수", size=3)+
 
  
  annotate("segment", x = as.Date("2020-03-20"), xend = as.Date("2020-03-22"), y = 780, yend = 780, linetype = 2, colour = "orange", size=1)+
  annotate("segment", x = as.Date("2020-03-20"), xend = as.Date("2020-03-22"), y = 750, yend = 750, linetype = 1, colour = "darkorange2", size=3)+
  annotate("segment", x = as.Date("2020-03-20"), xend = as.Date("2020-03-22"), y = 720, yend = 720, linetype = 1, colour = "black", size=1)+
 
  ggtitle('특별 출입국 절차')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))
add1

add2<-ggplot() +
  geom_bar(data=overseas_PI%>% 
             filter( confirmed_date>=as.Date("2020-03-18") & confirmed_date<=as.Date("2020-04-15")),
           aes(x=as.Date(confirmed_date),y=..count..*600/50,fill=infection_case),color="black")+
  geom_line(data=New_Time %>% 
              filter( date>=as.Date("2020-03-18") & date<=as.Date("2020-04-15") ),
            aes(date, Today_confirmed), size=1.2)+
  labs(x = "날짜(월-일)") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
  geom_vline(xintercept = as.numeric(New_Time$date[73]),color = "darkgreen", linetype = 2, size=0.9)+ 
  
  stat_peaks(data=New_Time %>% 
               filter( date>=as.Date("2020-03-18") & date<=as.Date("2020-04-15") ),
             aes(date, Today_confirmed),
             colour = "red") + 
  stat_peaks(data=New_Time %>% 
               filter( date>=as.Date("2020-03-18") & date<=as.Date("2020-04-15") ),
             aes(date, Today_confirmed),
             geom = "text", colour = "red",  
             vjust = -0.5, x.label.fmt = "%m-%d") +
  stat_valleys(data=New_Time %>% 
                 filter( date>=as.Date("2020-03-18") & date<=as.Date("2020-04-15") ),
               aes(date, Today_confirmed),
               colour = "blue") +  
  stat_valleys(data=New_Time %>% 
                 filter( date>=as.Date("2020-03-18") & date<=as.Date("2020-04-15") ),
               aes(date, Today_confirmed),
               geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d")+
  scale_x_date(date_labels = "%m-%d",breaks =c(seq(as.Date("2020-03-18"),as.Date("2020-04-15"),by="1 week"),as.Date("2020-04-15")) )+
  scale_y_continuous(limit=c(0,600),name="일일 확진자 수" ,sec.axis = sec_axis(~ . *50/600,name = "해외유입 표본 확진자 수"))+
  scale_fill_manual(values = "green")+
  annotate("text", x=as.Date("2020-04-03") , y=400, label = "4월 1일", size=4, color="darkgreen")+
  
  annotate("rect", xmin = as.Date("2020-04-08"), xmax = as.Date("2020-04-15"), ymin = 410, ymax = 510, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-04-12") , y=490, label = "정책 시행일", size=3)+
  annotate("text", x=as.Date("2020-04-12") , y=460, label = "해외유입 표본", size=3)+
  annotate("text", x=as.Date("2020-04-12") , y=430, label = "일일 확진자 수", size=3)+
  
  
  annotate("segment", x = as.Date("2020-04-09"), xend = as.Date("2020-04-10"), y = 490, yend = 490, linetype = 2, colour = "darkgreen", size=1)+
  annotate("segment", x = as.Date("2020-04-09"), xend = as.Date("2020-04-10"), y = 460, yend = 460, linetype = 1, colour = "green", size=3)+
  annotate("segment", x = as.Date("2020-04-09"), xend = as.Date("2020-04-10"), y = 430, yend = 430, linetype = 1, colour = "black", size=1)+
  
  ggtitle('입국 후 14일 자가격리')+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))
add2

add3<-ggplot() +
  geom_bar(data=overseas_PI%>% 
             filter( confirmed_date>=as.Date("2020-03-30") & confirmed_date<=as.Date("2020-04-27")),
           aes(x=as.Date(confirmed_date),y=..count..*600/50,fill=infection_case),color="black")+
  geom_line(data=New_Time %>% 
              filter( date>=as.Date("2020-03-30") & date<=as.Date("2020-04-27") ),
            aes(date, Today_confirmed),size=1.2)+
  labs(x = "날짜(월-일)") +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  theme(legend.position = "none")+
  geom_vline(xintercept = as.numeric(New_Time$date[85]),color = "tan4", linetype = 2, size=0.9)+ 
  
  stat_peaks(data=New_Time %>% 
               filter( date>=as.Date("2020-03-30") & date<=as.Date("2020-04-27") ),
             aes(date, Today_confirmed),
             colour = "red") + 
  stat_peaks(data=New_Time %>% 
               filter( date>=as.Date("2020-03-30") & date<=as.Date("2020-04-27") ),
             aes(date, Today_confirmed),
             geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%m-%d") +
  stat_valleys(data=New_Time %>% 
                 filter( date>=as.Date("2020-03-30") & date<=as.Date("2020-04-27") ),
               aes(date, Today_confirmed),
               colour = "blue") + 
  stat_valleys(data=New_Time %>% 
                 filter( date>=as.Date("2020-03-30") & date<=as.Date("2020-04-27") ),
               aes(date, Today_confirmed),
               geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%m-%d")+
  
  scale_x_date(date_labels = "%m-%d",breaks =c(seq(as.Date("2020-03-30"),as.Date("2020-04-27"),by="1 week"),as.Date("2020-04-27")) )+
  scale_y_continuous(limit=c(0,600),name="일일 확진자 수" ,sec.axis = sec_axis(~ . *50/600,name = "해외유입 표본 확진자 수"))+
  scale_fill_manual(values = "tan4")+
  
  annotate("rect", xmin = as.Date("2020-04-20"), xmax = as.Date("2020-04-27"), ymin = 460, ymax = 560, alpha = 0.9, fill="white") +
  annotate("text", x=as.Date("2020-04-24") , y=540, label = "정책 시행일", size=3)+
  annotate("text", x=as.Date("2020-04-24") , y=510, label = "해외유입 표본", size=3)+
  annotate("text", x=as.Date("2020-04-24") , y=480, label = "일일 확진자 수", size=3)+
  
  
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-22"), y = 540, yend = 540, linetype = 2, colour = "tan4", size=1)+
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-22"), y = 510, yend = 510, linetype = 1, colour = "tan4", size=3)+
  annotate("segment", x = as.Date("2020-04-21"), xend = as.Date("2020-04-22"), y = 480, yend = 480, linetype = 1, colour = "black", size=1)+
  
  ggtitle('미국발 입국자 검역강화')+
  annotate("text", x=as.Date("2020-04-14") , y=350, label = "4월 13일", size=4, color="tan4")+
  theme(axis.title = element_text(face="bold",size=17))+
  theme(plot.title = element_text(size = 20,face = "bold"))
add3

grid.arrange(add2,add3,nrow=1)
grid.arrange(plot6,plot7,nrow=1)
grid.arrange(plot6_1,plot7_1,nrow=1)
grid.arrange(plot9,plot10,nrow=1)

k2<-k[c(1:5),]
k2

k2 %>%
  ggplot()+
  geom_bar(aes(x=Var1,y=Freq,fill=Var1),stat="identity")

grid.arrange(plot9,plot10,heights=c(1.8,1))

