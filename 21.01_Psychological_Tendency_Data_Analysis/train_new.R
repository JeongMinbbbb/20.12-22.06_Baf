library(tidyverse)
library(corrplot)
library(gridExtra)
library(ggplot2)

setwd("C:/Users/SAMSUNG/Desktop/비어플/팀프로젝트 - 심리 성향 예측/open data")
getwd()
train <- read.csv('train.csv')

table(duplicated(train[,-1]))
str(train)
colnames(train)

train_new <- subset(train, select=-index)
train_new <- train_new[,c(seq(1,40,2),seq(2,40,2),41:77)]
str(train_new)

#### ========================== 범주형 변수 재범주화 ====================== ####
# 나이
# +70s   10s   20s   30s   40s   50s   60s 
# 235 14215 14112  7836  5051  2889  1194 
# 1     2     3     4     5     6     7 
# 14215 14112  7836  5051  2889  1194   235 
str(train_new)
table(train_new$age_group)
# 나이 그래프
# 그림 상으로 70대의 빈도가 적어 60대와 합쳐 60대 이상으로 재범주화

train_new$age_group <- ifelse(train_new$age_group=='10s',1,
                              ifelse(train_new$age_group=='20s',2,
                                     ifelse(train_new$age_group=='30s',3,
                                            ifelse(train_new$age_group=='40s',4,
                                                   ifelse(train_new$age_group=='50s',5,6)))))
table(train_new$age_group)

class(train_new$age_group)

str(train_new)

# 성별
table(train_new$gender)
# Female   Male 
# 21315  24217 
# 1     2 
# 21315 24217 
train_new$gender <- ifelse(train_new$gender=='Female',1,2)

table(train_new$gender)
class(train_new$gender)

# 인종
table(train_new$race)
# Arab                 Asian                 Black Indigenous Australian       Native American                 Other 
# 351                  6834                  2168                    53                   548                  4330 
# White 
# 31248 
# 1     2     3     4     5     6     7 
# 351  6834  2168    53   548 31248  4330
# 인종 그래프
train_new$race <- ifelse(train_new$race=='Arab',1,
                         ifelse(train_new$race=='Asian',2,
                                ifelse(train_new$race=='Black',3,
                                       ifelse(train_new$race=='Indigenous Australian',4,
                                              ifelse(train_new$race=='Native American',5,
                                                     ifelse(train_new$race=='White',6,7))))))
table(train_new$race)

# 종교
table(train_new$religion)
# Agnostic              Atheist             Buddhist   Christian_Catholic     Christian_Mormon      Christian_Other 
# 9624                10192                  850                 6431                  428                 5137 
# Christian_Protestant Hindu               Jewish               Muslim                Other                 Sikh 
# 4875                 1429                  487                 1192                 4770                  117
# 1    2     3     4     5     6     7     8     9     10     11     12
# 9624 10192 850   6431  4875  428   5137  1429  487   1192   117    4770 4
train_new$religion <- ifelse(train_new$religion=='Agnostic',1,
                             ifelse(train_new$religion=='Atheist',2,
                              ifelse(train_new$religion=='Buddhist',3,
                               ifelse(train_new$religion=='Christian_Catholic',4,
                                ifelse(train_new$religion=='Christian_Protestant',5,
                                 ifelse(train_new$religion=='Christian_Mormon',6,
                                  ifelse(train_new$religion=='Christian_Other',7,
                                   ifelse(train_new$religion=='Hindu',8,
                                    ifelse(train_new$religion=='Jewish',9,
                                      ifelse(train_new$religion=='Muslim',10,
                                        ifelse(train_new$religion=='Sikh',11,12)))))))))))
table(train_new$religion)
str(train_new)
#### ========================== age/edu =================================== ####
table(train_new$age_group,train_new$education)
origin <- train_new
tea <- train_new %>% filter((age_group==1)&((education==0)|(education==3)|(education==4)))
train_new <- setdiff(train_new,tea) #차집합

dim(train_new)
dim(tea)
dim(origin)

table(tea$age_group, tea$education)

idxage <- sample(nrow(tea), 0.67*nrow(tea))
tea1 <- tea[-idxage,]
tea2 <- tea[idxage,]
tea1$education <- 1
tea2$education <- 2
table(tea1$education)
table(tea2$education)
tea <- rbind(tea1, tea2)
table(tea$education)
train_new <- rbind(train_new, tea)
dim(train_new)
table(train_new$age_group, train_new$education)


train_new[which(train_new$education==0&train_new$age_group==2),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==3),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==4),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==5),'education']<-3
train_new[which(train_new$education==0&train_new$age_group==6),'education']<-4
table(train_new$age_group, train_new$education)

#### ========================== 무응답 ==================================== ####
str(train_new)

table(train_new$engnat)
table(train_new$hand)
table(train_new$married)
table(train_new$urban)

train_new[which(train_new$engnat==0),'engnat']<-1
train_new[which(train_new$hand==0),'hand']<-1
train_new[which(train_new$married==0),'married']<-1
train_new[which(train_new$urban==0),'urban']<-2

table(train_new$engnat)
table(train_new$hand)
table(train_new$married)
table(train_new$urban)

train_new$voted <- as.factor(train_new$voted)
#### ========================== Q_E ======================================= ####
str(train_new)
train_new$QE_mean <- apply(train_new[,c(21:40)],1,mean)
str(train_new)
train_new <- train_new %>% arrange(QE_mean)
boxplot(train_new$QE_mean)

train_new <- train_new[c(1:45150),]
for(i in 21:40){
  print(fivenum(log(train_new[,i])))
}# -inf -> 0 값 median


which(train_new$QhE==0)
which(train_new$QiE==0)
which(train_new$QkE==0)
which(train_new$QjE==0)
which(train_new$QoE==0)
which(train_new$QpE==0)
which(train_new$QqE==0)

train_new$QhE[29633]<-median(train_new$QhE)
train_new$QiE[29633]<-median(train_new$QiE)
train_new$QkE[18402]<-median(train_new$QkE)
train_new$QkE[40910]<-median(train_new$QkE)
train_new$QjE[26148]<-median(train_new$QjE)
train_new$QoE[33066]<-median(train_new$QoE)
train_new$QpE[9306]<-median(train_new$QpE)
train_new$QqE[9225]<-median(train_new$QqE)

train_new[29633, 21:40]
train_new[18402, 21:40]
train_new[40910, 21:40]
train_new[26148, 21:40]
train_new[33066, 21:40]
train_new[9306,  21:40]
train_new[9225,  21:40]

for(i in 1:20){
  print(fivenum(log(train_new[,i])))
}
summary(train_new[,21:40])
boxplot(train_new$QE_mean)
str(train_new)
train_new[,c(21:40,78)]<-log(train_new[,c(21:40,78)])
boxplot(train_new$QE_mean)
str(train_new)
#### ========================== wr_mean =================================== ####
str(train_new)
train_new$wr_mean <- apply(train_new[65:77],1,mean)

str(train_new)
train_new <- train_new[,-c(65:77)]
str(train_new)
#### ========================== wf_mean =================================== ####
str(train_new)
train_new$wf_mean <- apply(train_new[,c('wf_01','wf_02','wf_03')],1,mean)

train_new <- train_new[,-c(62:64)]
str(train_new)

#### ========================== Q_A ======================================= ####
# positive :  2 3  8 10 13 15 19       / b c h j m o s
# negative :  5 6 11 17 18             / e f k q r
# secret   :  1 4  7  9 12 14 16 20    / a d g i l n p t
# secret_n :  1 4  7  9 14             / a d g i n
# Tactics  :  3 6 15 18 19 7 12 14 20             / c f o r s
# Views    :  2 5  8 10 13 17 1 4 16   / b e h j m q
# Morality : 11 9                       / k
# 

train_new[,c(5,6,11,17,18,1,4,7,9,14)] <- 6-train_new[,c(5,6,11,17,18,1,4,7,9,14)]

corrplot(cor(train_new[c(3,6,15,18,19,2,5,8,10,13,17,11,1,4,7,9,12,14,16,20)]),method='number')

train_new$Mach_score <- apply(train_new[c(1:20)],1,mean)
train_new$Tactic <- apply(train_new[c(3,6,15,18,19,7,12,14,20)],1,mean)
train_new$Views <- apply(train_new[c(2,5,8,10,13,17,1,4,16)],1,mean)
train_new$Morality <- apply(train_new[c(11,9)],1,mean)

str(train_new)

train_new <- train_new[,-c(1:20)]
# 1-2.5   낮은 사람
# 2.5-3.5 중간 사람
# 3.5-5   높은 사람
quantile(1:5,c(1/3,2/3))

str(train_new)
#### ========================== TIPI ====================================== ####
# O C E A N
# positive : 5 3 1 7 9
# negative : 10 8 6 2 4
# 1/6  :E
# 2/7  :A
# 3/8  :C
# 4/9  :N
# 5/10 :O
# 1 2 3 4 5 6 7
# Da          Ag
str(train_new)
train_new[30:39] <- 7-train_new[30:39]

train_new[which(train_new[30]==0),30]<-4
train_new[which(train_new[31]==0),31]<-4
train_new[which(train_new[32]==0),32]<-4
train_new[which(train_new[33]==0),33]<-4
train_new[which(train_new[34]==0),34]<-4
train_new[which(train_new[35]==0),35]<-4
train_new[which(train_new[36]==0),36]<-4
train_new[which(train_new[37]==0),37]<-4
train_new[which(train_new[38]==0),38]<-4
train_new[which(train_new[39]==0),39]<-4

train_new[seq(31,39,2)]<-8-train_new[seq(31,39,2)]

train_new$Op <- (train_new$tp05+train_new$tp10)/2
train_new$Co <- (train_new$tp03+train_new$tp08)/2
train_new$Ex <- (train_new$tp01+train_new$tp06)/2
train_new$Ag <- (train_new$tp02+train_new$tp07)/2
train_new$Ne <- (train_new$tp04+train_new$tp09)/2

# ggplot(train_new, aes(Mach_score, Op, color=voted))+geom_point()
# ggplot(train_new, aes(Mach_score, Co, color=voted))+geom_point()
# ggplot(train_new, aes(Mach_score, Ex, color=voted))+geom_point()
# ggplot(train_new, aes(Mach_score, Ag, color=voted))+geom_point()
# ggplot(train_new, aes(Mach_score, Ne, color=voted))+geom_point()
# 
# # 마키아벨리즘이 높으면 
# # 친화성(Ag, 2번(-) 7번)이 낮고
# # 성실성(Co, 3번 8번(-))가 높다
# str(train_new)
# corrplot(cor(train_new[58:66]),method='number')
# 
# # 동정심이 많고 다정하다(7) 친화성
# # 믿음직스럽고 자기 관리를 잘 한다(3) 성실성
# # 1 2 3 4 5 6 7
# # Da          Ag
# # 비판적이며 논쟁을 좋아한다(2) 친화성
# # 계획적이지 않으며 덤벙댄다(8) 성실성
# # 7 6 5 4 3 2 1
# # Da          Ag
# 
# # 마키아벨리즘이 높을 때
# # 2,7낮아야 함, 3,8 높아야 함
# # 마키아벨리즘이 낮을 때
# # 2,7높아야 함, 3,8 낮아야 함
# 
# # 1    - 2.33  낮은 사람
# # 2.33 - 3.67  중간 사람
# # 3.67 - 5     높은 사람
# 
# train_new[which(train_new$Mach_3=='highM'&train_new$tp02>5),'tp02'] <- 
#   8-train_new[which(train_new$Mach_3=='highM'&train_new$tp02>5),'tp02']
# train_new[which(train_new$Mach_3=='highM'&train_new$tp07>5),'tp07'] <- 
#   8-train_new[which(train_new$Mach_3=='highM'&train_new$tp07>5),'tp07']
# train_new[which(train_new$Mach_3=='highM'&train_new$tp03<3),'tp03'] <- 
#   8-train_new[which(train_new$Mach_3=='highM'&train_new$tp03<3),'tp03']
# train_new[which(train_new$Mach_3=='highM'&train_new$tp08<3),'tp08'] <- 
#   8-train_new[which(train_new$Mach_3=='highM'&train_new$tp08<3),'tp08']
# 
# train_new[which(train_new$Mach_3=='lowM'&train_new$tp02<3),'tp02'] <- 
#   8-train_new[which(train_new$Mach_3=='lowM'&train_new$tp02<3),'tp02']
# train_new[which(train_new$Mach_3=='lowM'&train_new$tp07<3),'tp07'] <- 
#   8-train_new[which(train_new$Mach_3=='lowM'&train_new$tp07<3),'tp07']
# train_new[which(train_new$Mach_3=='lowM'&train_new$tp03>5),'tp03'] <- 
#   8-train_new[which(train_new$Mach_3=='lowM'&train_new$tp03>5),'tp03']
# train_new[which(train_new$Mach_3=='lowM'&train_new$tp08>5),'tp08'] <- 
#   8-train_new[which(train_new$Mach_3=='lowM'&train_new$tp08>5),'tp08']
# 
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp02<3),'tp02']<-3
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp02>5),'tp02']<-5
# 
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp03<3),'tp03']<-3
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp03>5),'tp03']<-5
# 
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp07<3),'tp07']<-3
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp07>5),'tp07']<-5
# 
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp08<3),'tp08']<-3
# train_new[which(train_new$Mach_3=='mediumM'&train_new$tp08>5),'tp08']<-5
# 
# train_new$Co <- (train_new$tp03+train_new$tp08)/2
# train_new$Ag <- (train_new$tp02+train_new$tp07)/2
# 
# ggplot(train_new, aes(Mach_score, Co, color=voted))+geom_point()
# ggplot(train_new, aes(Mach_score, Ag, color=voted))+geom_point()
# 
# str(train_new)
# corrplot(cor(train_new[,c('Mach_score','Co','Ag')]),method='number')

ggplot(train_new, aes(Mach_score, Co, color=voted))+geom_point()+facet_wrap(wf_mean~.)


train_new<- train_new[,-c(30:39)]

str(train_new)

#### ========================== 이상치 제거 =============================== ####
table(train_new$familysize)
train_new <- train_new[which(train_new$familysize<16),]
str(train_new)

# train_new <- train_new[,-c(1:20)]
#### ====================================================================== ####
train_new$voted <- as.factor(train_new$voted)
train_new$education <- as.factor(train_new$education)
train_new$engnat <- as.factor(train_new$engnat)
train_new$gender <- as.factor(train_new$gender)
train_new$hand <- as.factor(train_new$hand)
train_new$married <- as.factor(train_new$married)
train_new$age_group <- as.numeric(train_new$age_group)
train_new$race <- as.numeric(train_new$race)
train_new$religion <- as.numeric(train_new$religion)
train_new$urban <- as.factor(train_new$urban)

str(train_new)
train_new <- train_new[,-c(1:20)]

nrow(train_new)
index <- as.data.frame(c(0:45131))
colnames(index) <- c('index')
str(index)
train_new <- cbind(index, train_new)

str(train_new)
train_new <- subset(train_new, select=-QE_mean)

write.csv(train_new,'train_new.csv')

#### ====================================================================== ####


#### ====================================================================== ####
################################################################################
# 모델링 training set 생성 ####
# 1_1) race - Asian, White, Black, Other 01-16 ####
train_race1 <- train_new
table(train_race1$race)
train_race1[which(train_race1$race==1),'race']<-7
train_race1[which(train_race1$race==4),'race']<-7
train_race1[which(train_race1$race==5),'race']<-7
table(train_race1$race)
train_race1[which(train_race1$race==2),'race']<-1
train_race1[which(train_race1$race==3),'race']<-2
train_race1[which(train_race1$race==6),'race']<-3
train_race1[which(train_race1$race==7),'race']<-4
table(train_race1$race)

train_race1$race <- as.factor(train_race1$race)

train01<-train_race1
train02<-train_race1
train03<-train_race1
train04<-train_race1
train05<-train_race1
train06<-train_race1
train07<-train_race1
train08<-train_race1
train09<-train_race1
train10<-train_race1
train11<-train_race1
train12<-train_race1
train13<-train_race1
train14<-train_race1
train15<-train_race1
train16<-train_race1


# 1_2) race - Asian, White, Other        16-32 ####
train_race2<-train_new
table(train_race2$race)
train_race2[which(train_race2$race==1), 'race']<-7
train_race2[which(train_race2$race==3), 'race']<-7
train_race2[which(train_race2$race==4), 'race']<-7
train_race2[which(train_race2$race==5), 'race']<-7
table(train_race2$race)
train_race2[which(train_race2$race==2), 'race']<-1
train_race2[which(train_race2$race==6), 'race']<-2
train_race2[which(train_race2$race==7), 'race']<-3
table(train_race2$race)

train_race2$race <- as.factor(train_race2$race)

train17<-train_race2
train18<-train_race2
train19<-train_race2
train20<-train_race2
train21<-train_race2
train22<-train_race2
train23<-train_race2
train24<-train_race2
train25<-train_race2
train26<-train_race2
train27<-train_race2
train28<-train_race2
train29<-train_race2
train30<-train_race2
train31<-train_race2
train32<-train_race2

################################################################################
# 2_1) 10대 voted yes 제거 O 1 ####
train_age1 <- train01
table(train_age1$age_group, train_age1$voted)
train_age1[which(train_age1$age_group==1&train_age1$voted==1),'voted']<-2
table(train_age1$age_group, train_age1$voted)

train_age1$age_group <- as.factor(train_age1$age_group)

train01<-train_age1
train02<-train_age1
train03<-train_age1
train04<-train_age1
train05<-train_age1
train06<-train_age1
train07<-train_age1
train08<-train_age1
# 2_2) 10대 voted yes 제거 O 2 ####
train_age2 <- train17
table(train_age2$age_group, train_age2$voted)
train_age2[which(train_age2$age_group==1&train_age2$voted==1),'voted']<-2
table(train_age2$age_group, train_age2$voted)

train_age2$age_group <- as.factor(train_age2$age_group)

train17<-train_age2
train18<-train_age2
train19<-train_age2
train20<-train_age2
train21<-train_age2
train22<-train_age2
train23<-train_age2
train24<-train_age2

# 2_3) 10대 voted yes 제거 X 3 ####
# train09-16
# 2_4) 10대 voted yes 제거 X 4 ####
# train25-32
################################################################################
# 4_1) 크리스찬 병합 O, other 병합 O ####
train01[which(train01$religion==5|train01$religion==6|train01$religion==7),'religion']<-4
train01[which(train01$religion==3|train01$religion==8|train01$religion==9|
                train01$religion==10|train01$religion==11),'religion']<-12
table(train01$religion)
train09[which(train09$religion==5|train09$religion==6|train09$religion==7),'religion']<-4
train09[which(train09$religion==3|train09$religion==8|train09$religion==9|
                train09$religion==10|train09$religion==11),'religion']<-12
table(train09$religion)
train17[which(train17$religion==5|train17$religion==6|train17$religion==7),'religion']<-4
train17[which(train17$religion==3|train17$religion==8|train17$religion==9|
                train17$religion==10|train17$religion==11),'religion']<-12
table(train17$religion)
train25[which(train25$religion==5|train25$religion==6|train25$religion==7),'religion']<-4
train25[which(train25$religion==3|train25$religion==8|train25$religion==9|
                train25$religion==10|train25$religion==11),'religion']<-12
table(train25$religion)
train05[which(train05$religion==5|train05$religion==6|train05$religion==7),'religion']<-4
train05[which(train05$religion==3|train05$religion==8|train05$religion==9|
                train05$religion==10|train05$religion==11),'religion']<-12
table(train05$religion)
train13[which(train13$religion==5|train13$religion==6|train13$religion==7),'religion']<-4
train13[which(train13$religion==3|train13$religion==8|train13$religion==9|
                train13$religion==10|train13$religion==11),'religion']<-12
table(train13$religion)
train21[which(train21$religion==5|train21$religion==6|train21$religion==7),'religion']<-4
train21[which(train21$religion==3|train21$religion==8|train21$religion==9|
                train21$religion==10|train21$religion==11),'religion']<-12
table(train21$religion)
train29[which(train29$religion==5|train29$religion==6|train29$religion==7),'religion']<-4
train29[which(train29$religion==3|train29$religion==8|train29$religion==9|
                train29$religion==10|train29$religion==11),'religion']<-12
table(train29$religion)

# 4_2) 크리스찬 병합 O, other 병합 X ####
train02[which(train02$religion==5|train02$religion==6|train02$religion==7),'religion']<-4
table(train02$religion)
train10[which(train10$religion==5|train10$religion==6|train10$religion==7),'religion']<-4
table(train10$religion)
train18[which(train18$religion==5|train18$religion==6|train18$religion==7),'religion']<-4
table(train18$religion)
train26[which(train26$religion==5|train26$religion==6|train26$religion==7),'religion']<-4
table(train26$religion)
train06[which(train06$religion==5|train06$religion==6|train06$religion==7),'religion']<-4
table(train06$religion)
train14[which(train14$religion==5|train14$religion==6|train14$religion==7),'religion']<-4
table(train14$religion)
train22[which(train22$religion==5|train22$religion==6|train22$religion==7),'religion']<-4
table(train22$religion)
train30[which(train30$religion==5|train30$religion==6|train30$religion==7),'religion']<-4
table(train30$religion)

# 4_3) 크리스찬 병합 X, other 병합 O ####
train03[which(train03$religion==3|train03$religion==8|train03$religion==9|
                train03$religion==10|train03$religion==11),'religion']<-12
table(train03$religion)
train11[which(train11$religion==3|train11$religion==8|train11$religion==9|
                train11$religion==10|train11$religion==11),'religion']<-12
table(train11$religion)
train19[which(train19$religion==3|train19$religion==8|train19$religion==9|
                train19$religion==10|train19$religion==11),'religion']<-12
table(train19$religion)
train27[which(train27$religion==3|train27$religion==8|train27$religion==9|
                train27$religion==10|train27$religion==11),'religion']<-12
table(train27$religion)
train07[which(train07$religion==3|train07$religion==8|train07$religion==9|
                train07$religion==10|train07$religion==11),'religion']<-12
table(train07$religion)
train15[which(train15$religion==3|train15$religion==8|train15$religion==9|
                train15$religion==10|train15$religion==11),'religion']<-12
table(train15$religion)
train23[which(train23$religion==3|train23$religion==8|train23$religion==9|
                train23$religion==10|train23$religion==11),'religion']<-12
table(train23$religion)
train31[which(train31$religion==3|train31$religion==8|train31$religion==9|
                train31$religion==10|train31$religion==11),'religion']<-12
table(train31$religion)

# 4_4) 크리스찬 병합 X, other 병합 X ####
# train 04 08 12 16 20 24 28 32

train01$religion <- as.factor(train01$religion)
train02$religion <- as.factor(train02$religion)
train03$religion <- as.factor(train03$religion)
train04$religion <- as.factor(train04$religion)
train05$religion <- as.factor(train05$religion)
train06$religion <- as.factor(train06$religion)
train07$religion <- as.factor(train07$religion)
train08$religion <- as.factor(train08$religion)
train09$religion <- as.factor(train09$religion)
train10$religion <- as.factor(train10$religion)
train11$religion <- as.factor(train11$religion)
train12$religion <- as.factor(train12$religion)
train13$religion <- as.factor(train13$religion)
train14$religion <- as.factor(train14$religion)
train15$religion <- as.factor(train15$religion)
train16$religion <- as.factor(train16$religion)
train17$religion <- as.factor(train17$religion)
train18$religion <- as.factor(train18$religion)
train19$religion <- as.factor(train19$religion)
train20$religion <- as.factor(train20$religion)
train21$religion <- as.factor(train21$religion)
train22$religion <- as.factor(train22$religion)
train23$religion <- as.factor(train23$religion)
train24$religion <- as.factor(train24$religion)
train25$religion <- as.factor(train25$religion)
train26$religion <- as.factor(train26$religion)
train27$religion <- as.factor(train27$religion)
train28$religion <- as.factor(train28$religion)
train29$religion <- as.factor(train29$religion)
train30$religion <- as.factor(train30$religion)
train31$religion <- as.factor(train31$religion)
train32$religion <- as.factor(train32$religion)


################################################################################

train_new$voted <- as.factor(train_new$voted)
train_new$education <- as.factor(train_new$education)
train_new$engnat <- as.factor(train_new$engnat)
train_new$gender <- as.factor(train_new$gender)
train_new$hand <- as.factor(train_new$hand)
train_new$married <- as.factor(train_new$married)
train_new$age_group <- as.factor(train_new$age_group)
train_new$race <- as.factor(train_new$race)
train_new$religion <- as.factor(train_new$religion)
train_new$urban <- as.factor(train_new$urban)


str(train01)

write.csv(train20, 'train20.csv')
write.csv(train24, 'train24.csv')




