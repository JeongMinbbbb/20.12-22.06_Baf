library(corrplot) ; library(gridExtra) ; library(tidyverse)

 read.csv('train.csv')

str(train)

colnames(train)

train_ntable(train$age_group)
ew <- subset(train, select=-index)
str(train_new)

#################################################################
# ??Å°?Æº???Á¡??????####
# positive : b, c, h, j, m, o, s
# negative : e, f, k, q, r
# secret   : a, d, g, i, l, n, p, t
Q_A <- c()
for(i in 1:20){
  Q_A[i] <- paste0("Q",letters[i],"A")
}
Q_A
train_Q <- train[Q_A]
cor_trainQ <- cor(train_Q)
corrplot(cor_trainQ, method="color")

Q_negative <- c("QeA","QfA","QkA","QqA","QrA")
head(train_Q)
for(i in 1:5){
  train_Q[Q_negative[i]] <- 6-train_Q[Q_negative[i]]
}
cor_trainQ <- cor(train_Q)
corrplot(cor_trainQ, method="color")

Q_s_n <- c("QaA","QdA","QgA","QiA","QnA")
for(i in 1:5){
  train_Q[Q_s_n[i]] <- 6-train_Q[Q_s_n[i]]
}
cor_trainQ <- cor(train_Q)
corrplot(cor_trainQ, method="color")
train_Q$M_mean <- apply(train_Q,1,mean)
head(train_Q)
hist(train_Q$M_mean)
train_new$Mach_mean <- train_Q$M_mean
str(train_new)

for(i in 1:5){
  train_new[Q_negative[i]]<-6-train_new[Q_negative[i]]
  train_new[Q_s_n[i]]<-6-train_new[Q_s_n[i]]
}

#################################################################
# TIPI Á¡?? ????####

train_tp <- train[,51:60]
head(train_tp)
# O C E A N
# positive : 5 3 1 7 9
# negative : 10 8 6 2 4

train_tp$tp10 <- 8-train_tp$tp10
train_tp$tp08 <- 8-train_tp$tp08
train_tp$tp06 <- 8-train_tp$tp06
train_tp$tp02 <- 8-train_tp$tp02
train_tp$tp04 <- 8-train_tp$tp04

train_tp$O <- (train_tp$tp05+train_tp$tp10)/2
train_tp$C <- (train_tp$tp03+train_tp$tp08)/2
train_tp$E <- (train_tp$tp01+train_tp$tp06)/2
train_tp$A <- (train_tp$tp07+train_tp$tp02)/2
train_tp$N <- (train_tp$tp09+train_tp$tp04)/2

train_new$O <- train_tp$O
train_new$C <- train_tp$C
train_new$E <- train_tp$E
train_new$A <- train_tp$A
train_new$N <- train_tp$N

str(train_new)


#################################################################
# Q_A ??????####
q1 <- ggplot(data=train_new, aes(x=QaA, y=..count..,fill=as.factor(train_new[,1])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qa')+
  theme_bw()+ggtitle("q1_reverse")+theme(plot.title=element_text(hjust=0.5));q1
# ggsave("001_1.png")
q2 <- ggplot(data=train_new, aes(x=QbA, y=..count..,fill=as.factor(train_new[,3])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qb')+
  theme_bw()+ggtitle("q2")+theme(plot.title=element_text(hjust=0.5));q2
# ggsave("002.png")
q3 <- ggplot(data=train_new, aes(x=QcA, y=..count..,fill=as.factor(train_new[,5])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qc')+
  theme_bw()+ggtitle("q3")+theme(plot.title=element_text(hjust=0.5));q3
# ggsave("003.png")
q4 <- ggplot(data=train_new, aes(x=QdA, y=..count..,fill=as.factor(train_new[,7])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qd')+
  theme_bw()+ggtitle("q4_reverse")+theme(plot.title=element_text(hjust=0.5));q4
# ggsave("004_1.png")
q5 <- ggplot(data=train_new, aes(x=QeA, y=..count..,fill=as.factor(train_new[,9])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qe')+
  theme_bw()+ggtitle("q5_reverse")+theme(plot.title=element_text(hjust=0.5));q5
# ggsave("005_1.png")
q6 <- ggplot(data=train_new, aes(x=QfA, y=..count..,fill=as.factor(train_new[,11])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qf')+
  theme_bw()+ggtitle("q6_reverse")+theme(plot.title=element_text(hjust=0.5));q6
# ggsave("006_1.png")
q7 <- ggplot(data=train_new, aes(x=QgA, y=..count..,fill=as.factor(train_new[,13])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qg')+
  theme_bw()+ggtitle("q7_reverse")+theme(plot.title=element_text(hjust=0.5));q7
# ggsave("007_1.png")
q8 <- ggplot(data=train_new, aes(x=QhA, y=..count..,fill=as.factor(train_new[,15])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qh')+
  theme_bw()+ggtitle("q8")+theme(plot.title=element_text(hjust=0.5));q8
# ggsave("008.png")
q9 <- ggplot(data=train_new, aes(x=QiA, y=..count..,fill=as.factor(train_new[,17])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qi')+
  theme_bw()+ggtitle("q9_reverse")+theme(plot.title=element_text(hjust=0.5));q9
# ggsave("009_1.png")
q10 <- ggplot(data=train_new, aes(x=QjA, y=..count..,fill=as.factor(train_new[,19])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qj')+
  theme_bw()+ggtitle("q10")+theme(plot.title=element_text(hjust=0.5));q10
# ggsave("010.png")
q11 <- ggplot(data=train_new, aes(x=QkA, y=..count..,fill=as.factor(train_new[,21])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qk')+
  theme_bw()+ggtitle("q11_reverse")+theme(plot.title=element_text(hjust=0.5));q11
# ggsave("011_1.png")
q12 <- ggplot(data=train_new, aes(x=QlA, y=..count..,fill=as.factor(train_new[,23])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Ql')+
  theme_bw()+ggtitle("q12")+theme(plot.title=element_text(hjust=0.5));q12
# ggsave("012.png")
q13 <- ggplot(data=train_new, aes(x=QmA, y=..count..,fill=as.factor(train_new[,25])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qm')+
  theme_bw()+ggtitle("q13")+theme(plot.title=element_text(hjust=0.5));q13
# ggsave("013.png")
q14 <- ggplot(data=train_new, aes(x=QnA, y=..count..,fill=as.factor(train_new[,27])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qn')+
  theme_bw()+ggtitle("q14_reverse")+theme(plot.title=element_text(hjust=0.5));q14
# ggsave("014_1.png")
q15 <- ggplot(data=train_new, aes(x=QoA, y=..count..,fill=as.factor(train_new[,29])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qo')+
  theme_bw()+ggtitle("q15")+theme(plot.title=element_text(hjust=0.5));q15
# ggsave("015.png")
q16 <- ggplot(data=train_new, aes(x=QpA, y=..count..,fill=as.factor(train_new[,31])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qp')+
  theme_bw()+ggtitle("q16")+theme(plot.title=element_text(hjust=0.5));q16
# ggsave("016.png")
q17 <- ggplot(data=train_new, aes(x=QqA, y=..count..,fill=as.factor(train_new[,33])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qq')+
  theme_bw()+ggtitle("q17_reverse")+theme(plot.title=element_text(hjust=0.5));q17
# ggsave("017_1.png")
q18 <- ggplot(data=train_new, aes(x=QrA, y=..count..,fill=as.factor(train_new[,35])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qr')+
  theme_bw()+ggtitle("q18_reverse")+theme(plot.title=element_text(hjust=0.5));q18
# ggsave("018_1.png")
q19 <- ggplot(data=train_new, aes(x=QsA, y=..count..,fill=as.factor(train_new[,37])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qs')+
  theme_bw()+ggtitle("q19")+theme(plot.title=element_text(hjust=0.5));q19
# ggsave("019.png")
q20 <- ggplot(data=train_new, aes(x=QtA, y=..count..,fill=as.factor(train_new[,39])))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+labs(fill='Qt')+
  theme_bw()+ggtitle("q20")+theme(plot.title=element_text(hjust=0.5));q20
# ggsave("020.png")

#################################################################
# ?????? ???? integerÈ­####

# ????
# +70s   10s   20s   30s   40s   50s   60s 
# 235 14215 14112  7836  5051  2889  1194 
# 1     2     3     4     5     6     7 
# 14215 14112  7836  5051  2889  1194   235 
table(train_new$age_group)
for(i in 1:nrow(train_new)){
  if(train_new$age_group[i]=='10s'){
    train_new$age_group[i] <- 1}
  else if(train_new$age_group[i]=='20s'){
    train_new$age_group[i] <- 2}
  else if(train_new$age_group[i]=='30s'){
    train_new$age_group[i] <- 3}
  else if(train_new$age_group[i]=='40s'){
    train_new$age_group[i] <- 4}
  else if(train_new$age_group[i]=='50s'){
    train_new$age_group[i] <-5}
  else if(train_new$age_group[i]=='60s'){
    train_new$age_group[i] <- 6}
  else if(train_new$age_group[i]=='+70s'){
    train_new$age_group[i] <- 7}
}
train_new$age_group <- as.integer(train_new$age_group)

# ????
table(train_new$gender)
# Female   Male 
# 21315  24217 
# 1     2 
# 21315 24217 
for(i in 1:nrow(train_new)){
  if(train_new$gender[i]=='Female'){
    train_new$gender[i] <- 1}
  else if(train_new$gender[i]=='Male'){
    train_new$gender[i] <- 2}
}
train_new$gender <- as.integer(train_new$gender)
# ??Á¾
table(train_new$race)
# Arab                 Asian                 Black Indigenous Australian       Native American                 Other 
# 351                  6834                  2168                    53                   548                  4330 
# White 
# 31248 
# 1     2     3     4     5     6     7 
# 351  6834  2168    53   548 31248  4330
for(i in 1:nrow(train_new)){
  if(train_new$race[i]=='Arab'){
    train_new$race[i]<-1}
  else if(train_new$race[i]=='Asian'){
    train_new$race[i]<-2}
  else if(train_new$race[i]=='Black'){
    train_new$race[i]<-3}
  else if(train_new$race[i]=='Indigenous Australian'){
    train_new$race[i]<-4}
  else if(train_new$race[i]=='Native American'){
    train_new$race[i]<-5}
  else if(train_new$race[i]=='White'){
    train_new$race[i]<-6}
  else if(train_new$race[i]=='Other'){
    train_new$race[i]<-7}
}
train_new$race <- as.integer(train_new$race)

# Á¾??
table(train_new$religion)
# Agnostic              Atheist             Buddhist   Christian_Catholic     Christian_Mormon      Christian_Other 
# 9624                10192                  850                 6431                  428                 5137 
# Christian_Protestant Hindu               Jewish               Muslim                Other                 Sikh 
# 4875                 1429                  487                 1192                 4770                  117
# 1    2     3     4     5     6     7     8     9     10     11     12
# 9624 10192 850   6431  4875  428   5137  1429  487   1192   117    4770 4
for(i in 1:nrow(train_new)){
  if(train_new$religion[i]=='Agnostic'){
    train_new$religion[i]<-1}
  else if(train_new$religion[i]=='Atheist'){
    train_new$religion[i]<-2}
  else if(train_new$religion[i]=='Buddhist'){
    train_new$religion[i]<-3}
  else if(train_new$religion[i]=='Christian_Catholic'){
    train_new$religion[i]<-4}
  else if(train_new$religion[i]=='Christian_Protestant'){
    train_new$religion[i]<-5}
  else if(train_new$religion[i]=='Christian_Mormon'){
    train_new$religion[i]<-6}
  else if(train_new$religion[i]=='Christian_Other'){
    train_new$religion[i]<-7}
  else if(train_new$religion[i]=='Hindu'){
    train_new$religion[i]<-8}
  else if(train_new$religion[i]=='Jewish'){
    train_new$religion[i]<-9}
  else if(train_new$religion[i]=='Muslim'){
    train_new$religion[i]<-10}
  else if(train_new$religion[i]=='Sikh'){
    train_new$religion[i]<-11}
  else if(train_new$religion[i]=='Other'){
    train_new$religion[i]<-12}
}
train_new$religion <- as.integer(train_new$religion)

str(train_new)
#################################################################
# ?????? ???? barplot####
# age_group, education, engnat, familysize, gender, hand, married, race, religion, urban

# ???? ????
ggplot(data=train_new, aes(x=age_group,y=..count..,fill=age_group))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ??À° ???? ????
ggplot(data=train_new, aes(x=education,y=..count..,fill=as.factor(education)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

ggplot(data=train_new, aes(x=age_group,))

# ???? ?ð±¹¾?
ggplot(data=train_new, aes(x=engnat,y=..count..,fill=as.factor(engnat)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ??Á· ??À§
ggplot(data=train_new, aes(x=familysize,y=..count..,fill=familysize))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ????
ggplot(data=train_new, aes(x=gender,y=..count..,fill=gender))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ?Ê±??Ï´? ??
ggplot(data=train_new, aes(x=hand,y=..count..,fill=hand))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ??È¥ ????
ggplot(data=train_new, aes(x=married,y=..count..,fill=married))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ??Á¾ 
ggplot(data=train_new, aes(x=race,y=..count..,fill=race))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# Á¾??
ggplot(data=train_new, aes(x=religion,y=..count..,fill=religion))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

# ??????(À¯????)
ggplot(data=train_new, aes(x=urban,y=..count..,fill=urban))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)

#################################################################
# ?? ?????? ???????? ????####

# age_group
ggplot(data=train_new, aes(x=age_group,y=..count..,fill=as.factor(age_group)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("age_group frequency")+labs(fill="age_group")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("021.png")

# education
ggplot(data=train_new, aes(x=education,y=..count..,fill=as.factor(education)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("education frequency")+labs(fill="education")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("022.png")

# engnat
ggplot(data=train_new, aes(x=engnat,y=..count..,fill=as.factor(engnat)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("engnat frequency")+labs(fill="engnat")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("023.png")

# gender
ggplot(data=train_new, aes(x=gender,y=..count..,fill=as.factor(gender)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("gender frequency")+labs(fill="gender")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("024.png")

# hand
ggplot(data=train_new, aes(x=hand,y=..count..,fill=as.factor(hand)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("hand frequency")+labs(fill="hand")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("025.png")

# married
ggplot(data=train_new, aes(x=married,y=..count..,fill=as.factor(married)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("married frequency")+labs(fill="married")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("026.png")

# race
ggplot(data=train_new, aes(x=race,y=..count..,fill=as.factor(race)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("race frequency")+labs(fill="race")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("027.png")

# religion
ggplot(data=train_new, aes(x=religion,y=..count..,fill=as.factor(religion)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("religion frequency")+labs(fill="religion")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("028.png")

# urban
ggplot(data=train_new, aes(x=urban,y=..count..,fill=as.factor(urban)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("urban frequency")+labs(fill="urban")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("029.png")

# voted
ggplot(data=train_new, aes(x=voted,y=..count..,fill=as.factor(voted)))+
  geom_bar()+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("voted frequency")+labs(fill="voted")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("030.png")

#################################################################
# ?? ?????? ???????? MACH ????####

# age_group
ggplot(data=train_new, aes(x=age_group,y=Mach_mean, fill=as.factor(age_group)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by age_group")+labs(fill="age_group")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("031.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(age_group)))+
  geom_histogram()+facet_wrap(age_group~.)+
  ggtitle("Mach mean score histogram by age_group")+labs(fill="age_group")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("032.png")

# education
ggplot(data=train_new, aes(x=education,y=Mach_mean, fill=as.factor(education)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by education")+labs(fill="education")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("033.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(education)))+
  geom_histogram()+facet_wrap(education~.)+
  ggtitle("Mach mean score histogram by education")+labs(fill="education")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("034.png")

# engnat
ggplot(data=train_new, aes(x=engnat,y=Mach_mean, fill=as.factor(engnat)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by engnat")+labs(fill="engnat")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("035.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(engnat)))+
  geom_histogram()+facet_wrap(engnat~.)+
  ggtitle("Mach mean score histogram by engnat")+labs(fill="engnat")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("036.png")

# gender
ggplot(data=train_new, aes(x=gender,y=Mach_mean, fill=as.factor(gender)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by gender")+labs(fill="gender")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("037.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(gender)))+
  geom_histogram()+facet_wrap(gender~.)+
  ggtitle("Mach mean score histogram by gender")+labs(fill="gender")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("038.png")

# hand
ggplot(data=train_new, aes(x=hand,y=Mach_mean, fill=as.factor(hand)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by hand")+labs(fill="hand")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("039.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(hand)))+
  geom_histogram()+facet_wrap(hand~.)+
  ggtitle("Mach mean score histogram by hand")+labs(fill="hand")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("040.png")

# married
ggplot(data=train_new, aes(x=married,y=Mach_mean, fill=as.factor(married)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by married")+labs(fill="married")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("041.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(married)))+
  geom_histogram()+facet_wrap(married~.)+
  ggtitle("Mach mean score histogram by married")+labs(fill="married")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("042.png")

# race
ggplot(data=train_new, aes(x=race,y=Mach_mean, fill=as.factor(race)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by race")+labs(fill="race")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("043.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(race)))+
  geom_histogram()+facet_wrap(race~.)+
  ggtitle("Mach mean score histogram by race")+labs(fill="race")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("044.png")

# religion
ggplot(data=train_new, aes(x=religion,y=Mach_mean, fill=as.factor(religion)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by religion")+labs(fill="religion")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("045.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(religion)))+
  geom_histogram()+facet_wrap(religion~.)+
  ggtitle("Mach mean score histogram by religion")+labs(fill="religion")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("046.png")

# urban
ggplot(data=train_new, aes(x=urban,y=Mach_mean, fill=as.factor(urban)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by urban")+labs(fill="urban")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("047.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(urban)))+
  geom_histogram()+facet_wrap(urban~.)+
  ggtitle("Mach mean score histogram by urban")+labs(fill="urban")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("048.png")

# voted
ggplot(data=train_new, aes(x=voted,y=Mach_mean, fill=as.factor(voted)))+
  geom_boxplot()+ggtitle("Mach mean score's boxplot by voted")+labs(fill="voted")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("049.png")

ggplot(data=train_new, aes(x=Mach_mean, fill=as.factor(voted)))+
  geom_histogram()+facet_wrap(voted~.)+
  ggtitle("Mach mean score histogram by voted")+labs(fill="voted")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("050.png")

#################################################################
# ?? ?????? ???????? ??Ç¥ ???? ????####

# age_group
ggplot(data=train_new, aes(x=age_group,y=..count.., fill=as.factor(age_group)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("age_group vote frequency")+labs(fill="age_group")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("051.png")

# education
ggplot(data=train_new, aes(x=education,y=..count.., fill=as.factor(education)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("education vote frequency")+labs(fill="education")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("052.png")

# engnat
ggplot(data=train_new, aes(x=engnat,y=..count.., fill=as.factor(engnat)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("engnat vote frequency")+labs(fill="engnat")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("053.png")

# gender
ggplot(data=train_new, aes(x=gender,y=..count.., fill=as.factor(gender)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("gender vote frequency")+labs(fill="gender")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("054.png")

# hand
ggplot(data=train_new, aes(x=hand,y=..count.., fill=as.factor(hand)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("hand vote frequency")+labs(fill="hand")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("055.png")

# married
ggplot(data=train_new, aes(x=married,y=..count.., fill=as.factor(married)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("married vote frequency")+labs(fill="married")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("056.png")

# race
ggplot(data=train_new, aes(x=race,y=..count.., fill=as.factor(race)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("race vote frequency")+labs(fill="race")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("057.png")

# religion
ggplot(data=train_new, aes(x=religion,y=..count.., fill=as.factor(religion)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("religion vote frequency")+labs(fill="religion")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("058.png")

# urban
ggplot(data=train_new, aes(x=urban,y=..count.., fill=as.factor(urban)))+
  geom_bar()+facet_wrap(voted~.)+
  geom_text(stat="count", aes(label=..count..),vjust=-0.5)+
  ggtitle("urban vote frequency")+labs(fill="urban")+
  theme_bw()+theme(plot.title=element_text(hjust=0.5))
# ggsave("059.png")

#################################################################
# ?????????? - Mach ?????? ??????####

train_factor_mean <- train_new[,c('age_group','education','engnat','familysize','gender','hand','married','race','religion',
                                  'urban','Mach_mean','voted')]
cor_factor_mean <- cor(train_factor_mean)
cor_factor_mean
corrplot(cor_factor_mean,method='number')

# married & age_group, age_group & education


#################################################################
# mach - tipi ??????####
str(train_Q)
str(train_tp)
train_q_tp <- cbind(train_Q, train_tp)
str(train_q_tp)

cor_train_qtp <-cor(train_q_tp)
corrplot(cor_train_qtp, method='color')


#################################################################
# all column ??????####
cor_train <- cor(train_new)
corrplot(cor_train, method='number')
#################################################################
# ?? ???Ì±?####
#tp È¦??,Â¦?? df
tp_odd<-data.frame(train_new$tp01,train_new$tp03,train_new$tp05,
                   train_new$tp07,train_new$tp09)
tp_even<-data.frame(8-train_new$tp02,8-train_new$tp04,8-train_new$tp06,
                    8-train_new$tp08,8-train_new$tp10)
#tp_p_mean(È¦??????)
tp_p_mean<-apply(tp_odd,1,mean)

#tp_n_mean(Â¦??????)
tp_n_mean<-apply(tp_even,1,mean)

#tp_p_var(È¦???Ð»?)
tp_p_var<-apply(tp_odd,1,var)

#tp_n_var(Â¦???Ð»?)
tp_n_var<-apply(tp_even,1,var)

#wr_p(??Á¤???? ??Á¸?Ü¾?)
wr_p<-data.frame(train_new$wr_01,train_new$wr_05,train_new$wr_07,train_new$wr_08,
                 train_new$wr_09,train_new$wr_11,train_new$wr_13)
#wr_n(??Á¤???? ??Á¸?Ü¾?)
wr_n<-data.frame(train_new$wr_02,train_new$wr_03,train_new$wr_04,train_new$wr_06,
                 train_new$wr_10,train_new$wr_12)
#wr(??Á¸?Ü¾?)
wr<-cbind(wr_p,wr_n)

#wr_p_mean,wr_n_mean,wr_mean
wr_p_mean<-apply(wr_p,1,mean)
wr_n_mean<-apply(wr_n,1,mean)
wr_mean<-apply(wr,1,mean)

#wr_p_var,wr_n_var,wr_var
wr_p_var<-apply(wr_p,1,var)
wr_n_var<-apply(wr_n,1,var)
wr_var<-apply(wr,1,var)

#wf_mean,wf_var
wf<-data.frame(train_new$wf_01,train_new$wf_02,train_new$wf_03)
wf_mean<-apply(wf,1,mean)
wf_var<-apply(wf,1,var)

#Mach_var
head(train_Q)
str(train_Q)
train_Q<-train_Q[-21]
Mach_var<-apply(train_Q,1,var)
head(Mach_var)



#
train_proc<-data.frame(train_new$Mach_mean,Mach_var,train_new$O,train_new$C,train_new$E,
                       train_new$A,train_new$N,tp_p_mean,tp_n_mean,tp_p_var,tp_n_var,
                       wr_p_mean,wr_n_mean,wr_mean,wr_p_var,wr_n_var,wr_var,
                       wf_mean,wf_var,train_new$age_group,train_new$education,
                       train_new$engnat,train_new$gender,train_new$hand,train_new$married,
                       train_new$race,train_new$religion,train_new$familysize,train_new$urban,train_new$voted)

head(train_proc)
str(train_proc)
colnames(train_proc)<-c("Mach_mean","Mach_var","O","C","E","A","N","tp_p_mean",
                        "tp_n_mean","tp_p_var","tp_n_var","wr_p_mean","wr_n_mean",
                        "wr_mean","wr_p_var","wr_n_var","wr_var","wf_mean",
                        "wf_var","age_group","education","engnat",
                        "gender","hand","married","race","religion","familysize","urban",'voted')

str(train_proc)
head(train_proc)
#################################################################
# ?? ???? ?? ?À´? ?Ã°?####

# QE
QE <- c()
for(i in 1:20){
  QE[i] <- paste0("Q",letters[i],"E")
}
train_QT <- data.frame(train_new[,QE])

head(train_QT)

train_proc <- cbind(train_proc, train_QT)
str(train_proc)
train_proc$QE_mean <- apply(train_proc[,c(31:50)],1,mean)

train_proc <- train_proc %>% arrange(QE_mean)
boxplot(train_proc$QE_mean)

train_proc <- train_proc[c(1:45200),]
for(i in 31:50){
  print(fivenum(log(train_proc[,i])))
}# -inf -> 0 ?? median
for(i in 31:50){
print(train_proc %>% filter(train_proc[i]==0))
}

which(train_proc$QhE==0)
train_proc$QkE[18402]<-median(train_proc$QkE)
train_proc$QkE[40910]<-median(train_proc$QkE)
train_proc$QjE[26148]<-median(train_proc$QjE)
train_proc$QoE[33066]<-median(train_proc$QoE)
train_proc$QpE[9306]<-median(train_proc$QpE)
train_proc$QqE[9225]<-median(train_proc$QqE)
train_proc$QhE[29633]<-median(train_proc$QhE)
train_proc$QiE[29633]<-median(train_proc$QiE)

train_proc[18402,31:50]
train_proc[40910, 31:50]
train_proc[26148, 31:50]
train_proc[33066, 31:50]
train_proc[9306, 31:50]
train_proc[9225, 31:50]
train_proc[29633, 31:50]

for(i in 31:50){
  print(fivenum(log(train_proc[,i])))
}
summary(train_proc[,31:50])
dim(train_proc)
train_proc <- train_proc[c(1:45150),]

train_proc$QE_mean <- apply(train_proc[,c(31:50)],1,mean)
train_proc[,31:51]<-log(train_proc[,31:51])
head(train_proc)

boxplot(train_proc$QE_mean)

train_proc=train_proc[!(train_proc$familysize==2147483647),]
train_proc=train_proc[!(train_proc$familysize==999),]
train_proc=train_proc[!(train_proc$familysize==100),]

str(train_proc)


##################
str(train_proc)

train_proc <- train_proc[,c(1,3:7,14,20:30,51)]

str(train_proc)

#10???Îµ? ??Ç¥?? ???????Ìµ? ??Á¦
train_proc <- train_proc[!((train_proc$age_group==1)&(train_proc$voted==1)),]

#10???Îµ? ?Ð¼????????? ???????Ìµ? ??Á¹?? ????
train_proc[((train_proc$age_group==1)&(train_proc$education==3))|
             ((train_proc$age_group==1)&(train_proc$education==4)),]$education <- 2

#??Á¾ ????
train_proc[((train_proc$race==1)|(train_proc$race==3)|(train_proc$race==4)|
              (train_proc$race==5)),]$race <- 7
table(train_proc$race)
table(train_proc$age_group, train_proc$education)

train_proc <- train_proc[,c(1,3:7,14,20:51)]
str(train_proc)



