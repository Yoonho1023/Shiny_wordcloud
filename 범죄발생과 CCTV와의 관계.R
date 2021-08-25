setwd("E:/윤호/동아대학교-1/4학년 1학기/빅데 시각화/중간고사/범죄")

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(sqldf)){
  install.packages("sqldf")
  library(sqldf)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
library(dplyr)

#5대 강력사범 데이터 불러오기
crime<- read.csv("stat.csv")
head(crime)

#필요없는 년도의 데이터 제외하고 다시 저장
crime <- crime[crime$연도 %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020),]


# CCTV 데이터 불러오기
cctv_raw<- read.csv("전국CCTV표준데이터.csv")
str(cctv_raw)
cctv<-subset(cctv_raw, select=c(카메라대수, 설치연월))
cctv<-cctv[,c(2,1)]
head(cctv)
str(cctv)


#연월에서 연도만 뽑아서 설치연도라는 칼럼을 만들어서 저장
cctv$설치연도<-substr (cctv$설치연월,5,6)
head(cctv)

cctv$설치연도<- paste(20, cctv$설치연도, sep="")

cctv<-cctv[,c(3,2,1)]
cctv<-cctv[,-3]
table(cctv$설치연도)
str(cctv)


##설치연도가 2000년~2020년에 해당하는 데이터만 뽑아서 다시 저장 
final_cctv <- cctv[cctv$설치연도 %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020),]
head(final_cctv)
table(final_cctv$설치연도)


#final_cctv2와 crime type 확인
str(final_cctv)
str(crime)


#final_cctv의 char type을 integer로 변환
final_cctv$설치연도<- as.integer(final_cctv$설치연도)
final_cctv$카메라대수<- as.integer(final_cctv$카메라대수)
head(final_cctv)
str(final_cctv)


#설치연도별로 카메라 대수의 총합을 구함
final_cctv<-final_cctv %>% group_by(설치연도) %>% summarise(카메라대수 =sum(카메라대수))


#sqldf 라이브러리를 통해 두 데이터셋의 설치연도를 기준으로 붙힘
head(final_cctv)
head(crime)

fin <- sqldf("select a.설치연도,a.카메라대수, b.접수, b.계, b.구공판, b.구약식, 
      b.불기소, b.기타,b.미제 from final_cctv as a, crime as b 
      where a.설치연도 = b.연도")
fin

head(fin)
#################################
##혹시나 모를데이터 저장 (skip)##
#################################
write.csv(fin,"E:/윤호/동아대학교-1/4학년 1학기/빅데 시각화/중간고사/범죄/fin.csv" )
##########################
##데이터 불러오기 (skip)##
##########################
fin<-read.csv("fin.csv")
head(fin)
str(fin)


##EDA (기초통계 확인)

summary(fin)


##boxplot

ggplot(data = fin, aes(y=카메라대수))+geom_boxplot()+
  labs(title = "카메라 대수",
       caption = "Source: 공공데이터 포탈",
       x="",
       y="카메라 대수")+
  theme_bw()
ggplot(data = fin, aes(y=접수))+geom_boxplot()+
  labs(title = "강력범죄 접수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 접수")+
  theme_bw()
ggplot(data = fin, aes(y=계))+geom_boxplot()+
  labs(title = "강력범죄 집계 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 집계 횟수")+
  theme_bw()
ggplot(data = fin, aes(y=구공판))+geom_boxplot()+
  labs(title = "강력범죄 구공판 처리 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 구공판 횟수")+
  theme_bw()
ggplot(data = fin, aes(y=구약식))+geom_boxplot()+
  labs(title = "강력범죄 구약식 처리 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 구약식 횟수")+
  theme_bw()
ggplot(data = fin, aes(y=불기소))+geom_boxplot()+
  labs(title = "강력범죄 불기소 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 불기소 횟수")+
  theme_bw()
ggplot(data = fin, aes(y=기타))+geom_boxplot()+
  labs(title = "강력범죄 기타처리 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 기타처리 횟수")+
  theme_bw()
ggplot(data = fin, aes(y=미제))+geom_boxplot()+
  labs(title = "강력범죄 미제 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="",
       y="강력범죄 미제 횟수")+
  theme_bw()



#산점도(geom_point)

ggplot(data = fin, aes(x= 설치연도, y=카메라대수))+geom_point()+ geom_line()+
  labs(title = "설치연도 별 카메라 대수",
       caption = "Source: 공공데이터 포탈",
       x="설치연도",
       y="카메라 대수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도,y=접수))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 접수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 접수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도,y=계))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 집계 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 집계 횟수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도, y=구공판))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 구공판 처리 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 구공판 횟수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도, y=구약식))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 구약식 처리 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 구약식 횟수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도, y=불기소))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 불기소 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 불기소 횟수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도,y=기타))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 기타처리 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 기타처리 횟수")+
  theme_bw()

ggplot(data = fin, aes(x= 설치연도, y=미제))+geom_point()+geom_line()+
  labs(title = "설치연도 별 강력범죄 미제 횟수",
       caption = "Source: 대 검찰청 검찰통계시스템",
       x="설치연도",
       y="강력범죄 미제 횟수")+
  theme_bw()



#시각화

#강력범죄 접수와 CCTV 카메라 대수
#오른쪽 margin에 공간을 주고 두개를 비교하여 plot 그림

graphics.off()

par(mar = c(5, 4, 4, 6) + 0.1)
plot(fin$접수 ~ fin$설치연도, type = "b", pch = 19, col = "darkblue", axes = FALSE, xlab = "", 
     ylab = "", main = "강력범죄 접수와 CCTV")
axis(side = 2, col = "darkblue", col.axis = "darkblue" )
mtext("강력범죄 접수", side = 2, line = 2.5, col="darkblue")
box()
par(new = TRUE)
plot(fin$카메라대수 ~ fin$설치연도, type = "b", pch = 19, col = "coral3", axes = FALSE, xlab = "", 
     ylab = "")
axis(side = 1, at = 2000:2020, labels = fin$설치연도)
mtext("설치 연도", side = 1, line = 2)
axis(side = 4, col = "coral3", col.axis = "coral3")
mtext("카메라대수", side = 4, col = "coral3", line = 2.5)
legend("top", legend = c("강력범죄 접수", "카메라 대수"), text.col = c("darkblue", "coral3"), 
       pch = c(19,19), col = c("darkblue", "coral3"))


## 강력사건 구약식 처리와 CCTV

graphics.off()
par(mar = c(5, 4, 4, 6) + 0.1)
plot(fin$구약식 ~ fin$설치연도, type = "b", pch = 19, col = "blue violet", axes = FALSE, xlab = "", 
     ylab = "", main = "강력사건 구약식 처리와 CCTV")
axis(side = 2, col = "blue violet", col.axis = "blue violet" )
mtext("강력사건 구약식 처리", side = 2, line = 2.5, col="blue violet")
box()
par(new = TRUE)
plot(fin$카메라대수 ~ fin$설치연도, type = "b", pch = 19, col = "Orange Red", axes = FALSE, xlab = "", 
     ylab = "")
axis(side = 1, at = 2000:2020, labels = fin$설치연도)
mtext("설치 연도", side = 1, line = 2)
axis(side = 4, col = "Orange Red", col.axis = "Orange Red")
mtext("카메라대수", side = 4, col = "Orange Red", line = 2.5)
legend("top", legend = c("강력사건 구약식 처리", "카메라 대수"), text.col = c("blue violet", "Orange Red"), 
       pch = c(19,19), col = c("blue violet", "Orange Red"))



# 미제 강력사건과 CCTV 카메라 대수

graphics.off()

par(mar = c(5, 4, 4, 6) + 0.1)
plot(fin$미제 ~ fin$설치연도, type = "b", pch = 19, col = "forestgreen", axes = FALSE, xlab = "", 
     ylab = "", main = "미제 강력사건과 CCTV")
axis(side = 2, col = "forestgreen", col.axis = "forestgreen" )
mtext("미제 강력사건", side = 2, line = 2.5, col="forestgreen")
box()
par(new = TRUE)
plot(fin$카메라대수 ~ fin$설치연도, type = "b", pch = 19, col = "deeppink1", axes = FALSE, xlab = "", 
     ylab = "")
axis(side = 1, at = 2000:2020, labels = fin$설치연도)
mtext("설치 연도", side = 1, line = 2)
axis(side = 4, col = "deeppink1", col.axis = "deeppink1")
mtext("카메라대수", side = 4, col = "deeppink1", line = 2.5)
legend("top", legend = c("미제 강력사건", "카메라 대수"), text.col = c("forestgreen", "deeppink1"), 
       pch = c(19,19), col = c("forestgreen", "deeppink1"))




