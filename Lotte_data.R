### Lotte Data #####
### Loading and installing packages ###
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(sqldf)){
  install.packages("sqldf")
  library(sqldf)
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
library(dplyr)
setwd("E:/윤호/동아대학교-1/4학년 1학기/빅데 시각화/파일&코드/롯데/오리")
### Define analysis data ####
file=choose.files()
customer <- read.table(file, header=T, sep=",")
file1 = choose.files()
purchaseList <- read.table(file1, header=T, sep=",")

head(customer)

tb <- sqldf("select a.id, a.성별, a.연령, b.거래일자, b.상품대분류명, b.상품중분류명, b.구매건수, b.거래식별ID, 
      b.구매금액, b.점포ID from customer as a, purchaseList as b 
      where a.id = b.id")

## Create date field ##
tb$거래일자 <- as.Date(as.character(tb$거래일자), "%Y%m%d")
tb$거래월 <- as.POSIXlt(tb$거래일자)$mon ##월발췌를 위해 POSIXlt객체 사용
####### lubridate 패키지 이용 ###
tb$거래일자 <- ymd(tb$거래일자)
tb$거래월 <- month(tb$거래일자)
### Data exploration
#sub1 <-aggregate(round(tb$구매금액/1000,0), by=list(catDate=as.character(tb$거래월), catProduct=tb$상품대분류명, catStore=tb$점포ID), FUN=sum)
#sub2 <-aggregate(tb$구매건수, by=list(catMonth=as.character(tb$거래월), catProduct=tb$상품대분류명, catStore=tb$점포ID), FUN=sum)
#summary(sub1$x)
#sub1.quan <- quantile(sub1$x, prob=c(0.1, 0.25, 0.5, 0.75, 0.9))

s1 <- as.data.frame(tb %>% group_by(거래월, 상품대분류명, 점포ID) %>%
                      summarise(amount=sum(round(구매금액/1000,0)), cnt=sum(구매건수)))
s1.quan <- quantile(s1$amount, prob=c(0.1, 0.25, 0.5, 0.75, 0.9))

s2 <- as.data.frame(tb %>% group_by(상품대분류명, 점포ID) %>% 
                      summarise(amount=sum(round(구매금액/1000,0)), cnt=sum(구매건수)))
s1 <-subset(s1, 거래월!=10) #10월삭제
summary(s1$amount)
summary(s1$cnt)


## 점포별 매출액의 차이 
tapply(s1$amount, s1$점포ID, sum)
tapply(s1$amount, list(s1$점포ID, s1$거래월), sum)
boxplot(s1$amount~s1$점포ID)
boxplot(s1$amount~s1$상품대분류명)


#### 5월 7일 #####

table(s1$거래월)

s1 <- subset(s1, 거래월!=10 ) #10월 삭제

tapply(s1$amount, list(s1$점포ID, s1$월), sum)
boxplot(s1$amount~s1$점포ID)

g<- ggplot(s1, aes(x=점포ID, y= amount, fill= 점포ID)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3")+
  stat_boxplot(geom="errorbar")+
  stat_summary(fun.y = mean, geom = "point", shape= 23, size = 3, fill = "red")+
  labs(title="롯데 데이터 Boxplot", x="점포명", y="매출액")

g
#d


s2 <- as.data.frame(tb %>% group_by(상품대분류명, 점포ID) %>% 
                      summarise(amount=sum(round(구매금액/1000,0)), cnt= sum(구매건수)))
s1 <- as.data.frame(tb %>% group_by(거래월,상품대분류명, 점포ID) %>% 
                      summarise(amount=sum(round(구매금액/1000,0)), cnt= sum(구매건수)))
summary(s1$cnt)

head(tb)

###bubble plot###

(g1 <- ggplot(s1, aes(상품대분류명, amount))+
    geom_point(aes(col = 점포ID, size=cnt))+
    labs(title="", x="품목", y="매출액"))

### Tile plot###

(g2 <- ggplot(s1, aes(거래월, 점포ID))+
    geom_tile(aes(fill=amount))+
    scale_fill_gradientn(colours = brewer.pal(n=6, name="RdBu"))+
    ggtitle("월별 거래처에 대한 매출액"))

(g3 <- ggplot(s1, aes(거래월, 상품대분류명))+
    geom_tile(aes(fill=amount))+
    facet_wrap(~점포ID)+
    scale_fill_gradientn(colours = brewer.pal(n=6, name="RdBu"))+
    ggtitle("월별 거래처에 대한 매출액"))





g <- ggplot(s1, aes(점포ID, amount, fill=점포ID))
(g + geom_boxplot() + 
     stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="red")+
     stat_boxplot(geom ='errorbar')+
     scale_fill_brewer(palette="Set3")+
     labs(title="Box plot", 
       subtitle="매출액 vs 점포명",
       caption="Source: 롯데데이터, red=평균",
       x="점포명",
       y="매출액"))


##### 시각화 
(g <- ggplot(s2, aes(x=상품대분류명, y=amount)) + 
     geom_point(aes(col=점포ID, size=cnt)) +
     labs(title="Bubble Chart", subtitle="점포별 : 품목 vs 매출액",
       x = "품목", y="매출액"))
ggplot(s1, aes(x=거래월, y=점포ID)) + 
  geom_tile(aes(fill=amount)) +
  scale_fill_gradientn(colors =brewer.pal(n=5, name="RdBu")) +
  ggtitle("월별 거래처에 대한 매출액") + 
  xlab("거래월") +
  ylab("거래처") 
  
ggplot(s1, aes(x=거래월, y=상품대분류명)) + 
  geom_tile(aes(fill=amount)) +
  facet_wrap(~점포ID) +
  scale_fill_gradientn(colors =brewer.pal(n=5, name="RdBu")) +
  ggtitle("월별 거래처에 대한 매출액") + 
  xlab("거래월") +
  ylab("거래처") 


##### 인구특성별 분석
tb$age

sub3 <-aggregate(x=round(tb$구매금액/1000,0), 
                 by=list(catDate=as.character(tb$거래월),
                         catProduct=tb$상품대분류명,
                         catStore=tb$점포ID,
                         gender=tb$성별, ), FUN=sum)

head(tb)


ggplot(s1, aes(x=거래월, y=상품대분류명)) + 
  geom_tile(aes(fill=amount)) +
  facet_wrap(~점포ID) +
  scale_fill_gradientn(colors =brewer.pal(n=5, name="RdBu")) +
  ggtitle("월별 거래처에 대한 매출액") + 
  xlab("거래월") +
  ylab("거래처") 


dd <- as.data.frame(tb %>% group_by(연령) %>% 
                      summarise(amount=sum(구매금액),
                                cnt= sum(구매건수)))

head(tb)
head(dd)

(g <- ggplot(dd, aes(x=연령, y=amount)) + 
    geom_point(aes( col = 'pastel',size=cnt)) +
    labs(title="Bubble Chart", subtitle="나이대별 구매금액 및 구매건수",
         x = "연령", y="구매금액"))
dd
g


head(tb)
