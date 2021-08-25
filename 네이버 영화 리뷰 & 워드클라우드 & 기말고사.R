### 학번: 1518784     이름: 장윤호    빅데이터분석 시각화 프로그램


#문제 1

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


if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

library(dplyr)
library(RSQLite)
library(xlsx)

#위치 지정

setwd("C:/Shiny/final")
load("testData.rda")

str(testData)


#문제 2번
testData$grdp_per <- (testData$GRDP/testData$pop)*100
testData$grdp_per

testData$test_den <- (testData$pop/testData$nationPop)*100
testData$test_den

summary(testData$grdp_per)
summary(testData$test_den)


#문제 3-1번

testData$nation

test_sum <- testData %>%
  group_by(nation) %>%
  summarise(num = sum(pop),
            max_grdp= max(grdp_per))
test_sum

#문제 3-2번

barplot(sort(test_sum$num,decreasing=TRUE), main=paste("1518784 장윤호"),
        ylab="인구수", col=brewer.pal(18,"BrBG"), cex.names = 0.5)



#문제 3-3번

sort(test_sum$num)
test_sum

#문제 4-1번

testData


nat_select <- testData[testData$nation %in% c("AUS","CAN","DEU","ITA","JPN","KOR","GBR","USA"),]
str(nat_select)
nat_select

#문제 4-2번

max(nat_select$grdp_per)

maxdd <- subset(nat_select, grdp_per> 7.719610 & grdp_per <7.7197)
maxdd


#문제 4-3번

graphics.off()

boxplot(nat_select, col=brewer.pal(17,"Pastel2"),
        horizontal=T)

boxplot(grdp_per~nation, nat_select, xlab = "국가", ylab = "1인당GRDP", col=1:8)

boxplot(test_den~nation, nat_select, xlab = "국가", ylab = "인구밀도", col=1:8)

## 문제 4-3설명
# 답안지

# 문제 4-5번 (4-4번 없음)

str(nat_select)

cor(nat_select[c("grdp_per","test_den","doctorCnt","highServicerRate","airPollution")])


#문제 4-6번

install.packages("ggcorrplot")
library(ggcorrplot)


dd<-subset(nat_select, select = c("grdp_per","test_den","doctorCnt","highServicerRate","airPollution"))
dd

corr<-round(cor(dd),3)
corr


ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3,
           method = "circle",
           colors =c("tomato2", "white","springgreen3"),
           title = "1518784 장윤호",
           ggtheme = theme_bw())


#문제 4-7번

str(corr)

cor.test(nat_select$grdp_per, nat_select$test_den, method="pearson")

cor.test(nat_select$grdp_per, nat_select$doctorCnt, method="pearson")

cor.test(nat_select$grdp_per, nat_select$highServicerRate, method="pearson")

cor.test(nat_select$grdp_per, nat_select$airPollution, method="pearson")


#문제 5번


nat_select

ggg <- ggplot(nat_select, aes(x=grdp_per, y=test_den))+
    geom_point(aes(col=nation, size=doctorCnt))+
    labs(title="1518784 장윤호 bubble chart", subtitle="1인당 GRDP와 인구밀도",
         x= "1인당 GRDP", y="인구밀도")
ggg



############################################
#문제 6번과 7번은 추후에 업로드 하겠습니다.#
############################################


문제 6번

str(testData)
head(testData)

testData

nat_grdp <- testData %>%
  group_by(nation) %>%
  summarise(air= mean(airPollution),
            exlif= mean(expectedLife),
            grdp = mean(grdp_per))
nat_grdp
nat_grdp <- nat_grdp[!nat_grdp$nation=='FRA',]

d<- ggplot(nat_grdp, aes(x=air, y=exlif))+
  geom_point(aes(col=nation, size=grdp))+
  labs(title="1518784 장윤호 bubble chart", subtitle="국가별 공기오염에 따른 기대수명 비교",
       x= "공기 오염", y="기대 수명")
d

#자세한 설명은 첨부된 pdf파일 혹은 한글 파일에 있습니다!



#문제 7번

#참고: https://blog.naver.com/hongyou022/221853366013

#텍스트 분석 및 워드 클라우드를 위한 설치

install.packages("rJava")
install.packages("multilinguer")
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type ="binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))

install.packages("KoNLP") source("https://install-github.me/talgalili/installr") installr::install.java() library(KoNLP)

install.packages('wordcloud2')

library(multilinguer)
#library(tidyverse)
library(wordcloud2)
#library(nord)
library(dplyr)
library(KoNLP)
library(RColorBrewer)
library(rvest)
library(stringr)

#세종 사전
useSejongDic()  


##################################################
# 영화 '미나리' 리뷰에 대한 워드 클라우드 만들기 #
##################################################

#네이버에 미나리 영화코드 187310

movie_url <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=187310&type=after&onlyActualPointYn=N&onlySpoilerPointYn=Y&order=newest&page="

reply_list <- NULL

#페이지 1에서 20까지 
for(page in 1:20){
  
  url <- paste(movie_url,page,sep='')
  content <- read_html(url)
  
  node_1 <- html_nodes(content,".score_reple p") #댓글

  reply <- html_text(node_1)
  reply <- as.character(str_trim(reply))
 
  reply_list = append(reply_list, reply)
 
}

reply_list
dd<-reply_list

reviews <- gsub("관람객", "", reviews)
reviews <- gsub("\t|\n|\r", "", reviews)
reviews

# 파일에서 명사 추출
n1 <- sapply(reviews, extractNoun, USE.NAMES=F)
n1
n2 <- unlist(n1) 
n2
head(n2,10)

# 추출된 명사들 중 필요없는것 삭제

n2 <- gsub("들이", "", n2)
n2 <- gsub("영화", "", n2)
n2 <- gsub("", "", n2)
n2 <- gsub("년대", "", n2)
n2 <- gsub("하게", "", n2)
n2 <- gsub('[ㄱ-ㅎ]', "", n2)
n2 <- gsub('\\d+', "", n2)

# 두 글자 이상만 추출
n2 <- Filter(function(x){nchar(x) >= 2}, n2)


# 워드 카운트 정리
word_count <- table(n2) 
sort(word_count, decreasing = T)

# 순서대로 정렬해서 50개만 출력 
head(sort(word_count, decreasing=TRUE), 50)

df_word <- as.data.frame(word_count, stringsAsFactors = F)
df_word <- rename(df_word,
                  word = n2,
                  freq = Freq)
df_word

##top100으로 wordcloud2 그리기


top100_mi <- df_word %>%
  arrange(desc(freq)) %>%
  head(100)

top100_mi

wordcloud2(top100_mi, size=0.5,
           color='random-dark',
           backgroundColor = 'white',
           gridSize = 10)

read.csv(top100_mi, "minari.csv")


##################################################
# 영화 '기생충' 리뷰에 대한 워드 클라우드 만들기 #
##################################################

#네이버에 기생충 영화 코드 161967

movie_url <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=161967&type=after&onlyActualPointYn=N&onlySpoilerPointYn=Y&order=newest&page="

reply_list <- NULL

#페이지 1에서 20까지 
for(page in 1:20){
  url <- paste(movie_url,page,sep='')
  content <- read_html(url)
  node_1 <- html_nodes(content,".score_reple p") #댓글
  reply <- html_text(node_1)
  reply <- as.character(str_trim(reply))
  reply_list = append(reply_list, reply)
}

dd<-reply_list

reviews <- gsub("관람객", "", dd)
reviews <- gsub("\t|\n|\r", "", reviews)
reviews

# 파일에서 명사 추출
noun <- sapply(reviews, extractNoun, USE.NAMES=F)
mode(noun)
noun
noun2 <- unlist(noun) 
noun2
head(noun2,10)

# 사전에 단어 추가 및 추출된 명사의 삭제

noun2 <- gsub("^\u314b", "", noun2)
noun2 <- gsub("영화", "", noun2)
noun2 <- gsub("", "", noun2)
noun2 <- gsub("하게", "", noun2)
noun2 <- gsub("^ㅋ", "", noun2)
noun2 <- gsub('[ㄱ-ㅎ]', "", noun2)
noun2 <- gsub('\\d+', "", noun2)
noun2 <- gsub("송강호\\S*", "송강호", noun2)

# 두 글자 이상만 추출
noun2 <- Filter(function(x){nchar(x) >= 2}, noun2)


# 워드 카운트 정리
word_count <- table(noun2) 
sort(word_count, decreasing = T)

# 순서대로 정렬해서 50개만 출력 
head(sort(word_count, decreasing=TRUE), 50)

df_word1 <- as.data.frame(word_count, stringsAsFactors = F)
df_word1 <- rename(df_word1,
                  word = noun2,
                  freq = Freq)
df_word1

##top100으로 wordcloud2 그리기


top100_gi <- df_word1 %>%
  arrange(desc(freq)) %>%
  head(100)

top100_gi

wordcloud2(top100_gi, size=0.5,
           color='random-dark',
           backgroundColor = 'white',
           gridSize = 10)

read.csv(top100_gi, "gisaeng.csv")

