#Classification 
#Challenger Space Shuttle O-Ring Incident

#챌린저호 고무링 데이터 분석#1 - 데이터 로딩
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/_/미래/rstuido/r-practice/Classfication")
getwd()
chall<-read.csv("./o-ring-erosion-only.data",
                sep=' ',
                header = FALSE,
                strip.white = TRUE,
                stringsAsFactors = TRUE)
head(chall)

#데이터 프레임의 열(컬럼) 이름들을 ("o_ring_ct", "distress_ct", "temperature", "pressure", "launch_id") 로 변경

#"o_ring_ct" : 오링 개수
#"distress_ct" : 문제 발생 횟수
#"temperature" : 발사 직전 기온 (화씨도로 측정)
#"pressure" : 발사 직전 압력 (psi로 측정)
#"launch_id" : 발사 id (숫자)

names(chall)<-c("o_ring_ct","distress_ct","temperature","pressure","luanch_id")
glimpse(chall)

#챌린저호 고무링 데이터 분석 #2 - 데이터 시각화
chall %>% ggplot(aes(temperature,distress_ct))+geom_point()
