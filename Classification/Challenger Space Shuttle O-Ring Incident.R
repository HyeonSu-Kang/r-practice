#Classification 
#Challenger Space Shuttle O-Ring Incident

#챌린저호 고무링 데이터 분석#1 - 데이터 로딩
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/_/미래/rstuido/new")
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
#온도와 실패한 O링의 개수를 시각화
chall %>% ggplot(aes(temperature,distress_ct))+geom_point()
chall %>% ggplot(aes(factor(distress_ct),temperature))+geom_boxplot()

#챌린저호 고무링 데이터 분석 #3 - 모델링
chall_glm<-glm(cbind(distress_ct,o_ring_ct-distress_ct)~temperature,
               data = chall,
               family = 'binomial')
chall_glm
summary(chall_glm)
predict(chall_glm,data.frame(temperature=30))
#결과 값이 [0,1]을 벗어나 확률값이 아닌 것을 알 수 있음
#predict함수에서 type을 지정하지 않을 경우 선형 예측값 출력(default=link)
predict(chall_glm,data.frame(temperature=30),type='response')

#챌린저호 고무링 데이터 분석 #5 - 적합결과 시각화
logistic<-function(x){
  exp(x)/(exp(x)+1)
}
plot(c(20,85),c(0,1),type = "n",xlab = "Temperature",ylab = "Prob")
tp<-seq(20,85,1)
tp
chall_gm_pred<-predict(chall_glm,data.frame(temperature=tp),
                       se.fit = TRUE)
#chall_gm_pred는 tp에 대한 종속 변수 값 예측 결과와 함께 예측 표준 오차를 담고 있음음
lines(tp,logistic(chall_gm_pred$fit))
lines(tp,logistic(chall_gm_pred$fit-1.96*chall_gm_pred$se.fit),lty=2)
lines(tp,logistic(chall_gm_pred$fit+1.96*chall_gm_pred$se.fit),lty=2)
abline(v=30,lty=2,col='blue')

#앞선 분석에서는 화씨를 이용하여 모델리을 진행하였으나 우리가 사용하는 섭씨가 아니라 그결과가 잘 와닿지 않음.섭씨로 바꾸어서 모델링을 진행해보겠음

chall$temperature<-(chall$temperature-32)*5/9
glimpse(chall)

chall %>% ggplot(aes(temperature,distress_ct))+geom_point()
chall %>% ggplot(aes(factor(distress_ct),temperature))+geom_boxplot()

chall_glm<-glm(cbind(distress_ct,o_ring_ct-distress_ct)~temperature,
               data = chall,family = 'binomial')
chall_glm
summary(chall_glm)

#(30-32)*5/9=-1.111111
predict(chall_glm,data.frame(temperature=-1.111111))
predict(chall_glm,data.frame(temperature=-1.111111),type = 'response')

logistic<-function(x){
  exp(x)/(exp(x)+1)
}
#(20-32)*5/9= -6.66666,(85-32)*5/9=29.44444
plot(c(-6.666667,29.44444),c(0,1),type = "n",xlab = " Celsius Temperature",ylab = "Prob")
tp=seq(-6.666667,29.44444,1)
chall_gm_pred<-predict(chall_glm,data.frame(temperature=tp),
                       se.fit = TRUE)
lines(tp,logistic(chall_gm_pred$fit))
lines(tp,logistic(chall_gm_pred$fit-1.96*chall_gm_pred$se.fit),lty=2)
lines(tp,logistic(chall_gm_pred$fit+1.95*chall_gm_pred$se.fit),lty=2)
abline(v=-1.111111,lty=2,col='blue')
