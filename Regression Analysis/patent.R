#Regression Analysis
#특허출원건수 예측

#다음 데이터를 입력하고 기온에 따른 특허출원 건수에 대한 예측 모델을 선형회귀를 이용하여 모델링 해보자 
PatTempSet <-data.frame(
  Temp=c(1.5,0.9,1.4,0.5,-0.1,2.4,0.7,1.7,0.5,-0.7,-0.4,-1,1.5,0.7),
  Applicant=c(48,80,100,110,63,24,42,52,95,150,116,121,106,112)
)

#특허출원 건수 예측모델링 1-데이터 확인
install.packages("ggplot2")
library(ggplot2)
ggplot(PatTempSet,aes(x=Temp,y=Applicant))+geom_point()

#특허출원 건수 예측모델링 2-모델링 및 검정
result<-lm(Applicant~.,data=PatTempSet)
summary(result)

#F비는 10.49이며, 이 값은 p<.001수준에서 유의하다 -> 즉,회귀모형을 이용해서 예측하는 것이 더 낫다