#Regression Analysis
#This datase contains imformation collected by the U.S Service concerning housing in the area of Boston Mass.

#데이터 로딩

setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/_/미래/rstuido/r-practice")
getwd()

install.packages("dplyr")
install.packages("ggplot2")
install.packages("MQMF")
install.packages("tidyr")
install.packages("psych")
library(psych)
library(dplyr)
library(ggplot2)
library(MQMF)
library(tidyr)

house_price<-read.csv("./HousingData.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)
head(house_price)
house_price<-house_price %>% drop_na()

#데이터 요약/기초 통계량

glimpse(house_price)
summary(house_price)

install.packages("reshape2")
library(reshape2)
normalization<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}
temp_df<-melt(normalization(house_price))
ggplot(temp_df,aes(x=variable,y=value))+geom_boxplot()

#데이터 요약/상관분석

pairs(house_price %>% sample_n(min(1000,nrow(house_price))),
      lower.panel=function(x,y){
        points(x,y);
        abline(0,1,col='red')},
      upper.panel=panel.cor)

#예측모델링(데이터분할)
set.seed(230123)
n<-nrow(house_price)
idx<-1:n

train_idx <- sample(idx, n*.6)
valid_idx <- sample(setdiff(idx,train_idx),n*0.2)
test_idx <- setdiff(idx,valid_idx)

sprintf("Train:%d, Valid:%d, Test:%d",
        length(train_idx), length(valid_idx), length(test_idx))
training<- house_price[train_idx, ]
validation<- house_price[valid_idx, ]
test<-house_price[test_idx, ]

#예축모델링(선형회귀모델)
data_lm_full<-lm(MEDV~.,data = training)
summary(data_lm_full)
predict(data_lm_full,newdata = house_price[1:5, ])

#예축모델링(교호작용을 고려한 선형회귀모델)
data_lm_full_2<-lm(MEDV~.^2,data = training)
summary(data_lm_full_2)
sprintf("The number of parameter is :%d",length(coef(data_lm_full_2)))

#예축모델링(단계별 선택법을 이용한 모델 선정)
library(MASS)
data_step<-stepAIC(data_lm_full,
                  scope = list(upper= ~.^2,lower=~1))
data_step
anova(data_step)
summary(data_step)

#모델평가
y_obs<-validation$MEDV
yhat_lm<-predict(data_lm_full,newdata = validation)
yhat_lm_2<-predict(data_lm_full_2,newdata = validation)
yhat_step<-predict(data_step,newdata = validation)

rmse<-function(y_obs,y_hat){
  sqrt(mean((y_obs-y_hat)^2))
}
sprintf("Linear model: %.2f, Linear coef model: %.2f, Step model: %.2f",
        rmse(y_obs,yhat_lm),
        rmse(y_obs,yhat_lm_2),
        rmse(y_obs,yhat_step))

#오차값이 작은게 좋기 때문에 스텝(stepwise)변수 선택을 행한 모델의 예축성는이 가장 좋은 것을 확인할 수 있음음









