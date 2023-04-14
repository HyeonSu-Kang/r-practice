#Regression Analysis
#This datase contains imformation collected by the U.S Service concerning housing in the area of Boston Mass.

#데이터 로딩

setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/_/미래/rstuido/new")
getwd()

install.packages("dplyr")
install.packages("ggplot2")
install.packages("MQMF")
install.packages("tidyr")
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
