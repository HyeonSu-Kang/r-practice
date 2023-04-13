#Numeric feature engineering-Standardization
#표준화-데이를 0중심으로 양쪽으로 분포 시키는 방법으로, 표준화를 통해 데이터가 평균을 기준으로 얼마나 떨어져 있는지를 알 수 있음

#step_Yeojohnson() : 여-존슨변환
#step_center() : 각 데이터의 값에서 평균을 빼는 함수
#step_scale() : 표준편차로부터 데이터 값을 나누는 함수

install.packages("AmesHousing")
library(AmesHousing)
ames<-AmesHousing::make_ames()
set.seed(123)
index_1<-sample(1:nrow(ames),round(nrow(ames)*.7))
ames_train<-ames[index_1, ]
ames_test<-ames[-index_1, ]

ames_recipe<-recipe(Sale_Price ~.,data = ames_train) %>% 
  step_YeoJohnson(all_numeric())

ames_recipe %>% 
  step_center(all_numeric(),-all_outcomes()) %>%
  step_scale(all_numeric(),-all_outcomes()) %>% 
  prep()->result

install.packages("ggplot2")
library(ggplot2)
ggplot()+ geom_point(
  mapping=aes(x=Lot_Frontage,y=Alley),
  data=ames_train,pch=19,size=1.5,alpha=.2)
ggplot()+geom_point(
  mapping = aes(x=Lot_Frontage,y=Alley),
  data = result$template,pch=19,size=1.5,alpha=.2)
