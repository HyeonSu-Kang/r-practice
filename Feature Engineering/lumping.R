#Categorical feature engineering-Laumping
#범주형 변수의 축소
#대량의 데이터가 수집되다 보면 피처는 많은 레벨을 포함하게 되는 경우가 있음

install.packages("AmesHousing")
library(AmesHousing)
ames<-AmesHousing::make_ames()
set.seed(123)
index_1<-sample(1:nrow(ames),round(nrow(ames)*.7))
ames_train<-ames[index_1, ]
ames_test<-ames[-index_1, ]

install.packages("dplyr")
library(dplyr)
count(ames_train,Neighborhood) %>% arrange(n)
count(ames_train,Screen_Porch) %>% arrange(n)

#Collpsing 또는 Lumping을 통해 레벨을 축소함으로써 이익을 얻을 수 있음
#step_other(변수명,threshold=0.1,other="other")-threshold보다 낮은 레벨은 정의된 other로 변환
#bake(training daa,new_data=NULL)-파이프라인에 따라 처리된 데이터 반환

#전처리 파이프라인 생성
install.packages("recipes")
library(recipes)
ames_train$Screen_Porch <- as.factor(ames_train$Screen_Porch)
lumping<-recipe(Sale_Price~.,data=ames_train) %>% 
  step_other(Neighborhood,threshold=0.01,other="other") %>% 
  step_other(Screen_Porch,threshold=0.1,other=">0")

#파이프라인에 따라 전처리된 데이터 반환환
apply_2_training<-prep(lumping,training = ames_train) %>% 
  bake(ames_train)

#축소된 레벨 확인
count(apply_2_training,Neighborhood) %>% arrange(n)
