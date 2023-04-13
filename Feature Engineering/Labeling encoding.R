#Categorical feature engineering
#Labeling encoding
#범주형 변수를 수치형으로 변환하기 위한 방법
#step_integer(변수명)-범주형 변수를 수치형으로 변환하는 것으로 사전에 설정되어 있지 않다면 Encoding은 알파벳 순으로 숫자가 부여

install.packages("AmesHousin")
library(AmesHousing)

ames<-AmesHousing::make_ames()
set.seed(123)
index_1<-sample(1:nrow(ames),round(nrow(ames)*.7))
ames_train<-ames[index_1,]
ames_test<-ames[-index_1,]


lb_encoding<-recipe(Sale_Price~.,data = ames_train) %>% 
  step_integer(MS_SubClass)
apply_2_training<-prep(lb_encoding,ames_train) %>% 
  bake(ames_train)

count(ames_train,MS_SubClass)
count(apply_2_training,MS_SubClass)
