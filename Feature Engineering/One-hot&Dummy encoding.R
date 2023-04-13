#Categorical feature engineering
#One-hot & Dummy encoding
#범주형 변수를 수치형으로 변환하기 위한 방법
#step_dummpy(변수명,one-hot=FALSE)-One-hot encoding과 Dummy encoding은 같은 함수를 사용하며 one-hot encoding은 옵션으로 설정 가능 

install.packages("AmesHousing")
library(AmesHousing)
ames<-AmesHousing::make_ames()
set.seed(123)
index_1<-sample(1:nrow(ames),round(nrow(ames)*.7))
ames_train<-ames[index_1, ]
ames_test<-ames[-index_1, ]

oh_encoding<-recipe(Sale_Price ~., data = ames_train) %>% 
  step_dummy(all_nominal(),one_hot = TRUE)

apply_2_training<-prep(oh_encoding,ames_train) %>% 
  bake(ames_train)

dim(ames_train)
dim(apply_2_training)
