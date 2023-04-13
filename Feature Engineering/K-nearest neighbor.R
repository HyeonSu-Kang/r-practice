#Dealing wiht missingenss
#K-nearest neighbor
#step_impute_knn("columns name")-K-NN알고리즘을 이용하여 결측치 대체

#임의 결측치 발생
install.packages("AmesHousing")
library(AmesHousing)
ames<-AmesHousing::make_ames()
set.seed(123)
index_2<-sample(1:nrow(ames),round(nrow(ames)*.1))
ames_temp<-ames[index_2, ]
ames_remove_index<-sample(1:nrow(ames_temp),round(nrow(ames_temp)*0.2))
ames_remove<-ames[ames_remove_index, ]
ames_temp[ames_remove_index,'Gr_Liv_Area']<-NA
sum(is.na(ames_temp$Gr_Liv_Area))

#전처리 파이프라인 생성
install.packages("recipes")
library(recipes)
ames_recipe<-recipe(Sale_Price ~.,data = ames_temp) %>% 
  step_log(all_outcomes()) %>% 
  step_impute_knn(all_predictors(),neighbors = 6)
imp_knn_modles<-prep(ames_recipe,training = ames_temp)
imp_knn_modles

#파이프라인 결과 확인
head(imp_knn_modles$template$Gr_Liv_Area)

#원시데이터와 imputation 값 비교를 위한 데이터프레임 생성
result_1<- data.frame(
  "Sale_Price"=ames_remove$Sale_Price,
  "Gr_Live_Area"=ames_remove$Gr_Liv_Area,"Type"="Origin"
)
result_2 <- data.frame(
  "Sale_Price" = ames_remove$Sale_Price,
  "Gr_Live_Area" = imp_knn_modles$template$Gr_Liv_Area[ames_remove_index],
  "Type" = "K-NN"
)
result_imp_knn=rbind(result_1,result_2)
head(result_imp_knn)
result_imp_knn

#ggplot을 이용한 시각화
install.packages("ggplot2")
library(ggplot2)
ggplot(
  data=result_imp_knn,
  aes(x=Gr_Live_Area,
      y=Sale_Price))+geom_point(pch=19,size=1.5,aes(colour=factor(Type)))
