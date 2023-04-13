#Dealing with missingness
#Estimaed statistics

#임의 결측치 발생
install.packages("AmesHousing")
library(AmesHousing)
ames<-AmesHousing::make_ames()
set.seed(123)
index_2<-sample(1:nrow(ames),round(nrow(ames)*.1))
ames_temp<-ames[index_2, ]
ames_remove_index<-sample(1:nrow(ames_temp),round(nrow(ames_temp)*0.2))
ames_remove<- ames[ames_remove_index, ]
ames_temp[ames_remove_index,'Gr_Liv_Area']<-NA
sum(is.na(ames_temp$Gr_Liv_Area))

#전처리 파이프라인 생성
library(recipes)
ames_recipe<-recipe(Sale_Price~.,data=ames_temp) %>% 
  step_impute_median("Gr_Liv_Area")
imp_models<-prep(ames_recipe,training = ames_temp)

#파이프라인 결과 확인
head(imp_models$template$Gr_Liv_Area)

#원시데이터와 Imputation 값 비교를 위한 데이터프레임 생성
result_1<- data.frame(
  "Sale_Price"=ames_remove$Sale_Price,
  "Gr_Live_Area"=ames_remove$Gr_Liv_Area,"Type"="Origin"
)
result_2 <- data.frame(
  "Sale_Price" = ames_remove$Sale_Price,
  "Gr_Live_Area" = imp_models$template$Gr_Liv_Area[ames_remove_index],
  "Type" = "Median"
)
result_imp_median=rbind(result_1,result_2)
head(result_imp_median)

#ggplot을 이용한 시각화
install.packages("ggplot2")
library(ggplot2)
ggplot(
  data=result_imp_median,
  aes(x=Gr_Live_Area, y=Sale_Price)) +
  geom_point(pch=19, size=1.5, aes(colour=factor(Type)))
  
