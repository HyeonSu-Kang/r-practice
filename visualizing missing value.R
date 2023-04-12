#Dealing with missingness

#Visualizing missing value
#시각화 기법을 이용하면 데이텃에 포함된 결측갑의 위치와 각 속성에 포함된 정도를 한번에 확인가능 
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)

#AmesHousing은 Ames, Iowa의 부동산 데이터셋. 이 데이터셋은 주택의 다양한 특성과 가격을 포함
install.packages("AmesHousing")
library(AmesHousing)

AmesHousing::ames_raw %>% 
  is.na() %>%
  ggplot(aes(Var2,Var1,fill=value))+
  geom_raster()+
  coord_flip()+
  scale_y_continuous(NULL,expand = c(0.0))+
  scale_fill_grey(name="",label=c("Present","Missing"))+
  xlab("Observation")+
  theme(axis.text.y=element_text(size=4))

#visdat은 데이터셋의 변수별 결측값을 시각화해주는 함수
install.packages("visdat")  
library(visdat)  
vis_miss(AmesHousing::ames_raw,cluster = TRUE)
