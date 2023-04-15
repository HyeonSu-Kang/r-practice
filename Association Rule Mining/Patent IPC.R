#Association Rule Mining
#특허IPC를 이용한 기술탐색과 연구 기획

#특허IPC데이터 연관분석 #1 - 데이터전처리
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

setwd("C:/Users/SAMSUNG/OneDrive/바탕 화면/_/미래/rstuido/new")
getwd()
PatentIpcList<-read.transactions("./patent_ipc_list.txt",
                         format = "basket",
                         sep = ",",
                         skip=1)
inspect(PatentIpcList)
summary(PatentIpcList)
inspect(PatentIpcList[1:5])
itemFrequencyPlot(PatentIpcList,
                  xlab="IPC코드",yla="비율",col=1:5,topN=10)

#특허IPC데이터 연관분석 #2 - 연관분석
rules<-apriori(PatentIpcList,
               parameter = list(supp=0.05,conf=0.1))

#특허IPC데이터 연관분석 #3 - 연관분석 결과 확인하기기
inspect(head(sort(rules,by="support"),15))
plot(rules,method="graph")
