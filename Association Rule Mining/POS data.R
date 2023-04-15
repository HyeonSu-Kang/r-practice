#Association Rule Mining
#Discovering customer purchase patterns using POS data

#POS데이터 연관분석 #1 - 데이터 로딩/Transaction 보기기
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

data("Groceries")
summary(Groceries)
Groceries@itemInfo

#inspect 통해 어떤 상품이 같이 팔리는지 쉽게 파악할 수 있음
inspect(Groceries[1:3])
itemFrequencyPlot(Groceries,
              xlab="상품아이템",ylab="비율",col=1:5,topN=10)

#POS데이터 연관분석 #2 - 연관분석
rules<-apriori(Groceries,
               parameter = list(supp=0.02,conf=0.4,minlen=2,maxlen=3))

#POS데이터 연관분석 #3 - 연관분석 결과 확인하기
inspect(rules)

#POS데이터 연관분석 #4 - 연관분석 결과 확인하기
plot(rules,method = "graph")
#특정대상에 영향 받는 대상 탐색
rules2 <-apriori(Groceries,
                 parameter = list(supp=0.02,conf=0.4,minlen=2,maxlen=3),
                 appearance = list(default="rhs",lhs="root vegetables"))
inspect(rules2)
plot(rules2,method = "graph")
#특정대상에 영향을 주는 대상 탐색
rules3<-apriori(Groceries,
                parameter = list(supp=0.02,conf=0.4,minlen=2,maxlen=3),
                appearance = list(default="lhs",rhs="other vegetables"))
inspect(rules3)
plot(rules3,method="graph")
