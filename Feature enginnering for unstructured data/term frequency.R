#문서의 정량화  
#제인 오스틴의 소설 데이터셋을 이용하여 소설 속 용어빈도(tf) 실습

install.packages("dplyr")
install.packages("janeaustenr")
install.packages("tidytext")
library(dplyr)
library(janeaustenr)
library(tidytext)

#소설 내 등장하는 단어와 그 개수를 나타냄

book_words<- austen_books() %>% 
  unnest_tokens(input = text,output = word) %>% 
  count(book,word,sort = TRUE) %>% 
  ungroup()
book_words

#소설-용어-용어빈도=해당 소설에 나오는 모든 단ㅇ어의 수로 구성된 tiny data
total_words <- book_words %>% 
  group_by(book) %>% 
  summarise(total=sum(n))
total_words
book_words<-left_join(book_words,total_words)
book_words

#시각화
install.packages("ggplot2")
library(ggplot2)
ggplot(book_words,aes(n/total,fill=book))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA,0.0009)+
  facet_wrap(~book,ncol=2,scales = "free_y")

#지프의 법칙
freq_by_rank<-book_words %>% 
  group_by(book) %>% 
  mutate(rank=row_number(),
         `term frequency`=n/total)
freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank,`term frequency`,color=book))+
  geom_line(size=1.1,,alpha=.8,show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()
