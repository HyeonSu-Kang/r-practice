#빈도가 높은 단어 탐색 및 시각화

#오스틴 책 데이터 가져오기

install.packages("janeaustenr")
install.packages("dplyr")
install.packages("stringr")
library(janeaustenr)
library(dplyr)
library(stringr)

summary(austen_books())
original_books<- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber=row_number(),
         chapter=cumsum(
           str_detect(
             text,regex("^chapter [\\divxlc]",
                        ignore_case=TRUE)))) %>% 
  ungroup()
original_books

#불용어 제거 및 단어빈도 나타내기

install.packages("tidytext")
library(tidytext)
tidy_books<-original_books %>% 
  unnest_tokens(input = text,output = word)
tidy_books
data(stop_words)
tidy_books<-tidy_books %>% 
  anti_join(stop_words)
tidy_books %>% 
  count(word,sort = TRUE)

#흔한(common) 단어 시각화하기

install.packages("ggplot2")
library(ggplot2)

tidy_books %>% 
  count(word,sort = TRUE) %>% 
  filter(n>600) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
