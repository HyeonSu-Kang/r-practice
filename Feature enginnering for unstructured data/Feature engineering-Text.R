#Feature enginnering for unstructured data
#정돈데이터 구조로 변환하기

Emily_text<-c("Because I could not stop for Death-",
              "He kindly stopped for me -",
              "The Carriage held but just ourselves -",
              "and Immortality")
Emily_text
#전형적인 문자 벡터

install.packages("dplyr")
library(dplyr)
text_df<-tibble(line=1:4,text=Emily_text)
text_df

install.packages("tidytext")
library(tidytext)
text_df #column:lint, text
text_df %>% 
  unnest_tokens(word,text,to_lower = TRUE)
#text를 입력, word를 출력

