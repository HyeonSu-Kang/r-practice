#빈도가 높은 단어 탐색 및 시각화-흔한 단어 확인하기

install.packages("tidytext")
install.packages("tidyr")
install.packages("dplyr")
library(tidytext)
library(tidyr)
library(dplyr)


# H.G.웰스 작품 불러오기
#- [35]The Time Machine (타임머신)
#- [36]The War of the Worlds (세계 전쟁)
#- [5230]The Invisible Man (인비지블 맨)
#- [159]The Island of Doctor Moreau (닥터 모로의 섬)

install.packages("gutenbergr")
library(gutenbergr)
hgwells<-gutenberg_download(c(35,36,5230,159))
hgwells

# 브론테 자매 작품 불러오기
#- [1260] Jane Eyre (제인 에어)
#- [768] Wuthering Heights (폭풍의 언덕)
#- [969] The Tenant of Wildfell Hall (와일드펠 홀의 소작인)
#- [9182] Villette (빌레트)
#- [767] Agnes Grey (아그네스 그레이)

bronte<-gutenberg_download(c(1260,768,969,9182,767))

# H.G.웰스 작품 확인하기

tidy_hgwells<-hgwells %>% 
  unnest_tokens(input = text, output = word) %>% 
  anti_join(stop_words)
tidy_hgwells %>% 
  count(word,sort = TRUE)

# 브론테 자매 작품 확인하기

tidy_bronte<-bronte %>% 
  unnest_tokens(input=text,output=word) %>% 
  anti_join(stop_words)
tidy_bronte

#시각화화를 통해 작품간 단어 비교하기

install.packages("stringr")
library(stringr)
library(tidyr)
frequency<-bind_rows(mutate(tidy_bronte,author="Bronte Sisters"),
                     mutate(tidy_hgwells,author="H.G. Wells"),
                     mutate(tidy_books,author="Jane Austen")) %>% 
  mutate(word=str_extract(word,"[a-z]+")) %>% 
  count(author,word) %>% 
  group_by(author) %>% 
  mutate(proportion=n/sum(n)) %>% 
  select(-n) %>% 
  spread(author,proportion) %>% 
  gather(author,proportion,"Bronte Sisters", "H.G. Wells")
frequency

install.packages("ggplot2")
install.packages("scales")
library(scales)
library(ggplot2)
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion)))+
  geom_abline(color="gray40",lty=2)+
  geom_jitter(alpha=.1,size=2.5,width = .3,height = .3)+
  geom_text(aes(label=word),check_overlap=TRUE,vjust=1.5)+
  scale_x_log10(labels=percent_format())+
  scale_y_log10(labels=percent_format())+
  scale_color_gradient(limits=c(0,0.001),
                       low="darkslategray4",high = "gray75")+
  facet_wrap(~author,ncol=2)+
  theme(legend.position = "none")+
  labs(y="Jane Austen",x=NULL)
