
# < 텍스트 마이닝> : 자연어 처리, 즉 문자로 된 데이터에서 
#                     가치있는 정보를 얻어 내는 것
# 형태소 분석 ==> 명상 동상 형용사의 의미를 지닌 단어가 얼만큼 
#                 등장하는지 파악하는 것
# 크롤링 : sns나 웹사이트에 올라온 데이터를 수집하는 것.
# 필요한 패키지 : rJava, memoise, KoNLP

# 패키지로드
library(KoNLP)
library(dplyr)
library(stringr) #특수문자 제거
useNIADic() # 한국어 사전 로드

# 텍스트 불러오기
txt <- readLines("hiphop.txt")
head(txt)

# 특수 문자제거하기(전처리 작업)
txt <- str_replace_all(txt,"\\W"," ")

# 가장 많이 사용된 단어 알아보기
#1.명사추출
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")
nouns <- extractNoun(txt)
table(unlist(nouns))
#1.스칼라scala - 크기
#2.벡터vector - 방향성
#3.매트릭스matrix - 양방향성
#4.텐서tensor - 입체형데이터

# nulist(nouns) 모든 원자 요소를 vector벡터화 시킨다.
#2.추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성 
wordcount <- table(unlist(nouns))
wordcount
#3.데이터 프레임으로 변환
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
#4.변수명 수정
df_word <- rename(df_word, word=Var1, freq=Freq)
head(df_word,10)
#5.자주 사용되는 단어 빈도표 만들기
# 두글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >=2 )
top_20 <- df_word %>% 
  arrange(desc(freq)) %>%
  head(20)
top_20


# 워드클라우드 만들기
# 워드클라우드 : 단어의 빈도를 구름모양으로 표현한 그래프
#               얼마나 많이 사용되었는지 한눈에 파악
# 패키지 : wordcloud, RcolorBrewer(글자색깔표현)
library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8, "Dark2") #8가지색
set.seed(1234)
wordcloud(words = df_word$word, # 단어
          freq = df_word$freq,  # 빈도
          min.freq = 2,         # 최소 단어 빈도
          max.words = 200,      # 표현 단어 수
          random.order = F,     # 고빈도 단어 중앙 배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4, 0.3),    # 단어 크기 범위
          colors = pal)         # 색상 목록


# 텍스트 마이닝
#1.데이터로드
twitter <- read.csv("twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
#2.변수명 수정
twitter <- rename(twitter,
                  no=번호,
                  id=계정이름,
                  date=작성일,
                  tw=내용)
#3.특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W"," ")
str(twitter)
head(twitter$tw)
#4.명사추출
nouns <- extractNoun(twitter$tw)
#5.추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
#6.데이터프레임으로 변환
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
#7.변수명 수정
df_word <- rename(df_word,
                  word=Var1,
                  freq=Freq)
#8.두 글자 이상 단어 추출
df_word <- filter(df_word, nchar(word)>=2)
#9.상위 20개 추출
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top20

# 단어 빈도 막대 그래프 만들기
library(ggplot2)
order <- arrange(top20,freq)$word #빈도순서 변수 생성

ggplot(data = top20, aes(x=word,y=freq)) +
  ylim(0,2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit=order) +
  geom_text(aes(label=freq), hjust= -0.3)

# 워드클라우드 만들기
#pal <- brewer.pal(8,"Dark2")
pal <- brewer.pal(9,"Blues")[5:9]
set.seed(123)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)
warnings()




















