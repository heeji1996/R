#필요한 변수(컬럼,속성...)만 추출하기

exam<-read.csv("csv_exam.csv")
exam

library(dplyr) #먼저 패키지로드 한다.
exam %>% select(math)

# class가 1인 class,math,english 출력
exam %>% select(class,math,english) %>% filter(class==1)

# 특정컬럼 제외 '-컬럼' 표시하기 
exam %>% select(-math)

# 수학컬럼과 영어컬럼을 제외하고 출력
exam %>% select(-math,-english)

# 1반의 영어점수 출력
exam %>% filter(class==1) %>% select(english)

# 가독성 ? 줄바꿔쓰기
exam %>% 
  filter(class==1) %>% 
  select(english)

exam %>% 
  select(id,math) %>%
  head

# 138
mpg <-as.data.frame(ggplot2::mpg)
mpg
class_cty <- mpg %>% select(class,cty)
class_cty
suv <- class_cty %>% filter(class=="suv") 
compact <- class_cty %>% filter(class=="compact") 
mean(suv$cty)
mean(compact$cty)

# 2008.csv에서 항공사코드가 wn회사의 7월(month)평균 운항거리(distance)를 출력하시오
airline <- read.csv("2008.csv")
head(airline)
wn <- airline %>% filter(UniqueCarrier=="WN" & Month==7) %>% 
  select(UniqueCarrier,Month,Distance)
wn <- wn %>% filter(Distance!="NA")
wn
mean(wn$Distance) #[1] 641.3151
head(mpg,1)
