library(dplyr)
is.na() #na이가 있는지 검사 /결측치
table() #빈도측정

filter()
select() 
group_by() #그룹 집계
summarise() #요약통계
mutate() #컬럼추가
boxplot() #이상치
boxplot()$stats #극단치
# 극단치 처리: 극단치  --> NA --> 결측치 제거

#181
# 전문적인 그래프 그리기
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
midwest <- as.data.frame(ggplot2::midwest)
# 산점도 scater Plot : 연속된 두 값의 관계를 표현할 때 
                      # 관계 ? 정비례 반비례....
# 그래프 그리는 문법
# 1단계 : 데이터,축설정 - x축, y축 : ggplot(data=, aes(x=,y=))
# 2단계 : 그래프종류 - 점 막대 선 등 : + geom_point()
# 3단계 : 세부설정 - 축범위, 색, 표식 등 : + xlim() + ylim()
ggplot(data = mpg, aes(x=displ, y=hwy)) + 
  geom_point()  +
  xlim(3,6) + ylim(10,30)
#188
#Q1
ggplot(data = mpg, aes(x=cty,y=hwy)) +
  geom_point()

#Q2
ggplot(data = midwest,aes(x=poptotal, y=popasian)) +
  geom_point() +
  xlim(10000,500000) + ylim(1000,10000)

# 막대 그래프geom_col() - 집단간의 차이를 표현 
# 예) 자동차 종류별 갯수
# 구동방식별 평균 고속도로 연비
# (그룹함수를 그래프화 할 때)
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy=mean(hwy))
df_mpg
ggplot(data = df_mpg,aes(x=drv, y=mean_hwy)) +geom_col()
# 크기순으로 정렬하기 reoder() -기호를 붙이면 내림차순으로 정렬
ggplot(data=df_mpg,aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) +
  geom_col()

# geom_bar() : 축 한개만 존재 x축의 빈도그래프 그릴 때
              # x축이 연속일 때 값의 분포를 나타내는 빈도로 사용
ggplot(data = mpg, aes(x=drv)) + geom_bar()
ggplot(data = mpg, aes(x=hwy)) + geom_bar()

#193
#Q1. 회사별 suv의 cty평균 높은 순으로 막대 그래프프
mpg_suv <- mpg %>%
  filter(class=="suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty=mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
mpg_suv
ggplot(data = mpg_suv, aes(x=reorder(x=manufacturer, -mean_cty), y=mean_cty))+
  geom_col()
#Q2. classd의 빈도
ggplot(data = mpg, aes(x=class)) + geom_bar()

# 선형그래프 geom_line : 시계열 데이터를 표현
# 시계열 데이터란? 일정한 시간 간격을 두고 나열된 데이터를 말한다.
                # ex) 환율, 주가, 
economics <-as.data.frame(ggplot2::economics)
dim(economics)
str(economics)
head(economics,6)
ggplot(data = economics, aes(x=date, y=unemploy)) + 
  geom_line()
#Q1. 시간에 따른 psavert(개인저축률) 변화
ggplot(data = economics, aes(x=date, y=psavert)) +
  geom_line()
#Q2. 실업자와 저축율의 관계
ggplot(data = economics, aes(x=psavert, y=unemploy)) +
  geom_point()

# 상자그림그래프 - 집단 간 분포 차이 표현
#자동차 구동방식별 고속도로 연비 분포그래프
ggplot(data = mpg, aes(x=drv, y=hwy)) + 
  geom_boxplot()

# 비교) 집단간의 차이 -막대그래프
#자동차 구동방식별과 고속도로 연비 그래프
ggplot(data = mpg, aes(x=drv, y=hwy)) +
  geom_col()

#198
#Q1
mpg_class <- mpg %>%
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data = mpg_class, aes(x=class, y=cty)) +
  geom_boxplot()

# 실전데이터 분석
# 한국복지패널데이터 분석
install.packages("foreign")
library(foreign) # SPSS파일 불러오기
library(dplyr)   # 전처리
library(ggplot2) # 시각화  
library(readxl)  # 엑셀 파일 불러오기

# 데이터불러오기
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
#복사본 만들기
welfare <- raw_welfare

# 데이터 검토하기
head(welfare,1)
tail(welfare)
dim(welfare)
str(welfare)

# 분석을 용이하게 하기위해 컬럼명 변경
welfare <- rename(welfare,
                  sex = h10_g3,         #성별
                  birth = h10_g4,       #태어난 연도
                  marriage = h10_g10,   #혼인상태
                  religion = h10_g11,   #종교
                  income = p1002_8aq1,  #월급
                  code_job = h10_eco9,  #직업코드
                  code_region = h10_reg7)#지역코드 
# 데이터 전처리
table(welfare$sex)
table(is.na(welfare$income))
table(is.na(welfare$birth))





