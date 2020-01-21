#컬러명 바꾸기 rename()
# dplyr 패키지를 설치해야 사용 가능함 
df_raw <-data.frame(var1=c(1,2,1),
                    var2=c(2,3,2))
df_raw
#var1--> v1
#
install.packages("dplyr")
library(dplyr)  #로드해야 사용가능
df_raw<-rename(df_raw,v2=var2)
df_raw
df_raw<-rename(df_raw,v1=var1)
df_raw
#112
mpg<-as.data.frame(ggplot2::mpg)
mpg
mpg<-rename(mpg,city=cty)
mpg<-rename(mpg,highway=hwy)
head(mpg)

#파생변수: 기존의 변수를 변형해 만든 변수
#

df<-data.frame(sname<-c("김지훈","이유진","박동현","김민지"),
               eng=c(90,80,60,70),
               mat=c(50,60,100,20))
df
df$var_sum<-df$eng+df$mat
df
df$var_mean<-(df$eng+df$mat)/2
df

#mpg통합연비 평균
mpg$total<-(mpg$cty+mpg$hwy)/2
head(mpg)
mean(mpg$total)

#조건문 함수를 이용한 파생변수
summary(mpg$total) #1.기준값 정하기
hist(mpg$total) #히스토그램
mpg$test<-ifelse(mpg$total>=20,"pass","fail")#2.합격판정조건문(ifelse)
head(mpg,20)
table(mpg$test)#데이터 빈도의 개수
factor(mpg$test) #:몇가지 레벨로 되어있는지 출력
table(mpg$test) #:그 레벨당 몇개 요소가 존재하는 지 출력
qplot(mpg$test)#막대그래프 빈도, hist:구간

#중첩조건문
mpg$grade<-ifelse(mpg$toatal>=30,"A",
                  ifelse(mpg$total>=20,"B","C"))
head(mpg,20)                  
table(mpg$grade)
table()

#2008.csv에 취소 원인의 빈도그래프를 그려보시오.
#1 csv 를 로그한다(컬럼명이 없다)
big2008<-read.csv("2008.csv",header = F)
#2 각 컬럼을 확인한다
head(big2008)
#3 해당 컬럼의 빈도를 출력해본다.
factor(big2008$v23)
table(big2008$v23)
#4 빈도그래프 그린다
qplot(big2008$V23)

#123
df_mid <- as.data.frame(ggplot2::midwest)
df_mid
dim(df_mid)
df_mid <- rename(df_mid,total=poptotal, asian=popasian)
df_mid$ap <- (df_mid$asian / df_mid$total)*100
hist(df_mid$ap) 
#해석 : 0~5% 약 320지역정도 분포함
#       5~10% 약 70지역정도 분포함
#       10~15% 약 20지역정도 분포함
df_mid$a_a <- mean(df_mid$ap)
df_mid$test <- ifelse(df_mid$ap>df_mid$a_a,"large","small")
table(df_mid$test)
qplot(df_mid$test)

# 자유자재로 데이터 가공하기
#dplyr 패키지에 포함
# 행 추출 : sql에 where절 
filter()
# 열 추출 : sql에 select절
select()
# 정렬 : sql에 order by
arrange()
# 컬럼추가 : sql에 add columns
mutate()
# 통계산출
summarise()
# 그룹화 함수
group_by()
# 열 데이터 합치기
left_join()
# 행 데이터 합치기
bind_rows()

# A  %>%  B  : A의 결과를 B로 전달한다

#exam에서 class가 1인 경우만 추출해 출력
exam %>% filter(class==1)

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam
exam %>% filter(class==1)
exam %>% filter(class==2)
exam %>% filter(class!=1)
exam %>% filter(class!=1)
exam %>% filter(math>50)
exam %>% filter(math<50)
exam %>% filter(english>=80)
exam %>% filter(english<=80)
exam %>% filter(class==1 & math>=50)
exam %>% filter(class==2 & english >=80)
exam %>% filter(math>=90 | english>=90)

#특이한 연산자
# 제곱 ** ^ , %/% 몫, %% 나머지, %in% 매칭
mpg_4<- mpg %>% filter(displ<=4)
mpg_5<- mpg %>% filter(displ>=5)
mean(mpg_4$hwy)
mean(mpg_5$hwy)

mpg_audi<- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)

mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(mpg_n$hwy)
