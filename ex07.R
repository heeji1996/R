# 함수내의 특정 속성을 이용해서 결측지 제거 후 연사하기
# sum(data,na.rm=T) ,
# mean(data,na.rm=T) 에느 na.rm=T 속으로 추가하여 

exam <- read.csv("csv_exam.csv")
exam[c(3,8,15), "math"] <- NA #3,8,15행 math에 NA주기
exam
#결측치 제거하여 평균구하기
exam %>% summarise(mean_math=mean(math,na.rm = T))
exam %>% summarise(mean_math=mean(math,na.rm = T),
                   sum_math=sum(math,na.rm = T),
                   median_math=median(math,na.rm = T))

# 결측지 대체하기
# : 데이터가 작소 결측치가 많은 경우 제거하게 되면 많은 데이터가 손실 됨 

# 1.평균값으로 결측지 대체
# NA이면 55로 대체하기 
mean(exam$math, na.rm = T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math)) # 결측치 빈도표
exam

# 170
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212), "hwy"] <- NA
#Q1
table(is.na(mpg$dry)) #결측치:0
table(is.na(mpg$hwy)) #결측치:5
#Q2
mpg %>% filter(!is.na(hwy)) %>% #결측치 제거
  group_by(drv) %>%
  summarise(mean(hwy))

# 2006에서 Cancelled 취소,CancellationCode 취소 유형별사유 횟수를 구하시오
#1)인스턴스 생성
air <- read.csv("2006.csv", stringsAsFactors = F)
#2)각 컬럼에 NA가 있는 지확인
table(is.na(air$Cancelled))
table(is.na(air$CancellationCode))
#3)NA제거 후 횟수 구하기
air %>% select(Cancelled,CancellationCode) %>%
  filter(Cancelled==1) %>% #취소된것만
  group_by(CancellationCode) %>% 
  summarise(count=n())

# 이상치(outlier) 정제하기
# : 정상범주에서 크게 벗어난 값을 말한다
# 이상치의 존재는 분석결과를 크게 왜곡시킨다
# 이상치 ==> 결측치 ==> 결측치제거
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
outlier
table(outlier$sex)
table(outlier$score)
# sex,score 의 이상치를 결측치로 바꾸기
outlier$sex <- ifelse(outlier$sex==3, NA, outlier$sex)
outlier$score <- ifelse(outlier$score>5, NA, outlier$score)
# is.na(컬럼), is.omit(객체), 속성 is.rm = T
# 결측지 제거 한 성별에 따른 score구하기
outlier %>% na.omit() %>%
  group_by(sex) %>%
  summarise(avg_score=mean(score))
#
outlier %>% filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score=mean(score))


# 이상치제거하기 - 극단적인 값(극단치)
# : 논리적으로는 존재할 수 있으나 극던적으로 크거나 작은 값
library(ggplot2)
boxplot(mpg$hwy) # 통계적인 기준의 극단치 산출요령
# qplot 빈도, hist 구간빈도, boxplot극단치
qplot(mpg$hwy)
hist(mpg$hwy)
# 상자 그림 통계치 출력
boxplot(mpg$hwy)$stats
# [1,]   12 <== 보다 작으면 이상치로 판단
# [2,]   18
# [3,]   25
# [4,]   27 
# [5,]   37 <== 보다 크면 이상치로 판단
# 이상치를 제거한 후 고속도로 평균연비를 구하시오
mpg$hwy <- ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy) 
table(is.na(mpg$hwy)) # 결측치 : 8
mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm = T))

# drv별 도시별 평균연비
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty>26 | mpg$cty<9, NA, mpg$cty)
mpg %>% group_by(drv) %>%
  summarise(mean_cty=mean(cty,na.rm = T))

#178
mpg<-as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93), "drv"] <-"k" #drv 이상치 할당
mpg[c(229,43,129,203), "cty"] <- c(3,4,39,42) #cty 이상치 할당
#Q1.이상치 확인 후 처리 %in% 사용
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)
table(is.na(mpg$drv)) # 결측치 :4
#Q2. cty 이상치를 boxplot를 이용해서 이상치를 결측치로 전환
boxplot(mpg$cty)$stats
mpg$cty<- ifelse(mpg$cty<9 | mpg$cty>26, NA, mpg$cty)
table(is.na(mpg$cty)) # 결측치 :9#Q3. drv별 cty 평균을 구하시오. dplyr를 활용
mpg %>% filter(!is.na(cty)& !is.na(drv)) %>% 
  group_by(drv) %>%
  summarise(mean_cty=mean(cty))


















