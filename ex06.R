# 정렬하기
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam
exam %>% arrange(math) #올림차순

exam %>% arrange(desc(math)) #내림차순

exam %>% arrange(class,math) # 2중정렬렬

#141
mpg <- as.data.frame(ggplot2::mpg)
mpg
audi <- mpg %>% filter(manufacturer=="audi") %>% 
  arrange(desc(hwy))
head(audi,5)

# 2008.csv에서 5월 WN운항사의 운항거리를 많은 순서로 출력하되,
# 출발지와 목적지를 같이 표기하시오
air <- read.csv("2008.csv")
head(air)
wn <- air %>% filter(Month==5 & UniqueCarrier=="WN") %>%
  select(Month,UniqueCarrier,Origin,Dest,Distance) %>%
  arrange(desc(Distance)) %>% head(10)
wn

# 파생변수 추가하기 mutate()
# exam$total <- DataFrame에서는 이렇게 함
exam %>% mutate(total=math+english+science,
                mean=(math+english+science)/3) %>% head()
exam %>% mutate(total=math+english+science) %>% 
         mutate(mean=total/3) %>% head()

exam %>% mutate(test=ifelse(science>=60,"pass","fail")) %>%
  head()

exam %>% mutate(total=math+english+science) %>%
  arrange(total) %>% head()

#144
mpgs <- mpg %>% mutate(total=hwy+cty) 
mpgs <- mpgs %>% mutate(avg=total/2)
mpgs %>% arrange(desc(avg)) %>% head(3)
mpg %>% mutate(total=hwy+cty) %>%
      mutate(avg=total/2) %>%
      arrange(desc(avg)) %>% head(3)
#기본적으로 사용하면
mpgCopy <- as.data.frame(ggplot2::mpg)
mpgCopy$tot <- mpgCopy$hwy+mpgCopy$cty
mpgCopy$avg <- mpgCopy$tot/2
#mpgCopy$avg <- sort(mpgCopy$avg,decreasing = T)
mpgCopy <- mpgCopy[c(order(-mpgCopy$avg)),]
head(mpgCopy,3)

# filter()행, select()열, arrange()정렬

# summary : 최대 최소 중앙 1/4쿼터 3/4쿼터 : base 소속
summarise() #dplyr 소속

# 설명 : summarise 요약 결과를 새로운 컬럼으로 추가 해준다.
exam %>% summarise(mean_math=mean(math))

# 반 별 수학의 평균점수 출력
exam %>% group_by(class) %>%
  summarise(mean_math=mean(math))

# filter, select, arrange, mutate, group_by, summerise

# 반 별 수학의 여러통계량 출력
exam %>% group_by(class) %>%
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n=n())

# 회사별, 구동방식 별 cty 평균 10개산출
mpg %>% group_by(manufacturer,drv) %>%
  summarise(mean_cty=mean(cty)) %>%
  head(10)

mpg <- as.data.frame(ggplot2::mpg)
# 복수 집단  
mpg %>% group_by(manufacturer) %>%
  filter(class=="suv") %>%
  mutate(tot=(cty+hwy)/2) %>%
  summarise(mean_tot=mean(tot)) %>%
  arrange(desc(mean_tot)) %>%
  head(5)
# 결론 group_by에서 집계를 사용하려면 summarisze 함수를 사용해야한다

mpg_Copy <- as.data.frame(ggplot2::mpg) 
#150
mpg_Copy %>% group_by(class) %>% 
  summarise(mean_cty=mean(cty)) %>%
  arrange(desc(mean_cty))
mpg_Copy %>% group_by(manufacturer) %>%
  summarise(mean_hwy=mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)
mpg_Copy %>% filter(class=="compact") %>%
  group_by(manufacturer) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
  
# 2008에서 월별 항공사별 출항 횟수 많은 순서대로 100개출력
air <- read.csv("2008.csv")
air <- air %>% filter(DepTime != "NA" & DepTime <=2004 & Distance > 0) 
res<-air %>% select(Month, UniqueCarrier, DepTime) %>%
  group_by(Month,UniqueCarrier) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(100)
res
factor(air$DepTime)

# 데이터 합치기
# 가로로 합치기
test1 <- data.frame(id=c(1,2,3,4,5),
                    midterm=c(60,80,70,90,85))
test2 <- data.frame(id=c(1,2,3,4,5),
                    final=c(70,83,65,95,80))
total <- left_join(test1,test2,by="id")
total

name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c("kim","lee","park","choi","jung"))
name
exam_new <- left_join(exam,name, by="class")
exam_new

# 세로로 합치기
group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,80,70,90,85))
group_b <- data.frame(id = c(6,7,8,9,10),
                      test =c(70,83,65,95,80))
group_all <- bind_rows(group_a,group_b)
group_all

#156
fuel <- data.frame(fl=c("c","d","e","p","r"),
                   price_fl=c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = F)
fuel
mpg<- left_join(mpg,fuel,by="fl")
mpg %>% select(model,fl,price_fl) %>%
  head(5)

#160
#1
mw<-as.data.frame(ggplot2::midwest)
mw<-mw %>% mutate(mw_k=100*(poptotal-popadults)/poptotal)
mw
#2
mw %>% arrange(desc(mw_k)) %>%
  head(10)
#3
mm <- data.frame(b=c("large","middle","small"),
                 g=c("40%","60~40", "30미만"))
mw <- mw %>% mutate(b=ifelse(mw_k>=40, "large", 
                    ifelse(mw_k<30, "small", "middle")))
head(mw,10)
#4
mw %>% mutate(asian=100*popasian/poptotal) %>%
  select(state,county,asian) %>%
  arrange(asian) %>%
  head(10)
  