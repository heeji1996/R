

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
# sav 파일 로드하기
raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
# 데이터프레임 복제
welfare <- raw_welfare
# 컬럼명변경
welfare <- rename(welfare,
                  sex = h10_g3,         #성별
                  birth = h10_g4,       #태어난 연도
                  marriage = h10_g10,   #혼인상태
                  religion = h10_g11,   #종교
                  income = p1002_8aq1,  #월급
                  code_job = h10_eco9,  #직업코드
                  code_region = h10_reg7)#지역코드 
# 데이터 검토하기
head(welfare,1)
tail(welfare)
dim(welfare)
str(welfare)
class(welfare$code_job) # 특정컬럼의 데이터형

# 성별 전처리
welfare$sex <- ifelse(welfare$sex==9,NA,welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex==1,"male","female")
table(welfare$sex)

# 수입전처리
summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income=mean(income))
sex_income
ggplot(data = sex_income, aes(x=sex, y=mean_income)) +
  geom_col()

# 나이
summary(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth)
welfare$age <- 2019 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# 나이에 따른 월급 평균표
age_income <- welfare %>% 
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income=mean(income))
head(age_income)
ggplot(data = age_income, aes(x=age, y=mean_income)) +
  geom_line()

# 연령대에 따른 월급차이
welfare <- welfare %>%
  mutate(ageg = ifelse(age<30,"young",
                       ifelse(age<=59,"middle", "old")))
table(welfare$ageg)
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income=mean(income))
ageg_income
ggplot(data = ageg_income, aes(x=ageg, y=mean_income)) + 
  geom_col() +
  scale_x_discrete(limits = c("young","middle","old"))

# 9-5 연령대 및 성별 월급차이
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young","middle","old"))
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
   group_by(age, sex) %>%
  summarise(mean_income=mean(income))
head(sex_age)
ggplot(data = sex_age, aes(x=age, y=mean_income, col=sex)) +
  geom_line()

# 9-6 직업별 월급차이
# 직업코드 전처리
class(welfare$code_job) # 직종코드가 "numeric"숫자로 되어있음
table(welfare$code_job)
list_job <- read_excel("Koweps_Codebook.xlsx",
                       col_names = T, sheet = 2) #조인할 문서
head(list_job)
dim(list_job)
welfare <- left_join(welfare, list_job, id="code_job")
## Joining, by ="code_job"
welfare %>% filter(!is.na(code_job)) %>%
  select(code_job,job) %>%
  head(10)
# 직업별 월급차이 분석
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income=mean(income))
head(job_income)
#직업 월급평균 상위 10개
top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10
ggplot(data = top10, aes(x=reorder(job,mean_income),y=mean_income)) +
  geom_col() + coord_flip()

# 월급하위 10개 직종, 출력 후 시각화
bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)
bottom10  
ggplot(data = bottom10,aes(x=reorder(job,-mean_income),y=mean_income)) +
  geom_col() + coord_flip() + ylim(0,850)

# 데이터 분석이 이미 상위10을 취득했지만 그것을 시각화 할 때도 
# reorder()하여 순서를 지정해줘야한다. 왜냐하면 x축의 기본 순서는 
# abc, 123, 가나다 순으로 나열되기 때문이다.


# big2007에서 출항지연(DepTime)이 많은 DepTime 항공사이름 10를 출력하고
# 시각화하시오, y축 지연횟수
#전처리
air <- read.csv("2007.csv", header = T)
air <- air %>% 
  select(UniqueCarrier, DepTime)
carriers <- read.csv("carriers.csv", header = T)
class(air$UniqueCarrier) #facto
class(air$DepTime) #"integer"
carriers

#조인 
air <- left_join(air, carriers, id="UniqueCarrier")

#분석
newAir2007 <- air %>% 
  filter(!is.na(DepTime) & DepTime>0) %>%
  group_by(Description) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(10)
newAir2007
# 시각화
ggplot(data = newAir2007, aes(x=reorder(Description, count),y=count)) +
  geom_col() + coord_flip()


# 9-7 성별 직업 빈도
# 성별로 어떤 직업이 많은가?
#Q1.각 성별로 직업별 빈도를 구해 상위 10개를 추출
job_male <- welfare %>%
  filter(!is.na(job) & sex=="male") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male #남
job_female <- welfare %>%
  filter(!is.na(job) & sex=="female") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female #여

# 9-8 종교와 이혼률
# 종교 : 1 있음, 2없음, 9모름
class(welfare$religion)
table(welfare$religion) #9없음
welfare$religion <- ifelse(welfare$religion==1,"yes","no")
table(welfare$religion)
qplot(welfare$religion)

# 결혼 : 0비해당, 1유배우, 2사별, 3이혼, 4별거, 5미혼, 6기타
# 1 marrige, 3 divorce, 나머지 NA 처리
welfare$group_marriage <- ifelse(welfare$marriage==1,"marrige",
                           ifelse(welfare$marriage==3,"divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

# 종교 유/무에 따른 이혼률
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n=n()) %>% # 2x2=4 종류n값
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
religion_marriage
# 이혼추출
divorce <- religion_marriage %>%
  filter(group_marriage=="divorce") %>%
  select(religion,pct)
divorce
ggplot(data = divorce, aes(x=religion,y=pct)) +geom_col()

#연령대별 이혼율
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
ageg_marriage
#초년제외 이혼추출
ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marriage=="divorce") %>%
  select(ageg,pct)
ggplot(data = ageg_divorce, aes(x=ageg,y=pct)) + geom_col()











