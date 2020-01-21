

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
# sav ���� �ε��ϱ�
raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
# ������������ ����
welfare <- raw_welfare
# �÷�������
welfare <- rename(welfare,
                  sex = h10_g3,         #����
                  birth = h10_g4,       #�¾ ����
                  marriage = h10_g10,   #ȥ�λ���
                  religion = h10_g11,   #����
                  income = p1002_8aq1,  #����
                  code_job = h10_eco9,  #�����ڵ�
                  code_region = h10_reg7)#�����ڵ� 
# ������ �����ϱ�
head(welfare,1)
tail(welfare)
dim(welfare)
str(welfare)
class(welfare$code_job) # Ư���÷��� ��������

# ���� ��ó��
welfare$sex <- ifelse(welfare$sex==9,NA,welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex==1,"male","female")
table(welfare$sex)

# ������ó��
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

# ����
summary(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth)
welfare$age <- 2019 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# ���̿� ���� ���� ���ǥ
age_income <- welfare %>% 
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income=mean(income))
head(age_income)
ggplot(data = age_income, aes(x=age, y=mean_income)) +
  geom_line()

# ���ɴ뿡 ���� ��������
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

# 9-5 ���ɴ� �� ���� ��������
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

# 9-6 ������ ��������
# �����ڵ� ��ó��
class(welfare$code_job) # �����ڵ尡 "numeric"���ڷ� �Ǿ�����
table(welfare$code_job)
list_job <- read_excel("Koweps_Codebook.xlsx",
                       col_names = T, sheet = 2) #������ ����
head(list_job)
dim(list_job)
welfare <- left_join(welfare, list_job, id="code_job")
## Joining, by ="code_job"
welfare %>% filter(!is.na(code_job)) %>%
  select(code_job,job) %>%
  head(10)
# ������ �������� �м�
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income=mean(income))
head(job_income)
#���� ������� ���� 10��
top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10
ggplot(data = top10, aes(x=reorder(job,mean_income),y=mean_income)) +
  geom_col() + coord_flip()

# �������� 10�� ����, ��� �� �ð�ȭ
bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)
bottom10  
ggplot(data = bottom10,aes(x=reorder(job,-mean_income),y=mean_income)) +
  geom_col() + coord_flip() + ylim(0,850)

# ������ �м��� �̹� ����10�� ��������� �װ��� �ð�ȭ �� ���� 
# reorder()�Ͽ� ������ ����������Ѵ�. �ֳ��ϸ� x���� �⺻ ������ 
# abc, 123, ������ ������ �����Ǳ� �����̴�.


# big2007���� ��������(DepTime)�� ���� DepTime �װ����̸� 10�� ����ϰ�
# �ð�ȭ�Ͻÿ�, y�� ����Ƚ��
#��ó��
air <- read.csv("2007.csv", header = T)
air <- air %>% 
  select(UniqueCarrier, DepTime)
carriers <- read.csv("carriers.csv", header = T)
class(air$UniqueCarrier) #facto
class(air$DepTime) #"integer"
carriers

#���� 
air <- left_join(air, carriers, id="UniqueCarrier")

#�м�
newAir2007 <- air %>% 
  filter(!is.na(DepTime) & DepTime>0) %>%
  group_by(Description) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(10)
newAir2007
# �ð�ȭ
ggplot(data = newAir2007, aes(x=reorder(Description, count),y=count)) +
  geom_col() + coord_flip()


# 9-7 ���� ���� ��
# ������ � ������ ������?
#Q1.�� ������ ������ �󵵸� ���� ���� 10���� ����
job_male <- welfare %>%
  filter(!is.na(job) & sex=="male") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male #��
job_female <- welfare %>%
  filter(!is.na(job) & sex=="female") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female #��

# 9-8 ������ ��ȥ��
# ���� : 1 ����, 2����, 9��
class(welfare$religion)
table(welfare$religion) #9����
welfare$religion <- ifelse(welfare$religion==1,"yes","no")
table(welfare$religion)
qplot(welfare$religion)

# ��ȥ : 0���ش�, 1�����, 2�纰, 3��ȥ, 4����, 5��ȥ, 6��Ÿ
# 1 marrige, 3 divorce, ������ NA ó��
welfare$group_marriage <- ifelse(welfare$marriage==1,"marrige",
                           ifelse(welfare$marriage==3,"divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

# ���� ��/���� ���� ��ȥ��
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n=n()) %>% # 2x2=4 ����n��
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
religion_marriage
# ��ȥ����
divorce <- religion_marriage %>%
  filter(group_marriage=="divorce") %>%
  select(religion,pct)
divorce
ggplot(data = divorce, aes(x=religion,y=pct)) +geom_col()

#���ɴ뺰 ��ȥ��
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
ageg_marriage
#�ʳ����� ��ȥ����
ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marriage=="divorce") %>%
  select(ageg,pct)
ggplot(data = ageg_divorce, aes(x=ageg,y=pct)) + geom_col()










