# �����ϱ�
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam
exam %>% arrange(math) #�ø�����

exam %>% arrange(desc(math)) #��������

exam %>% arrange(class,math) # 2�����ķ�

#141
mpg <- as.data.frame(ggplot2::mpg)
mpg
audi <- mpg %>% filter(manufacturer=="audi") %>% 
  arrange(desc(hwy))
head(audi,5)

# 2008.csv���� 5�� WN���׻��� ���װŸ��� ���� ������ ����ϵ�,
# ������� �������� ���� ǥ���Ͻÿ�
air <- read.csv("2008.csv")
head(air)
wn <- air %>% filter(Month==5 & UniqueCarrier=="WN") %>%
  select(Month,UniqueCarrier,Origin,Dest,Distance) %>%
  arrange(desc(Distance)) %>% head(10)
wn

# �Ļ����� �߰��ϱ� mutate()
# exam$total <- DataFrame������ �̷��� ��
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
#�⺻������ ����ϸ�
mpgCopy <- as.data.frame(ggplot2::mpg)
mpgCopy$tot <- mpgCopy$hwy+mpgCopy$cty
mpgCopy$avg <- mpgCopy$tot/2
#mpgCopy$avg <- sort(mpgCopy$avg,decreasing = T)
mpgCopy <- mpgCopy[c(order(-mpgCopy$avg)),]
head(mpgCopy,3)

# filter()��, select()��, arrange()����

# summary : �ִ� �ּ� �߾� 1/4���� 3/4���� : base �Ҽ�
summarise() #dplyr �Ҽ�

# ���� : summarise ��� ����� ���ο� �÷����� �߰� ���ش�.
exam %>% summarise(mean_math=mean(math))

# �� �� ������ ������� ���
exam %>% group_by(class) %>%
  summarise(mean_math=mean(math))

# filter, select, arrange, mutate, group_by, summerise

# �� �� ������ ������跮 ���
exam %>% group_by(class) %>%
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n=n())

# ȸ�纰, ������� �� cty ��� 10������
mpg %>% group_by(manufacturer,drv) %>%
  summarise(mean_cty=mean(cty)) %>%
  head(10)

mpg <- as.data.frame(ggplot2::mpg)
# ���� ����  
mpg %>% group_by(manufacturer) %>%
  filter(class=="suv") %>%
  mutate(tot=(cty+hwy)/2) %>%
  summarise(mean_tot=mean(tot)) %>%
  arrange(desc(mean_tot)) %>%
  head(5)
# ��� group_by���� ���踦 ����Ϸ��� summarisze �Լ��� ����ؾ��Ѵ�

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
  
# 2008���� ���� �װ��纰 ���� Ƚ�� ���� ������� 100�����
air <- read.csv("2008.csv")
air <- air %>% filter(DepTime != "NA" & DepTime <=2004 & Distance > 0) 
res<-air %>% select(Month, UniqueCarrier, DepTime) %>%
  group_by(Month,UniqueCarrier) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(100)
res
factor(air$DepTime)

# ������ ��ġ��
# ���η� ��ġ��
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

# ���η� ��ġ��
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
                 g=c("40%","60~40", "30�̸�"))
mw <- mw %>% mutate(b=ifelse(mw_k>=40, "large", 
                    ifelse(mw_k<30, "small", "middle")))
head(mw,10)
#4
mw %>% mutate(asian=100*popasian/poptotal) %>%
  select(state,county,asian) %>%
  arrange(asian) %>%
  head(10)
  