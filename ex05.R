#�ʿ��� ����(�÷�,�Ӽ�...)�� �����ϱ�

exam<-read.csv("csv_exam.csv")
exam

library(dplyr) #���� ��Ű���ε� �Ѵ�.
exam %>% select(math)

# class�� 1�� class,math,english ���
exam %>% select(class,math,english) %>% filter(class==1)

# Ư���÷� ���� '-�÷�' ǥ���ϱ� 
exam %>% select(-math)

# �����÷��� �����÷��� �����ϰ� ���
exam %>% select(-math,-english)

# 1���� �������� ���
exam %>% filter(class==1) %>% select(english)

# ������ ? �ٹٲ㾲��
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

# 2008.csv���� �װ����ڵ尡 wnȸ���� 7��(month)��� ���װŸ�(distance)�� ����Ͻÿ�
airline <- read.csv("2008.csv")
head(airline)
wn <- airline %>% filter(UniqueCarrier=="WN" & Month==7) %>% 
  select(UniqueCarrier,Month,Distance)
wn <- wn %>% filter(Distance!="NA")
wn
mean(wn$Distance) #[1] 641.3151
head(mpg,1)