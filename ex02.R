#�÷��� �ٲٱ� rename()
# dplyr ��Ű���� ��ġ�ؾ� ��� ������ 
df_raw <-data.frame(var1=c(1,2,1),
                    var2=c(2,3,2))
df_raw
#var1--> v1
#
install.packages("dplyr")
library(dplyr)  #�ε��ؾ� ��밡��
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

#�Ļ�����: ������ ������ ������ ���� ����
#

df<-data.frame(sname<-c("������","������","�ڵ���","�����"),
               eng=c(90,80,60,70),
               mat=c(50,60,100,20))
df
df$var_sum<-df$eng+df$mat
df
df$var_mean<-(df$eng+df$mat)/2
df

#mpg���տ��� ���
mpg$total<-(mpg$cty+mpg$hwy)/2
head(mpg)
mean(mpg$total)

#���ǹ� �Լ��� �̿��� �Ļ�����
summary(mpg$total) #1.���ذ� ���ϱ�
hist(mpg$total) #������׷�
mpg$test<-ifelse(mpg$total>=20,"pass","fail")#2.�հ��������ǹ�(ifelse)
head(mpg,20)
table(mpg$test)#������ ���� ����
factor(mpg$test) #:��� ������ �Ǿ��ִ��� ���
table(mpg$test) #:�� ������ � ��Ұ� �����ϴ� �� ���
qplot(mpg$test)#����׷��� ��, hist:����

#��ø���ǹ�
mpg$grade<-ifelse(mpg$toatal>=30,"A",
                  ifelse(mpg$total>=20,"B","C"))
head(mpg,20)                  
table(mpg$grade)
table()

#2008.csv�� ��� ������ �󵵱׷����� �׷����ÿ�.
#1 csv �� �α��Ѵ�(�÷����� ����)
big2008<-read.csv("2008.csv",header = F)
#2 �� �÷��� Ȯ���Ѵ�
head(big2008)
#3 �ش� �÷��� �󵵸� ����غ���.
factor(big2008$v23)
table(big2008$v23)
#4 �󵵱׷��� �׸���
qplot(big2008$V23)

#123
df_mid <- as.data.frame(ggplot2::midwest)
df_mid
dim(df_mid)
df_mid <- rename(df_mid,total=poptotal, asian=popasian)
df_mid$ap <- (df_mid$asian / df_mid$total)*100
hist(df_mid$ap) 
#�ؼ� : 0~5% �� 320�������� ������
#       5~10% �� 70�������� ������
#       10~15% �� 20�������� ������
df_mid$a_a <- mean(df_mid$ap)
df_mid$test <- ifelse(df_mid$ap>df_mid$a_a,"large","small")
table(df_mid$test)
qplot(df_mid$test)

# ��������� ������ �����ϱ�
#dplyr ��Ű���� ����
# �� ���� : sql�� where�� 
filter()
# �� ���� : sql�� select��
select()
# ���� : sql�� order by
arrange()
# �÷��߰� : sql�� add columns
mutate()
# ������
summarise()
# �׷�ȭ �Լ�
group_by()
# �� ������ ��ġ��
left_join()
# �� ������ ��ġ��
bind_rows()

# A  %>%  B  : A�� ����� B�� �����Ѵ�

#exam���� class�� 1�� ��츸 ������ ���
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

#Ư���� ������
# ���� ** ^ , %/% ��, %% ������, %in% ��Ī
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